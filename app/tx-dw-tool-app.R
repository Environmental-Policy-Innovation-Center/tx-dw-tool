# Interactive Dashboard for Texas Drinking Water Data
# Worklog Here: https://docs.google.com/document/d/1sLpBgD32_SmQjgAgVCe_KZUwDSNOTNz3f5kES09f4AM/edit
# Created for Environmental Policy Innovation Center
# Created by Gabriel Watson on 04.04.2024

library(shiny)
library(sf)
library(leaflet)
library(reactable)
library(aws.s3)
library(geojsonsf)
library(leaflet.extras)
library(htmltools)
library(dplyr)
library(bivariatechoropleths)
library(ggplot2)
library(plotly)
library(shinybusy)
library(stringr)
library(scales)
library(shinyalert)
library(reactable)
library(shinyjs)
library(shinycssloaders)
library(purrr)
library(shinyBS)
library(rlang)
library(reactablefmtr)
library(googlesheets4)
library(reactable.extras)
library(tippy)
library(viridis)
library(promises)
library(future)
library(tinytex)
library(googledrive)


## TICKET LIST

###########
### UI #### 
###########

#######

ui <- fluidPage(
  
  ## Global CSS alterations 
  tags$head(
    tags$style(
      ## Spacing on selectize control and rounding corners on plotly chart 
      HTML("
      .selectize-control {
        margin-top: -20px; 
        margin-bottom: -20px;
        margin-left: -2px;
        margin-top: -10px; 
      }
      .main-svg {
        border-radius: 5px; 
      }
      
       .info-button {
      position: absolute;
      top: 5px;
      right: 20px;
      background-color: transparent;
      border: none;
      color: black;
       }
    
    }
    ")
    )
  ),
  
  reactable_extras_dependency(),
  add_busy_spinner(spin = "fading-circle", position = "top-left"),
  useShinyjs(),
  
  sidebarLayout(
    div(
      id ="sidebar",
      sidebarPanel(
        style = "position: fixed; height: 100%; width: 420px; overflow-y: auto; margin-left: -30px;", 
        width = 3,
        actionButton("showInfo", "", icon(name = "circle-question", lib = "font-awesome", style = "font-size: 17px"), class = "info-button"), 
        uiOutput("SelectGeography", style = "width: 95%"), 
        bsCollapsePanel(
          div(
            style = "display: inline-block; position: relative;",
            div(
              style = "margin-left: -10px; margin-bottom: -18px",
              tipify(el = icon(name = "filter", lib = "font-awesome"), 
                     placement = "right", title = HTML("Filter the utilities selected by these categories. Some filters might not effect your data due to insufficent count."))
            ),
            ## we need this lil HTML to space out the text to the right
            HTML("&nbsp;"),
            "Filter by Categories"
          ),
          uiOutput("SelectCat", style = "line-height: 20px; margin-top: -10px; margin-bottom: -10px;"),  
          style = "primary"
        ),
        uiOutput("SummaryStats", style = "margin-bottom: 10px; margin-top: -10px;"),
        uiOutput("VarOne", style = "width: 90%"), 
        uiOutput("VarOneMinMax"),
        plotlyOutput("VarOneHist", width = "350px", height = "125px"),
        uiOutput("VarTwo", style = "width: 90%; margin-top: 10px"), 
        uiOutput("VarTwoMinMax"),
        plotlyOutput("VarTwoHist", width = "350px", height = "125px"),
        actionButton("Context", "Table or Map",icon(name = "arrows-left-right", lib = "font-awesome"),
                     ## ET added margin-top v 
                     style = "margin-bottom: 10px;; margin-top: 10px;"), 
        #   actionButton("hideSidebar", "Hide sidebar"),
        ### ET Added V
        downloadButton("Report", "Generate report",
                       icon = icon("file-arrow-down", lib = "font-awesome")),
        div(style = "display:inline-block",
            downloadButton("downloadData", "Download data"),
            div(style = "margin-top:-15px",
                radioButtons("downloadType", "", 
                             choices = c(".csv" = ".csv",
                                         ".geojson" = ".geojson"),
                             inline = TRUE)
                
            ),
        ))
    ),
    mainPanel(
      style = "margin-left: -15px;",
      leafletOutput("Map", height = "100vh"),
      hidden(
        uiOutput("TableText",
                 style = "font-size: 15px; margin-left: 5px; position:relative; z-index: 500; font-style: italic; margin-top: 5px;"),
        uiOutput("Table", style = "margin-left: 5px; background-color: none;", width = "100px")),
      width = 9
    ),
    position = c("right"), 
    fluid = TRUE
  )
)



################
#### SERVER #### 
################
server <- function(input, output, session) {
  
  #################### 
  ### Data Import #### 
  #################### 
  ## TO DO: Move this to the .RMD and return 1 object with 2 dataframes 
  
  tx_raw <- aws.s3::s3read_using(st_read, 
                                 object = "state-drinking-water/TX/clean/app/app_test_data_simplified_v2.geojson",
                                 bucket = "tech-team-data", 
                                 quiet = TRUE)
  
  tx_counties <- aws.s3::s3read_using(st_read, 
                                      object = "state-drinking-water/TX/clean/app/tx_counties_simplified_v2.geojson",
                                      bucket = "tech-team-data",
                                      quiet = TRUE)
  
  tx_regions <- aws.s3::s3read_using(st_read, 
                                     object = "state-drinking-water/TX/clean/app/tx_regions_simplified.geojson",
                                     bucket = "tech-team-data",
                                     quiet = TRUE)
  
  tx_sab_super_simplified <- aws.s3::s3read_using(st_read, 
                                                  object = "state-drinking-water/TX/clean/app/tx_sab_super_simplified.geojson",
                                                  bucket = "tech-team-data",
                                                  quiet = TRUE)
  
  suppressMessages({data_dict <- aws.s3::s3read_using(read.csv, 
                                                      object = "state-drinking-water/TX/clean/app/data_dict_v2.csv",
                                                      bucket = "tech-team-data")})
  
  suppressWarnings({report <- aws.s3::s3read_using(readLines,object = "state-drinking-water/TX/clean/app/tx-report.Rmd",
                                                   bucket = "tech-team-data")})
  ################
  ### Variables ##
  ################
  ## Data Dictionary 
  data_dict <- data_dict %>%
    filter(var_name %in% colnames(tx_raw))
  
  #Main data 
  Controller <- reactiveValues()
  Controller$data <- tx_raw 
  
  # reactive value to hold promise:
  data_to_plot <- reactiveVal(NULL)
  
  # List of counties
  Counties <- unique(tx_raw$county_served)
  
  ## Generating unique list of regionsw
  pwsid_regions <- tx_raw %>%
    data.frame()%>%
    select(regions)%>%
    unique()
  
  unique_regions <- unique(unlist(strsplit(pwsid_regions$regions, ",\\s*")))
  unique_regions <- unique_regions[!is.na(unique_regions)]
  
  ## poltly chart events
  event_one <- reactive(event_data(event = "plotly_selected", source = "a", priority = "event"))
  event_two <- reactive(event_data(event = "plotly_selected", source = "b", priority = "event"))

  
  ## Building categorical filters here so they can be accessed in pull down and logic handler 
  ## TO DO: Set these columns to by a part of the Controller - potentially as an s3 or store the app data as a list - with this as an additional dataframe
  ## TO DO: Set this to the controller! 
  checkboxSelection <- reactiveValues()
  
  cat_dict <- data_dict %>%
    filter(cat_var == "yes")
  
  cat_choices <- str_split(cat_dict$var_name, cat_dict$clean_name)
  
  
  cat_columns <- cat_dict$var_name
  cat_labels <- cat_dict$clean_name
  
  
  # ooh this took a while with some help with gpt but its nice and clean. 
  cont_dict <- data_dict %>%
    filter(cont_var == "yes")
  
  
  cont_choices <- cont_dict %>%
    split(.$category) %>%
    lapply(function(x) setNames(x$var_name, x$clean_name))
  
  
  ################
  ### Observes ###
  ################
  
  
  ######################################
  ## Observe Event for Application Logic
  ######################################
  
  ## TO DO: abstract the checkbox input names and pass as ine thing 
  observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE,
               list(input$Bivariate, 
                    # this isolate prevents re rendering loop 
                    input$Simplify,
                    isolate(Controller$data_select), 
                    isolate(Controller$data),
                    input$VarOne, 
                    input$VarTwo, 
                    input$Geography, 
                    event_one(), 
                    event_two(), input$FilterCats), {
                      
                      ## Simplifying polygons                 
                      if(input$Simplify == TRUE)
                      {
                        Controller$data <- tx_raw %>%
                          data.frame()%>%
                          select(-c(geometry))%>%
                          left_join(.,tx_sab_super_simplified)%>%
                          st_as_sf()
                      }
                      else
                      {
                        Controller$data <- tx_raw
                      }
                      
                      ## Main Catches and Geography Filter
                      if(length(input$Geography) == 0 | input$VarOne == input$VarTwo | input$VarOne == "" | input$VarTwo == "") {
                        showNotification(id = "notification", "Insufficient data selected or duplicate variables chosen, please change selection", type = "warning")
                      } else {
                        
                        ## Filter data based on selected geography
                        if(!str_detect(paste(input$Geography, collapse = "|"), "All Texas")) {
          

                          selected_inputs <- unlist(str_split(input$Geography, ","))
                          
                          # Initialize a logical vector to store the filtering result
                          filter_result <- logical(nrow(Controller$data))
                          
                          # Loop through each row
                          for (i in 1:nrow(Controller$data)) {
                            # Check if county_served or any region matches any of the selected inputs
                            if (any(sapply(str_split(Controller$data$county_served[i], ",\\s*"), function(x) any(x %in% selected_inputs))) |
                                any(sapply(str_split(Controller$data$regions[i], ",\\s*"), function(x) any(x %in% selected_inputs)))) {
                              filter_result[i] <- TRUE
                            }
                          }
                          # Filter the data
                          Data <- Controller$data[filter_result, ]
                          
                        } else {
                          Data <- Controller$data
                        }
                        
                        ## Calculate event min and max values
                        event_one_max <- if (!is.null(event_one())) max(event_one()$x, na.rm = TRUE) else max(Data %>% pull(!!input$VarOne), na.rm = TRUE)
                        event_one_min <- if (!is.null(event_one())) min(event_one()$x, na.rm = TRUE) else min(Data %>% pull(!!input$VarOne), na.rm = TRUE)
                        event_two_max <- if (!is.null(event_two())) max(event_two()$x, na.rm = TRUE) else max(Data %>% pull(!!input$VarTwo), na.rm = TRUE)
                        event_two_min <- if (!is.null(event_two())) min(event_two()$x, na.rm = TRUE) else min(Data %>% pull(!!input$VarTwo), na.rm = TRUE)
                        ## Filter data based on events
                        
                        Data <- Data %>%
                          filter(!!as.symbol(input$VarOne) <= event_one_max &
                                   !!as.symbol(input$VarOne) >= event_one_min &
                                   !!as.symbol(input$VarTwo) <= event_two_max &
                                   !!as.symbol(input$VarTwo) >= event_two_min)
                        
                        if (!is.null(input$FilterCats[1])) {
                          if (input$FilterCats > 0) {
                            for (col in cat_columns) {
                              checkboxSelection[[col]] <- input[[col]]
                            }
                            for (col in cat_columns) {
                              selectedChoices <- checkboxSelection[[col]]
                              if (is.null(selectedChoices)) {
                                # If "NULL" is in selectedChoices, set Data to be 1 row
                                Data <- Data %>% slice(1)
                                break  # Exit the loop
                              } else if (length(selectedChoices) > 0) {
                                Data <- Data[Data[[col]] %in% selectedChoices, ]
                              }
                            }
                          }
                        }

                        if(nrow(Data) < 2) {
                          showNotification(id = "notification", "Insufficient data, please select at least two utilities.", type = "warning")
                        } else {
                          
                          ## Update Controller$data_select
                          isolate(Controller$data_select <- Data)
                          
                          # Calculate label text outside the loop
                          label_text <- paste0(
                            "<b>", str_to_title(Data$pws_name), "</b> <br>",
                            "<b>", data_dict %>% filter(var_name == "pwsid") %>% pull(clean_name), ": </b> ", Data$pwsid, " <br>",
                            "<b>", data_dict %>% filter(var_name == "estimate_total_pop") %>% pull(clean_name), ": </b> ", round(Data$estimate_total_pop, 0), " <br>",
                            "<b>", data_dict %>% filter(var_name == !!(input$VarOne)) %>% pull(clean_name), ": </b> ", round(Data[[input$VarOne]], 2), " <br>",
                            "<b>", data_dict %>% filter(var_name == !!(input$VarTwo)) %>% pull(clean_name), ": </b> ", round(Data[[input$VarTwo]], 2), " <br>"
                          )
                          Data$label_text <- lapply(label_text, htmltools::HTML)
                          
                          
                          ## setting up leafletproxy 
                          leaflet_proxy <- leafletProxy("Map") %>%
                            clearGroup("Sabs")%>%
                            clearControls()
                          
                          if (input$Bivariate == TRUE) {
                            ## Bivariate plot
                            leaflet_proxy %>%
                              bivariatechoropleths::addBivariateChoropleth(
                                map_data = Data,
                                var1_name = input$VarOne,
                                var2_name = input$VarTwo,
                                ntiles = 3,
                                var1_label = data_dict %>% filter(var_name == !!(input$VarOne)) %>% pull(clean_name),
                                var2_label = data_dict %>% filter(var_name == !!(input$VarTwo)) %>% pull(clean_name),
                                weight = 1,
                                fillOpacity = 0.7,
                                color = "black",
                                paletteFunction = pals::tolochko.redblue,
                                group = "Sabs"
                              ) %>%
                              addPolygons(data = Data,
                                          label = ~label_text,
                                          fillOpacity = 0,
                                          opacity = 0,
                                          group = "Sabs")
                          } else {
                            ## Univariate plot
                            n_unique <- length(unique(na.omit(Data[[input$VarOne]])))
                            if (n_unique == 1) {
                              colvar <- c(0, 1)
                            } else {
                              colvar <- quantile(Data[[input$VarOne]], probs = c(0, 0.25, 0.5, 0.75, 1))
                            }
                            
                            pal <- colorNumeric(
                              palette = c("#f5f5f5", "#7ab3d1", "#036eae"),
                              domain = colvar
                            )
                            leaflet_proxy %>%
                              addPolygons(data = Data,
                                          layerId = ~pwsid,
                                          label = ~label_text,
                                          fillColor = ~pal(Data[[input$VarOne]]),
                                          fillOpacity = .9,
                                          weight = .75,
                                          color = "grey",
                                          group = "Sabs") %>%
                              addLegend(pal = pal, values = colvar, position = "bottomleft", title = data_dict %>% filter(var_name == !!(input$VarOne)) %>% pull(clean_name))
                          }
                        }
                      }
                      Sys.sleep(.5)
                      hide_spinner()
                    })
  
  ################################# 
  # Observe for panel show/hides ##
  #################################
  
  observeEvent(input$Context, {
    toggle("Map", anim = FALSE,animType = "slide")
    toggle("Table", anim = FALSE,animType = "slide")
    toggle("TableText", anim = FALSE,animType = "slide")
  })
  
  observeEvent(ignoreInit = TRUE, input$hideSidebar,{
    toggle("sidebar", anim = FALSE,animType = "slide")
  })
  
  ################
  #### Map #######
  ################
  output$Map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 1, maxZoom = 12))%>%
      addProviderTiles(providers$CartoDB.Positron, group = "Streets")%>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Topographic")%>%
      addMapPane("base_polygons", zIndex = 200)%>%
      addPolygons(data = tx_counties, fillColor = "grey", fillOpacity = .2, 
                  weight = .7, color = "black", label = ~namelsad, group = "Counties",
                  options = pathOptions(pane = "base_polygons"))%>%
      addPolygons(data = tx_regions, fillColor = "grey", fillOpacity = .2, 
                  weight = .7, color = "black", label = ~label_2, group = "Regions",
                  options = pathOptions(pane = "base_polygons"))%>%
      setView(-95.58292, 31.94214, zoom = 7)%>%
      hideGroup("Counties")%>%
      hideGroup("Regions")%>%
      hideGroup("Topographic")%>%
      addLayersControl(overlayGroups = c("Counties","Regions"), baseGroups =  c("Streets", "Topographic"))
  })
  
  
  
  ################
  #### Sidebar ###
  ################
  
  ## Geography Filter ##
  output$SelectGeography <- renderUI({
    GeoChoices <- list()
    
    GeoChoices$All <- c("All Texas","")
    GeoChoices$Regions <- sort(unique_regions)
    GeoChoices$Counties <- sort(Counties)
    
    tagList(
      tipify(el = icon(name = "map-location-dot", lib = "font-awesome", style = "font-size: 17px"), placement = "right", 
             title = HTML("Search or select a region defined by the Texas Water Development Board, or an individual county. Max selection size is five")),
      HTML(paste("<b> Geography: </b>")),
      selectizeInput("Geography","", choices = GeoChoices, selected = "I - East Texas", multiple = TRUE, options = list(maxItems = 5)),
      div(style = "display:flex; align-items: center; margin-top: -20px",
          tipify(el = icon(name = "draw-polygon", lib = "font-awesome", style = "font-size: 17px; margin-right: 5px;"), placement = "right",
                 title = HTML("On limited bandwith or plotting lots of data? Reduce the data quality for service area boundary geographies to improve rendering")),
          HTML(paste("<b> Simplify: </b>")),
          HTML("&nbsp;"), # Adding a non-breaking space for spacing
          div(style = "margin-bottom: -12px; margin-right: -3px;",
              checkboxInput("Simplify", ""))
          
      ))
  })
  
  
  # quick function for generating checkbox inputs for catagorical filters dynamically
  generateCheckboxGroupInput <- function(inputId, label, choices, selected) {
    checkboxGroupInput(inputId, label, choices = choices, selected = selected)
  }
  
  output$SelectCat <- renderUI({

    inputIds <- cat_dict$var_name
    
    checkbox_inputs <- mapply(function(col, lab, inputId) {
      choices <- sort(na.omit(unique(Controller$data[[col]])))
      selected <- unique(Controller$data[[col]])
      div(
        class = "col-md-6",
        generateCheckboxGroupInput(inputId, lab, choices, selected)
      )
    }, cat_columns, cat_labels, inputIds, SIMPLIFY = FALSE)
    tagList(
    fluidRow(do.call(tagList, checkbox_inputs)),
    actionButton("FilterCats", "Filter")
        )
  })
  
  
  output$SummaryStats <- renderUI({
    req(Controller$data_select)
    ## Number of selected utilities
    ## Total population served
    ## Median Household Income
    ## Percent of Color 
    
    UtilityCount <- paste("<i>", "Utility Count: </i> <b>", scales::number(length(unique(Controller$data_select$pwsid)), big.mark = ","),"</b>", "<br>")
    Population <- paste("<i>", "Utility Users: </i> <b>", scales::number(sum(Controller$data_select$estimate_total_pop, na.rm = TRUE), big.mark = ","),"</b>", "<br>")
    MHI <- paste("<i> ", "Avg. Median Household Income: </i> <b>", dollar(mean(Controller$data_select$estimate_mhi, na.rm = TRUE)),"</b>", "<br>")
    POC <- paste("<i>", "Percent of Color: </i> <b>", scales::percent(mean(Controller$data_select$estimate_poc_alone_per, na.rm = TRUE) / 100) ,"</b>", "<br>")
    
    tagList(
      HTML(paste("<b> Summary Statistics: </b> <br>")),
      HTML(UtilityCount),
      HTML(Population), 
      HTML(POC),
      HTML(MHI)
    )
    
  })
  
  ## Variable One Select
  output$VarOne <- renderUI({
    tagList(
      div(style = "display:flex; align-items: center; margin-top: -10px; margin-bottom: -6px",
          tipify(el = icon(name = "clone", lib = "font-awesome", style = "font-size: 17px; margin-right: 5px;"), placement = "right",
                 title = HTML("Map your primary variable against your secondary variable")),
          HTML(paste("<b> Bivariate Mapping: </b>")),
          HTML("&nbsp;"), # Adding a non-breaking space for spacing
          div(style = "margin-bottom: -9px; margin-right: -6px;",
              checkboxInput("Bivariate", "", value = FALSE))
      ),
      tipify(el = icon(name = "chart-column", lib = "font-awesome", style = "color: #7ab3d1; font-size: 17px"), placement = "right", 
             title = HTML("Select a variable to modify the map and histogram below. You can define the variable range by clicking and dragging the histogram. Double click to reset.")),
      HTML(paste("<b> Primary Variable: </b>")),
      selectizeInput("VarOne", "", choices = cont_choices, selected = "estimate_mhi", multiple = FALSE)
    )
    
  })
  
  output$VarOneMinMax <- renderUI({
    req(Controller$data_select)
  #  checkbox
    event_one_max <- if (!is.null(event_one())) max(event_one()$x, na.rm = TRUE) else max(Controller$data_select %>% pull(!!input$VarOne), na.rm = TRUE)
    event_one_min <- if (!is.null(event_one())) min(event_one()$x, na.rm = TRUE) else min(Controller$data_select%>% pull(!!input$VarOne), na.rm = TRUE)
    
    min <- paste(" <i> Min: </i> <b>",scales::number(round(event_one_min,4),big.mark = ","), "</b>")
    max <- paste(" <i> Max: </i> <b>",scales::number(round(event_one_max,2),big.mark = ","), "</b>")
    
    tagList(
        HTML(min),
        HTML(max),
      )
  })
  
  # Variable One Hist
  output$VarOneHist <- renderPlotly({
    req(Controller$data_select)
    req(input$VarOne)
    
    m <- list(
      l = 5,
      r = 5,
      b = 5,
      t = 5,
      pad = 5
    )
    
    plot_ly(x =  Controller$data_select %>% pull(!!input$VarOne), 
            type = "histogram", source = "a", 
            nbinsx = 50, 
            marker = list(color = "#7ab3d1", line = list(color = "grey", width = 1)))%>% 
      config(displayModeBar = FALSE) %>%
      event_register("plotly_selected")%>%
      layout( dragmode = "select", margin = m)
  })
  
  ## Variable Two Select
  output$VarTwo <- renderUI({
    tagList(
      tipify(el = icon(name = "chart-column", lib = "font-awesome", style = "color: #dd7c8a; font-size: 17px;"), placement = "right",
             title = HTML("Select a variable to modify the utilities mapped and the histogram below. Click Bivariate to map this variable with your primary variable. You can define the variable range by clicking and dragging the histogram. Double click to reset.")),
      HTML(paste("<b> Secondary Variable: </b>")),
      selectInput("VarTwo", "", choices = cont_choices, selected = "healthbased_violations_5yr", multiple = FALSE)
    )
  })
  
  output$VarTwoMinMax <- renderUI({
    req(Controller$data_select)
    event_two_max <- if (!is.null(event_two())) max(event_two()$x, na.rm = TRUE) else max(Controller$data_select %>% pull(!!input$VarTwo), na.rm = TRUE)
    event_two_min <- if (!is.null(event_two())) min(event_two()$x, na.rm = TRUE) else min(Controller$data_select%>% pull(!!input$VarTwo), na.rm = TRUE)
    
    min <- paste(" <i> Min: </i> <b>",scales::number(round(event_two_min,2),big.mark = ","), "</b>")
    max <- paste(" <i> Max: </i> <b>",scales::number(round(event_two_max,2),big.mark = ","), "</b>")
    
    tagList(
      HTML(min),
      HTML(max)
    )
    
  })
  
  # Variable Two Hist
  output$VarTwoHist <- renderPlotly({
    req(Controller$data_select)
    req(input$VarTwo)
    m <- list(
      l = 5,
      r = 5,
      b = 5,
      t = 5,
      pad = 5
    )
    
    plot_ly(x =  Controller$data_select %>% pull(!!input$VarTwo), type = "histogram", source = "b", 
            nbinsx = 50, 
      marker = list(color = "#dd7c8a", line = list(color = "grey", width = 1)))%>% 
      config(displayModeBar = FALSE) %>%
      event_register("plotly_selected")%>%
      layout(dragmode = "select",  margin = m)
  })
  
  ################
  #### Table #####
  ################
  output$TableText <- renderText({
    paste("Click a column to sort and slide a column to expand")
  })
  
  output$Table <- renderUI({
    req(Controller$data_select)
    TableData <- Controller$data_select %>%
      data.frame()%>%
      select(-c(geometry)) %>%
      mutate_if(is.numeric, round, digits = 2) %>%
      select(-c("tier", "east_tx_flag")) %>%
      relocate(pws_name)
    renderReactable({
      reactable(TableData,
                columns = list(
                  # utility characteristics:
                  pwsid = colDef(aggregate = "unique",
                                 name = "ID"),
                  pws_name = colDef(aggregate = "unique",
                                    name = "Water System Name", 
                                    minWidth = 150),
                  county_served = colDef(aggregate = "unique",
                                         name = "County"),
                  regions = colDef(aggregate = "unique",
                                   name = "Region"),
                  primary_source_code = colDef(aggregate = "unique",
                                               name = "Source"),
                  owner_type_description = colDef(aggregate = "unique",
                                                  name = "Owner"),
                  pop_catagories = colDef(aggregate = "unique",
                                          name = "Pop Cat"),
                  pop_density = colDef(name = "Pop Density"),
                  area_miles = colDef(name = "Area (mi)"),
                  # socioeconomic:
                  estimate_mhi = colDef(name = "MHI ($)"),
                  estimate_total_pop = colDef(name = "Population", 
                                              minWidth = 150),
                  estimate_white_alone_per = colDef(name = "% White Alone", 
                                                    minWidth = 150),
                  estimate_black_alone_per= colDef(name = "% Black Alone", 
                                                   minWidth = 150),
                  estimate_AIAN_alone_per= colDef(name = "% American Indian and Alaskan Native Alone", 
                                                  minWidth = 150),
                  estimate_asian_alone_per = colDef(name = "% Asian Alone", 
                                                    minWidth = 150),
                  estimate_NAPI_alone_per= colDef(name = "% Native American and Pacific Islander Alone", 
                                                  minWidth = 150),
                  estimate_other_alone_per= colDef(name = "% Other Race Alone", 
                                                   minWidth = 150),
                  estimate_mixed_alone_per= colDef(name = "% Mixed Race Alone", 
                                                   minWidth = 150),
                  estimate_poc_alone_per = colDef(name = "%POC", 
                                                  minWidth = 150),
                  estimate_hisp_alone_per = colDef(name = "% Latino/a", 
                                                   minWidth = 150),
                  estimate_ageunder_5_per = colDef(name = "% Age < 5", 
                                                   minWidth = 150),
                  estimate_bachelors_per = colDef(name = "% Bachelor's Degree", 
                                                  minWidth = 150),
                  estimate_prof_degree_per = colDef(name = "% Professional Degree", 
                                                    minWidth = 150),
                  estimate_laborforce_unemployed_per = colDef(name = "% Unemployment", 
                                                              minWidth = 150),
                  estimate_hh_below_pov_per = colDef(name = "% Poverty"),
                  percent_disadv_cejst= colDef(name = "% Service Area that is Disadvantaged", 
                                               minWidth = 150),
                  mean_thresholds_exceeded_cejst = colDef(name = "Mean CEJST Thresholds Exceeded", 
                                                          minWidth = 150),
                  # violations:
                  paperwork_violations_10yr = colDef(name = "Non-Health, 10yr"),
                  healthbased_violations_10yr = colDef(name = "Health, 10yr"),
                  total_violations_10yr = colDef(name = "Total, 10yr"),
                  paperwork_violations_5yr = colDef(name = "Non-Health, 5yr"),
                  healthbased_violations_5yr = colDef(name = "Health, 5yr"),
                  total_violations_5yr = colDef(name = "Total, 5yr"),
                  # financial: 
                  total_water_sewer = colDef(name = "Annual Water & Sewer Rate ($)", 
                                             minWidth = 150),
                  dwsrf_times_funded= colDef(name = "Times funded - DW SRF (2009 - 2020)", 
                                             minWidth = 150),
                  dwsrf_total_assistance= colDef(name = "Total assistance - DW SRF (2009 - 2020)", 
                                                 minWidth = 150),
                  dwsrf_total_pf= colDef(name = "Total principal forgiveness - DW SRF (2009 - 2020)", 
                                         minWidth = 150),
                  dwsrf_median_assistance= colDef(name = "Median assistance - DW SRF (2009 - 2020)", 
                                                  minWidth = 150),
                  # environmental: 
                  limited_water_use = colDef(name = "Limited Water Notices (2023-2024)", 
                                             minWidth = 150),
                  total_bwn= colDef(name = "Boil Water Notices Since 2018", 
                                    minWidth = 150),
                  cvi_weighted_score = colDef(name = "Climate Vulnerability Index", 
                                              minWidth = 150),
                  haz_waste= colDef(name = "Hazardous Waste Indicator", 
                                    minWidth = 150),
                  rmp= colDef(name = "RMP Facility Indicator"),
                  storage_tanks = colDef(name = "Underground Storage Tank Indicator", 
                                         minWidth = 150),
                  superfund = colDef(name = "Superfund Proximity Indicator"),
                  waste_discharge= colDef(name = "Waste Discharge Indicator", 
                                          minWidth = 150)
                ),
                columnGroups = list(
                  colGroup(name = "Utility", columns = c("pwsid", "pws_name",
                                                         "county_served",
                                                         "regions",
                                                         "primary_source_code",
                                                         "owner_type_description", "pop_catagories",
                                                         "pop_density", "area_miles")),
                  colGroup(name = "Socioeconomic", columns = c("estimate_mhi", "estimate_total_pop",
                                                               "estimate_hisp_alone_per", "estimate_laborforce_unemployed_per",
                                                               "estimate_hh_below_pov_per", "estimate_poc_alone_per", 
                                                               "estimate_white_alone_per",
                                                               "estimate_black_alone_per",
                                                               "estimate_AIAN_alone_per",
                                                               "estimate_asian_alone_per",
                                                               "estimate_NAPI_alone_per",
                                                               "estimate_other_alone_per",
                                                               "estimate_mixed_alone_per",
                                                               "estimate_bachelors_per",
                                                               "estimate_prof_degree_per",
                                                               "estimate_ageunder_5_per",
                                                               "percent_disadv_cejst", "mean_thresholds_exceeded_cejst")),
                  colGroup(name = "Violations", columns = c("healthbased_violations_5yr",
                                                            "healthbased_violations_10yr",
                                                            "paperwork_violations_5yr",
                                                            "paperwork_violations_10yr",
                                                            "total_violations_5yr",
                                                            "total_violations_10yr")), 
                  colGroup(name = "Financial", columns = c("total_water_sewer", 
                                                           "dwsrf_times_funded", 
                                                           "dwsrf_total_assistance",
                                                           "dwsrf_total_pf",
                                                           "dwsrf_median_assistance")), 
                  colGroup(name = "Environmental", columns = c("limited_water_use", 
                                                               "cvi_weighted_score",
                                                               "total_bwn", 
                                                               "haz_waste", "rmp", 
                                                               "storage_tanks", "superfund", 
                                                               "waste_discharge"))
                ),
                highlight = TRUE,
                bordered = TRUE,
                resizable = TRUE,
                showSortable = TRUE,
                searchable = TRUE,
                # adopted from this stock overflow question: https://stackoverflow.com/questions/74222616/change-search-bar-text-in-reactable-table-in-r
                language = reactableLang(
                  searchPlaceholder = "Search the table",
                  noData = "No entries found",
                  pageInfo = "{rowStart}\u2013{rowEnd} of {rows} entries",
                  pagePrevious = "\u276e",
                  pageNext = "\u276f",
                  pagePreviousLabel = "Previous page",
                  pageNextLabel = "Next page"),
                defaultPageSize = 15,
      )
    })
  })
  

  InfoModal <- modalDialog(
    title = HTML("<b> Texas Community Water System Prioritization Tool - dev 1.0 </b>"),
    HTML("<b> Quick Start: </b>"),
    HTML("<br>"),
    icon(name = "map-location-dot", lib = "font-awesome", style = "font-size: 17px"),
    HTML("Search or select an area of choice from the Geography filter."),
    HTML("<br>"),
    icon(name = "filter", lib = "font-awesome", style = "font-size: 17px"),
    HTML(" Click Filter by Categories to expand categorical filter options.  "),
    HTML("<br>"),
    icon(name = "chart-column", lib = "font-awesome", style = "size: 17px"),
    HTML("Select a Primary and Secondary variable to map."),
    HTML("<br>"),
    HTML("Adjust data ranges by click and dragging the chart - double click to reset."),
    HTML("<br>"),
    icon(name = "clone", lib = "font-awesome", style = "font-size: 17px;"),
    HTML(" Select Bivariate mapping to map both variables."),
    HTML("<br>"),
    icon(name = "arrows-left-right", lib = "font-awesome", style = "font-size: 17px;"),
    HTML("Click Table or Map to toggle between the map and the table. "),
    HTML("<br>"),
    icon(name = "computer-mouse", lib = "font-awesome", style = "font-size: 17px;"),
    HTML("Mouse over the application's icons to learn more!"),
    HTML("<br>"),
    HTML("<br>"),
    HTML("<b> About and Uses: </b>"),
    HTML("<br>"),
    HTML("This application was developed to assist in prioritizing advocacy and technical assistance for community water systems in Texas. 
         Known as a screening tool [link], the data and insights generated from this tool are to be taken in conjunction with research and local knowledge to inform outreach and not a sole source of information. 
         This tool can be used to identify utilities based on a user determined set of characteristics. Keep in mind, these data are a small component of utility operations and drinking water user experience. Generally speaking, utilities are working to balance quality water, low rates, and financial stability, all while staying within regulatory compliance. 
         This balancing act can be difficult for under-resourced utilities"),
    HTML("<br>"),
    HTML("<br>"),
    HTML("<b> More Information and Feedback: </b>"),
    HTML("<li> To learn more about how to use this tool, visit our vignettes [link].   </li>"),
    HTML("<li> For documentation, methods, and reproducing this application, see our Github [link] </li>"),
    HTML("<li> Downloading the complete dataset can be found here [link].  </li> "),
    HTML("<li> To read our deep-dive report on East Texas, click here [link].  </li>"),
    HTML("<li> Got feedback? Take our survey! [link] "),
    HTML("<br>"),
    HTML("<br>"),
    HTML("<b> Attribution and License:  </b>"),
    HTML("<br>"),
    HTML("Developed in partnership with Mitchell and Temple foundations by Environmental Policy Innovation Center (EPIC). EPIC makes no assurances to the accuracy of the tools data. 
         All underlying code, methods, and data are available at our Github under a Creative Commons License."),
    easyClose = FALSE,
    footer = modalButton("Close"),
  )
  
  observeEvent(input$showInfo, ignoreNULL = FALSE,
               {
                 showModal(InfoModal)
               })
  
  ################
  #### Report ####
  ################

  # code for report adopted from: https://shiny.posit.co/r/articles/build/generating-reports/
  output$Report <- downloadHandler(
    
    filename = "Report.html",
    content = function(file_n) {
      withProgress(message = 'Rendering, please wait!', {
        # this downloads - need to figure out how to add it to the params
        # shinyscreenshot::screenshot(id = "Map")
        # src <- normalizePath("tx-report.Rmd")
        temp_dir <- tempdir()
        owd <- setwd(temp_dir)
        on.exit(setwd(owd))
        
        temp_rmd <- file.path(tempdir(), "tx-report.Rmd")
        
        Sys.sleep(1)
        
        writeLines(report, temp_rmd)
        params <- list(data_p = Controller$data_select,
                       var_one = input$VarOne,
                       var_two = input$VarTwo)
        out <- rmarkdown::render("tx-report.Rmd",
                                 params = params,
                                 envir = new.env(parent = globalenv()))
        file.rename(out, file_n)
      })
    })
  
# download handler: 
output$downloadData <- downloadHandler(
  
  filename = "tx-dw-app.zip",
  
  content = function(file) {
    
    drive_deauth()
     
    # add data dictionary: 
    dictionary_csv <- drive_download("https://docs.google.com/spreadsheets/d/1bzNPxhL-l6DeGElhG1c70Of8DGAQasMDUuX3rPHVe2A/edit#gid=0", 
                                     file.path(tempdir(), "tx-app-data-dictionary.csv"), overwrite = TRUE)
    
    # add methods doc: 
    methods_doc <- drive_download("https://docs.google.com/document/d/1va2Iq2oJxnqiwgNHD4bWpXKxdWbq-TYoYkosj1oz_JU/edit", 
                                   file.path(tempdir(),"tx-app-methods.docx"), overwrite = TRUE)
    
    # if statement to handle different file formats: 
    if(input$downloadType == ".csv") {
      # grabbing the selected data: 
      csv_data <- Controller$data_select %>% 
        as.data.frame() %>%
        select(-"geometry")
      write.csv(csv_data, 
                file.path(tempdir(), "tx-app-selected-data.csv"), 
                row.names = FALSE)
      data_path_selected <- file.path(tempdir(), "tx-app-selected-data.csv")
      
      # grabbing the full dataset: 
      tx_raw_data <- tx_raw %>%
        as.data.frame() %>%
        select(-"geometry")
      write.csv(tx_raw_data, 
                file.path(tempdir(), "tx-app-full-data.csv"), 
                row.names = FALSE)
      data_path_full <- file.path(tempdir(), "tx-app-full-data.csv")
      
    } else if(input$downloadType == ".geojson") {
      # grabbing the selected data: 
      st_write(Controller$data_select, file.path(tempdir(), "tx-app-selected-data.geojson"), delete_layer = TRUE)
      data_path_selected <- file.path(tempdir(), "tx-app-selected-data.geojson")
      
      # grabbing the full dataset: 
      st_write(tx_raw, file.path(tempdir(), "tx-app-full-data.geojson"), delete_layer = TRUE)
      data_path_full <- file.path(tempdir(), "tx-app-full-data.geojson")
    }
    
    # zippin' it up!
    zip::zip(file, files = c(file.path(tempdir(), "tx-app-data-dictionary.csv"),
                             file.path(tempdir(),"tx-app-methods.docx"),
                             data_path_selected, 
                             data_path_full),
             mode = "cherry-pick")
  })

}
# Run the application 
shinyApp(ui = ui, server = server)
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
library(waiter)
library(zip)


###########
### UI #### 
###########
ui <- fluidPage(
  
  #  Global CSS alterations 
  tags$head(
    tags$style(
      # Spacing on selective control, rounding corners on plotly chart, and modifying info button
      HTML("
      .selectize-control {
        margin-top: -20px; 
        margin-bottom: -10px;
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
  
  ## Instantiating Waitress and install dependencies  
  useWaitress(color = "#79b2d1", percent_color = "#333333"),
  reactable_extras_dependency(),
  add_busy_spinner(spin = "fading-circle", position = "top-left"),
  useShinyjs(),
  
  ## Sidebar layout 
  sidebarLayout(
    div(
      id ="sidebar",
      sidebarPanel(
        style = "position: fixed; height: 90vh; overflow-y: auto; margin-left: -30px;", div(style = "display:inline-block; float:right; margin-bottom: 20px"),
        width = 3,
        actionButton("showInfo", "", icon(name = "circle-question", lib = "font-awesome", style = "font-size: 17px"), class = "info-button"), 
        uiOutput("SelectGeography", style = "width: 95%"), 
        bsCollapsePanel(
          div(
            style = "display: inline-block; position: relative;",
            div(
              style = "margin-left: -10px; margin-bottom: -18px",
              tipify(el = icon(name = "filter", lib = "font-awesome"), 
                     placement = "right", title = HTML("Filter the utilities selected by these categories. Some filters might not effect your data due to insufficent count. As you change data, map will recolor."))
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
                     style = "margin-bottom: 10px;; margin-top: 10px;"), 
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
    # Main Panel Layout
    mainPanel(
      style = "margin-left: -15px;",
      leafletOutput("Map", height = "90vh"),
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
  
  # Starting Waitress 
  waitress <- Waitress$
    new(theme = "overlay-percent")$
    start() # start
  
  #################### 
  ### Data Import #### 
  #################### 
  # increase by 20
  waitress$inc(20) 
  # Raw Application Data
  tx_raw <- aws.s3::s3read_using(st_read, 
                                 object = "state-drinking-water/TX/clean/app/app-data-prod.geojson",
                                 bucket = "tech-team-data", 
                                 quiet = TRUE)
  # TX Counties
  tx_counties <- aws.s3::s3read_using(st_read, 
                                      object = "state-drinking-water/TX/clean/app/tx-counties-simple-prod.geojson",
                                      bucket = "tech-team-data",
                                      quiet = TRUE)
  # increase by 20
  waitress$inc(20) 
  
  # TX Regions
  tx_regions <- aws.s3::s3read_using(st_read, 
                                     object = "state-drinking-water/TX/clean/app/tx-regions-simple-prod.geojson",
                                     bucket = "tech-team-data",
                                     quiet = TRUE)
  
  # Simpified TX Data
  tx_sab_super_simplified <- aws.s3::s3read_using(st_read, 
                                                  object = "state-drinking-water/TX/clean/app/tx-sab-super-simple-prod.geojson",
                                                  bucket = "tech-team-data",
                                                  quiet = TRUE)
  # increase by 20
  waitress$inc(20) 

  # Data Dictionary 
  suppressMessages({data_dict <- aws.s3::s3read_using(read.csv,
                                                      object = "state-drinking-water/TX/clean/app/app-data-dict-prod.csv",
                                                      bucket = "tech-team-data")})
  # Writing data dictionary to temp 
  write.csv(data_dict, file.path(tempdir(), "tx-app-data-dictionary.csv"), row.names = FALSE)
  suppressWarnings({report <- aws.s3::s3read_using(readLines,object = "state-drinking-water/TX/clean/app/tx-report-prod.Rmd",
                                                   bucket = "tech-team-data")})

  drive_deauth()
  methods_doc <- drive_download("https://docs.google.com/document/d/1va2Iq2oJxnqiwgNHD4bWpXKxdWbq-TYoYkosj1oz_JU/edit", 
                                file.path(tempdir(),"tx-app-methods.docx"), overwrite = TRUE)
  
  ################
  ### Variables ##
  ################
  # Modifying data dictionary to only include defs for variables in app data
  data_dict <- data_dict %>%
    filter(var_name %in% colnames(tx_raw))
  
  # Setting the controller to the raw data 
  Controller <- reactiveValues()
  Controller$data <- tx_raw 
  
  # List of counties
  Counties <- unique(tx_raw$county_served)
  
  # increase by 20
  waitress$inc(20) 
  
  # Generating unique list of regions
  pwsid_regions <- tx_raw %>%
    data.frame()%>%
    select(regions)%>%
    unique()
  
  unique_regions <- unique(unlist(strsplit(pwsid_regions$regions, ",\\s*")))
  unique_regions <- unique_regions[!is.na(unique_regions)]
  
  ## plotly chart events
  event_one <- reactive(event_data(event = "plotly_selected", source = "a", priority = "event"))
  event_two <- reactive(event_data(event = "plotly_selected", source = "b", priority = "event"))


  # increase by 20
  waitress$inc(20) 
  
  # instantiating checkbox selection 
  checkboxSelection <- reactiveValues()
  
  # setting up categorical filter options and labels 
  cat_dict <- data_dict %>%
    filter(cat_var == "yes")
  
  cat_choices <- str_split(cat_dict$var_name, cat_dict$clean_name)
  
  cat_columns <- cat_dict$var_name
  cat_labels <- cat_dict$clean_name
  
  # Continous variables choices 
  cont_dict <- data_dict %>%
    filter(cont_var == "yes")
  
  cont_order <- c(
    "Calculated",
    "Water Delivery System",
    "Financial",
    "Socioeconomic",
    "Environmental Justice Indicators"
  )
  
  cont_choices <- cont_dict %>%
    split(.$category) %>%
    lapply(function(x) setNames(x$var_name, x$clean_name))
  
  cont_choices <- cont_choices[match(cont_order, names(cont_choices))]

  # closing waitress 
  waitress$close() 
  ################
  ### Observes ###
  ################
  
  
  ######################################
  ## Observe Event for Application Logic
  ######################################
  
  ### Main Observe Event ###
  ## Logic 
  ## 1. Checks to see if data has been simplified 
  ## 2. Geography selection and Input$VarOne and Input$VarTwo insufficient data handling 
  ## 3. Input$VarOne and Input$VarTwo selection 
  ## 4. Categorical variable selection
  ## 5. Insufficent data handling 
  ## 6. Updating Controller$data_select 
  ## 7. Creating leaflet labels and basic leaflet proxy call
  ## 8. Bivariate mapping leaflet proxy
  ## 9. Single variate mapping leaflet proxy
  observeEvent(
    ignoreInit = TRUE, ignoreNULL = TRUE,
    list(
      input$Bivariate,
      input$Simplify,
      isolate(Controller$data_select),
      isolate(Controller$data),
      input$VarOne,
      input$VarTwo,
      input$Geography,
      event_one(),
      event_two(),
      input$FilterCats
    ),
    {
# 1. Checks to see if data has been simplified                  
      if (input$Simplify == TRUE) {
        Controller$data <- tx_raw %>%
          data.frame() %>%
          select(-c(geometry)) %>%
          left_join(., tx_sab_super_simplified) %>%
          st_as_sf()
      } else {
        Controller$data <- tx_raw
      }
      
# 2. Geography selection and Input$VarOne and Input$VarTwo insufficient data handling  
      if (length(input$Geography) == 0 | input$VarOne == input$VarTwo | input$VarOne == "" | input$VarTwo == "") {
        showNotification(id = "notification", "Insufficient data selected or duplicate variables chosen, please change selection", type = "warning")
      } else {
        # Filter data based on selected geography or All Texas
        if (!str_detect(paste(input$Geography, collapse = "|"), "All Texas")) {
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
        
# 3. Input$VarOne and Input$VarTwo selection 
        event_one_max <- if (!is.null(event_one())) max(event_one()$x, na.rm = TRUE) else max(Data %>% pull(!!input$VarOne), na.rm = TRUE)
        event_one_min <- if (!is.null(event_one())) min(event_one()$x, na.rm = TRUE) else min(Data %>% pull(!!input$VarOne), na.rm = TRUE)
        event_two_max <- if (!is.null(event_two())) max(event_two()$x, na.rm = TRUE) else max(Data %>% pull(!!input$VarTwo), na.rm = TRUE)
        event_two_min <- if (!is.null(event_two())) min(event_two()$x, na.rm = TRUE) else min(Data %>% pull(!!input$VarTwo), na.rm = TRUE)
        
        Data <- Data %>%
          filter(!!as.symbol(input$VarOne) <= event_one_max &
                   !!as.symbol(input$VarOne) >= event_one_min &
                   !!as.symbol(input$VarTwo) <= event_two_max &
                   !!as.symbol(input$VarTwo) >= event_two_min)
        
# 4. Categorical variable selection
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
        
# 5. Insufficent data handling 
        if (nrow(Data) < 2) {
          showNotification(id = "notification", "Insufficient data, please select at least two utilities.", type = "warning")
        } else {
          # 6. Updating Controller$data_select 
          isolate(Controller$data_select <- Data)
          
          # 7. Creating leaflet labels and basic leaflet proxy call
          label_text <- paste0(
            "<b>", str_to_title(Data$pws_name), "</b> <br>",
            "<b>", data_dict %>% filter(var_name == "pwsid") %>% pull(clean_name), ": </b> ", Data$pwsid, " <br>",
            "<b>", data_dict %>% filter(var_name == "estimate_total_pop") %>% pull(clean_name), ": </b> ", scales::number(round(Data$estimate_total_pop, 0), big.mark = ","), " <br>",
            "<b>", data_dict %>% filter(var_name == "open_health_viol") %>% pull(clean_name), ": </b> ", Data$open_health_viol, "<br>",
            "<b>", data_dict %>% filter(var_name == !!(input$VarOne)) %>% pull(clean_name), ": </b> ", number(round(Data[[input$VarOne]], 2), big.mark = ","), " <br>",
            "<b>", data_dict %>% filter(var_name == !!(input$VarTwo)) %>% pull(clean_name), ": </b> ", number(round(Data[[input$VarTwo]], 2), big.mark = ","), " <br>"
          )
          Data$label_text <- lapply(label_text, htmltools::HTML)
          
          ## setting up leafletproxy 
          leaflet_proxy <- leafletProxy("Map") %>%
            clearGroup("Sabs") %>%
            clearControls()
          
## 8. Bivariate mapping leaflet proxy
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
                fillOpacity = 0.9,
                color = "black",
                paletteFunction = pals::tolochko.redblue,
                group = "Sabs") %>%
             addPolygons(data = Data,
                label = ~label_text,
                fillOpacity = 0,
                opacity = 0,
                group = "Sabs")
  ## 9. Single variate mapping leaflet proxy
          } else {
            # creating color pallete 
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
    }
  )

  
  ################################# 
  # Observe for panel show/hides ##
  #################################
  
  # Create a reactive timer that fires every 5 minutes
  timer <- reactiveTimer(5 * 60 * 1000)
  
  # Function to print something every 5 minutes
  printEveryFiveMinutes <- function() {
    print("Running....")
    # Add your desired action here
  }
  
  # Observer that executes the action every time the timer fires
  observe({
    timer()
    printEveryFiveMinutes()
  })

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
    leaflet(options = leafletOptions(minZoom = 6))%>%
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
      selectizeInput("Geography","", choices = GeoChoices, selected = c("I - East Texas", "D - North East Texas"), multiple = TRUE, options = list(maxItems = 5)),
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
  
  # Render checkbox inputs for categorical variables
  output$SelectCat <- renderUI({
    # Get variable names of categorical columns
    inputIds <- cat_dict$var_name
    
    # Generate checkbox inputs
    checkbox_inputs <- mapply(function(col, lab, inputId) {
      choices <- sort(na.omit(unique(Controller$data[[col]])))
      selected <- unique(Controller$data[[col]])
      # Generate checkbox input and layout
      div(class = "col-md-6", generateCheckboxGroupInput(inputId, lab, choices, selected))
    }, cat_columns, cat_labels, inputIds, SIMPLIFY = FALSE)
    
    # Organize checkbox inputs with a filter button
    tagList(fluidRow(do.call(tagList, checkbox_inputs)), actionButton("FilterCats", "Filter"))
  })
  
  output$SummaryStats <- renderUI({
    req(Controller$data_select)
    
    HB <- Controller$data_select %>%
          filter(open_health_viol == "Yes")%>%
          pull(open_health_viol)%>%
          length()
    
    UtilityCount <- paste("<i>", "Utility Count: </i> <b>", scales::number(length(unique(Controller$data_select$pwsid)), big.mark = ","),"</b>", "<br>")
    Population <- paste("<i>", "Utility Users: </i> <b>", scales::number(sum(Controller$data_select$estimate_total_pop, na.rm = TRUE), big.mark = ","),"</b>", "<br>")
    MHI <- paste("<i> ", "Avg. Median Household Income: </i> <b>", dollar(mean(Controller$data_select$estimate_mhi, na.rm = TRUE)),"</b>", "<br>")
    HB <- paste("<i>", "Open Health Violations: </i> <b>", HB ,"</b>", "<br>")
    
    tagList(
      HTML(paste("<b> Summary Statistics: </b> <br>")),
      HTML(UtilityCount),
      HTML(Population), 
      HTML(HB),
      HTML(MHI)
    )
  })
  
  ## Variable One Select
  output$VarOne <- renderUI({
    tagList(
      div(style = "display:flex; align-items: center; margin-top: -10px; margin-bottom: -3px",
          tipify(el = icon(name = "clone", lib = "font-awesome", style = "font-size: 17px; margin-right: 5px;"), placement = "right",
                 title = HTML("Map your primary variable against your secondary variable. As you change data, map will recolor")),
          HTML(paste("<b> Bivariate Mapping: </b>")),
          HTML("&nbsp;"), # Adding a non-breaking space for spacing
          div(style = "margin-bottom: -9px; margin-right: -6px;",
              checkboxInput("Bivariate", "", value = FALSE))
      ),
      tipify(el = icon(name = "chart-column", lib = "font-awesome", style = "color: #7ab3d1; font-size: 17px"), placement = "right", 
             title = HTML("Select a variable to modify the map and histogram below. You can define the variable range by clicking and dragging the histogram. As you change data, map will recolor. Double click to reset.")),
      HTML(paste("<b> Primary Variable: </b>")),
      selectizeInput("VarOne", "", choices = cont_choices, selected = "estimate_mhi", multiple = FALSE)
    )
  })
  
  ## Variable Two Select
  output$VarTwo <- renderUI({
    tagList(
      tipify(el = icon(name = "chart-column", lib = "font-awesome", style = "color: #dd7c8a; font-size: 17px;"), placement = "right",
             title = HTML("Select a variable to modify the utilities mapped and the histogram below. Click Bivariate to map this variable with your primary variable. You can define the variable range by clicking and dragging the histogram. As you change data, map will recolor. Double click to reset.")),
      HTML(paste("<b> Secondary Variable: </b>")),
      selectInput("VarTwo", "", choices = cont_choices, selected = "healthbased_violations_5yr", multiple = FALSE)
    )
  })
  
  ## Variable One Min Max
  output$VarOneMinMax <- renderUI({
    req(Controller$data_select)
    event_one_max <- if (!is.null(event_one())) max(event_one()$x, na.rm = TRUE) else max(Controller$data_select %>% pull(!!input$VarOne), na.rm = TRUE)
    event_one_min <- if (!is.null(event_one())) min(event_one()$x, na.rm = TRUE) else min(Controller$data_select%>% pull(!!input$VarOne), na.rm = TRUE)
    
    min <- paste(" <i> Min: </i> <b>",scales::number(round(event_one_min,4),big.mark = ","), "</b>")
    max <- paste(" <i> Max: </i> <b>",scales::number(round(event_one_max,2),big.mark = ","), "</b>")
    
    tagList(
        HTML(min),
        HTML(max),
      )

  })
  
  ## Variable Two Min Max
  output$VarTwoMinMax <- renderUI({
    req(Controller$data_select)
    event_two_max <- if (!is.null(event_two())) max(event_two()$x, na.rm = TRUE) else max(Controller$data_select %>% pull(!!input$VarTwo), na.rm = TRUE)
    event_two_min <- if (!is.null(event_two())) min(event_two()$x, na.rm = TRUE) else min(Controller$data_select%>% pull(!!input$VarTwo), na.rm = TRUE)
    
    min <- paste(" <i> Min: </i> <b>",scales::number(round(event_two_min,2),big.mark = ","), "</b>")
    max <- paste(" <i> Max: </i> <b>",scales::number(round(event_two_max,2),big.mark = ","), "</b>")
    
    tagList(
      HTML(min),
      HTML(max))
  })
  
  # Variable One Histogram
  output$VarOneHist <- renderPlotly({
    req(Controller$data_select)
    req(input$VarOne)
    
    plot_ly(x = Controller$data_select %>% pull(!!input$VarOne), 
            type = "histogram", source = "a", 
            nbinsx = 50, 
            marker = list(color = "#7ab3d1", line = list(color = "grey", width = 1)))%>% 
            config(displayModeBar = FALSE) %>%
            event_register("plotly_selected")%>%
            layout(yaxis = list(
                   title = paste('<i> Utilites </i> '),
                   titlefont = list(size = 12),
                   tickfont = list(size = 10)),
                   dragmode = "select",
                   margin = list(l = 5, r = 5, b = 5, t = 5, pad = 5))
  })
  
  # Variable Two Histogram
  output$VarTwoHist <- renderPlotly({
    req(Controller$data_select)
    req(input$VarTwo)
    
    plot_ly(x =  Controller$data_select %>% pull(!!input$VarTwo), 
            type = "histogram", 
            source = "b", 
            nbinsx = 50, 
            marker = list(color = "#dd7c8a", line = list(color = "grey", width = 1)))%>% 
            config(displayModeBar = FALSE) %>%
            event_register("plotly_selected")%>%
            layout(yaxis = list(
                   title = paste('<i> Utilites </i> '),
                   titlefont = list(size = 12),
                   tickfont = list(size = 10)),
                   dragmode = "select",
                   margin = list(l = 5, r = 5, b = 5, t = 5, pad = 5))
  })
  
  
  ################
  #### Table #####
  ################
  
  ## Helper Table Text
  output$TableText <- renderText({
    paste("Click a column to sort and slide a column to expand")
  })
  
  ## Table Rendering
  output$Table <- renderUI({
    req(Controller$data_select)

    # grabbing data for the table: 
    TableData <-  Controller$data_select %>%
      data.frame()%>%
      select(-c(geometry)) %>%
      mutate_if(is.numeric, round, digits = 2) %>%
      relocate(pws_name)%>%
      mutate(pws_name = str_to_title(pws_name))

    # changing column names to human-readable - with the help of this
    # post: https://stackoverflow.com/questions/59314285/selectively-rename-r-data-frame-column-names-using-a-key-value-pair-dictionary
    colnames(TableData) <- dplyr::recode(
      colnames(TableData), 
      !!!setNames(as.character(data_dict$clean_name), data_dict$var_name)
    )
    
    # grabbing column groups for the table: 
    colgroups <- data_dict %>% 
      filter(clean_name %in% names(TableData)) %>%
      group_by(category) %>%
      reframe(clean_name) 
    
    # rendering reactable: 
    renderReactable({
      reactable(TableData,
                columnGroups = list(
                  colGroup(name = "Water Delivery System", columns = colgroups[colgroups$category == "Water Delivery System",]$clean_name), 
                  colGroup(name = "Calculated", columns = colgroups[colgroups$category == "Calculated",]$clean_name), 
                  colGroup(name = "Socioeconomic", columns = colgroups[colgroups$category == "Socioeconomic",]$clean_name),
                  colGroup(name = "Financial", columns = colgroups[colgroups$category == "Financial",]$clean_name),
                  colGroup(name = "Environmental Justice Indicators", columns = colgroups[colgroups$category == "Environmental Justice Indicators",]$clean_name)),
                highlight = TRUE,
                defaultColDef = colDef(minWidth = 150), 
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
  
  ################
  ## Info Modal ##
  ################
  InfoModal <- modalDialog(
    title = HTML("<b> Texas Community Water System Prioritization Tool - Version 1.2 </b>"),
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
    HTML("&emsp;"),
    HTML("&emsp;"),
    HTML("- Adjust data ranges by click and dragging the histogram - double click to reset."),
    HTML("<br>"),
    HTML("&emsp;"),
    HTML("&emsp;"),
    HTML("- Map recolors when using the histogram or changing primary variable."),
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
    HTML("This application was developed to assist in prioritizing advocacy and technical assistance for community water systems in Texas. Known generally as a screening tool, the data and insights generated from this tool are to be taken in conjunction with research and local knowledge to inform outreach and not a sole source of information. 
         This tool can be used to identify utilities based on a user-determined set of characteristics. Keep in mind, these data are a small component of utility operations and drinking water user experience. Generally speaking, utilities are working to balance quality water, low rates, and financial stability, all while staying within regulatory compliance. 
         This balancing act can be difficult for under-resourced utilities."),
    HTML("<br>"),
    HTML("<br>"),
    HTML("<b> More Information and Feedback: </b>"),
    HTML("<li> Got feedback? Take our"),
    tags$a(href=paste("https://forms.gle/Xjbeur68qukaRmFo7"), "survey",  target="_blank"),
    HTML("and visit our"),
    tags$a(href=paste("https://docs.google.com/document/d/1MvfLFHDhTKoyLuk-cEPwFj8LPZTtdzPLBrkbhbuU38Y/edit"), "public log!",  target="_blank"),
    HTML("<br>"),
    HTML("<li> To learn more about how to use this tool, visit our"),
    tags$a(href=paste("https://docs.google.com/document/d/1iYJihLxewJy6s818eXE_ktmb-rtBa1aSBC2SYauub6I/edit"), "getting started",  target="_blank"),
    HTML("and"),
    tags$a(href=paste("https://docs.google.com/document/d/1AoiQePqMsgzsQ2I3BdmcNBsYZRkJ6mnZZcQNalFBpII/edit"), "advanced",  target="_blank"),
    HTML("vignettes."),
    HTML("<li> For documentation, methods, and reproducing this application, see our"),
    tags$a(href=paste("https://github.com/Environmental-Policy-Innovation-Center/tx-dw-tool"), "Github",  target="_blank"),
    HTML("foundations by"), tags$a(href=paste("https://policyinnovation.org"), "Environmental Policy Innovation Center (EPIC). ",  target="_blank"), 
    HTML("EPIC makes no assurances to the accuracy of the tools data. All underlying code, methods, and data are available under a Creative Commons License."),
    HTML("<br>"),
    HTML("<br>"),
    HTML("<i>", "Last updated: ", "July 16, 2024", "</i>"),

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
  # Helper function for report data download 
  reportGenerator <- function(data, var_one, var_two)
  {
    temp_dir <- tempdir()
    owd <- setwd(temp_dir)
    on.exit(setwd(owd))
    
    temp_rmd <- file.path(tempdir(), "tx-report-prod.Rmd")
    
    writeLines(report, temp_rmd)
    params <- list(data_p = data,
                   var_one = var_one,
                   var_two = var_two)
    
    out <- rmarkdown::render("tx-report-prod.Rmd",
                             params = params,
                             envir = new.env(parent = globalenv()))
    return(out)
  }
  
  ## Report download handler 
  output$Report <- downloadHandler(
    Sys.sleep(1),
    
    filename = "Report.html",
    content = function(file_n) {
      Sys.sleep(4)

      withProgress(message = 'Rendering, please wait!', {
        ## prints to ensure data is available (issue with downloading not working on first go)
        print(nrow(Controller$data_select))
        print(input$VarOne)
        print(input$VarTwo)

        file.rename(reportGenerator(Controller$data_select,input$VarOne,input$VarTwo), file_n)

      })
    })
  
  # download handler: 
  output$downloadData <- downloadHandler(
    
    filename = "tx-dw-app.zip",
    
    content = function(file) {
      ## prints to ensure data is available (issue with downloading not working on first go)
      Sys.sleep(4)
      print(nrow(Controller$data_select))
      
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
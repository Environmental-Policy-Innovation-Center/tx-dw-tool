# Interactive Dashboard for Drinking Water Funding 
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
library(tinytex)
library(reactablefmtr)
library(googlesheets4)
library(reactable.extras)
library(openxlsx)

## TICKET LIST
## Fixes 
##    xPull down no data handling 
##    xNo data selected during histogram selection 
##    Map flashing on render
##    xFix same variable clashing 
##    xMake utility type filter cumulative
## Enhancements 
##    Popups 
##    xRegion filters
##    App ready variable names
##    variable type groupings for pulldowns 
##    Documentation workflow 
##    xTable 
##    Report 

###########
### UI #### 
###########
ui <- fluidPage(

  reactable_extras_dependency(),
  add_busy_spinner(spin = "fading-circle", position = "top-left"),
  useShinyjs(),
  
  sidebarLayout(
    div(
      id ="sidebar",
      sidebarPanel(
        style = "position: fixed; height: 100%; width: 500px; overflow-y: auto; margin-left: -30px;", 
        div(style = "display:inline-block; float:right; margin-bottom: 20px"),
        width = 4,
        uiOutput("SelectGeography", style = "width: 100%"), 
        bsCollapse(
          id = "CollapsePanel", 
     #     open = c("Filter by Categories"), 
          multiple = TRUE,
          bsCollapsePanel(
            "Filter by Categories",
            uiOutput("SelectCat", style = "line-height: 20px; margin-top: -10px; margin-bottom: -10px;"),  
            style = "primary"
          )
        ),
        uiOutput("SummaryStats", style = "margin-bottom: 10px"),
        uiOutput("VarOne", style = "width: 100%"), 
        plotlyOutput("VarOneHist", width = "300px", height = "150px"),
        uiOutput("VarTwo", style = "width: 100%"), 
        plotlyOutput("VarTwoHist", width = "300px", height = "150px"),
        actionButton("Context", "Table or Map",icon(name = "arrows-left-right", lib = "font-awesome"),
                     ## ET added margin-top v 
                     style = "margin-bottom: 10px; ; margin-top: 10px;"), 
     downloadButton("Report", "Generate report",
                    icon = icon("file-arrow-down", lib = "font-awesome"),
                    style = "margin-bottom: 10px; margin-left: 10px; margin-top: 10px;"),
     radioButtons("downloadType", "Download Type", 
                  choices = c("CSV" = ".csv",
                              "GEOJSON" = ".geojson",
                              "EXCEL" = ".xlsx"),
                  inline = TRUE),
     downloadButton("downloadData", "Download data"),
      )
    ),
    mainPanel(
      style = "margin-left: -15px;",
      leafletOutput("Map", height = "100vh"),
      uiOutput("TableText", style = "font-size: 15px; margin-left: 5px; position:relative; z-index: 500; font-style: italic; margin-top: 5px;"),
      reactable_extras_ui("table"),
      width = 8
    ),
    position = c("right"), 
    fluid = TRUE
  )
)



################
#### SERVER #### 
################
server <- function(input, output) {
  
#################### 
### Data Import #### 
#################### 
## TO DO: Move this to the .RMD and return 1 object with 2 dataframes 
tx_raw <- aws.s3::s3read_using(st_read, 
                         object = "state-drinking-water/TX/clean/app/app_test_data_simplified.geojson",
                         bucket = "tech-team-data")
### ET Added V
gs4_deauth()
URL <- "https://docs.google.com/spreadsheets/d/1bzNPxhL-l6DeGElhG1c70Of8DGAQasMDUuX3rPHVe2A/edit#gid=0"
data_dict <- read_sheet(URL, sheet = "var_names")


### ET Added ^

################
### Variables ##
################
## Data Dictionary 
data_dict <- data_dict %>%
  filter(var_name %in% colnames(tx_raw))

#Main data 
Controller <- reactiveValues()
Controller$data <- tx_raw 

# List of counties
Counties <- unique(tx_raw$county_served)

## Generating unique list of regions
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

print(cat_columns)
print(cat_labels)

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
observeEvent(ignoreInit = TRUE, 
             list(input$Bivariate, 
                  # this isolate prevents re rendering loop 
                  isolate(Controller$data_select), 
                  input$VarOne, 
                  input$VarTwo, 
                  input$Geography, 
                  input$Catagory, 
                  event_one(), 
                  event_two(),
                  input$owner_type_description, input$primary_source_code, input$pop_catagories, input$tier), {
                    start <- Sys.time()
## show spinner                                        
show_spinner() 

if(length(input$Geography) == 0 | input$VarOne == input$VarTwo) {
  showNotification(id = "notification", "Insufficient data selected or duplicate variables chosen, please change selection", type = "warning")
} else {
  ## Filter data based on selected geography
  if(!str_detect(paste(input$Geography, collapse = "|"), "All")) {
    Data <- Controller$data %>%
      filter(county_served %in% input$Geography | regions %in% input$Geography)
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

  ## Filter data based on type selection
  if (!is.null(input$owner_type_description)) {
    for (col in cat_columns) {
      checkboxSelection[[col]] <- input[[col]]
    }
    for (col in cat_columns) {
      selectedChoices <- checkboxSelection[[col]]
      if (length(selectedChoices) > 0) {
        Data <- Data[Data[[col]] %in% selectedChoices, ]
      }
    }
  }

  if(nrow(Data) < 3) {
    showNotification(id = "notification", "Insufficient data, please select more utilities", type = "warning")
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
      clearShapes() %>%
      clearControls()
    
    if (input$Bivariate == TRUE) {
      ## Bivariate plot
      leaflet_proxy %>%
        bivariatechoropleths::addBivariateChoropleth(
          map_data = Data,
          var1_name = input$VarTwo,
          var2_name = input$VarOne,
          ntiles = 3,
          var1_label = data_dict %>% filter(var_name == !!(input$VarTwo)) %>% pull(clean_name),
          var2_label = data_dict %>% filter(var_name == !!(input$VarOne)) %>% pull(clean_name),
          weight = 1,
          fillOpacity = 0.7,
          color = "black",
          paletteFunction = pals::tolochko.redblue
        ) %>%
        addPolygons(data = Data,
                    label = ~label_text,
                    fillOpacity = 0,
                    opacity = 0)
    } else {
      ## Univariate plot
      colvar <- Data[[input$VarOne]]
      pal <- colorNumeric(
        palette = c("#f5f5f5", "#dd7c8a", "#cc0124"),
        domain = colvar
      )
      leaflet_proxy %>%
        addPolygons(data = Data,
                    layerId = ~pwsid,
                    label = ~label_text,
                    fillColor = ~pal(colvar),
                    fillOpacity = .9,
                    weight = .75,
                    color = "grey") %>%
        addLegend(pal = pal, values = colvar, position = "bottomleft", title = data_dict %>% filter(var_name == !!(input$VarOne)) %>% pull(clean_name))
    }
  }
}
#print(Sys.time() -start )
Sys.sleep(.05) 
hide_spinner()
})

################################# 
# Observe for panel show/hides ##
#################################

observeEvent(input$Context, {
  toggle("Map", anim = FALSE,animType = "slide")
})


################
#### Map #######
################
## TO DO 
## Add UpdateLeaflet for data_select
output$Map <- renderLeaflet({
  leaflet(options = leafletOptions(minZoom = 1, maxZoom = 12))%>%
              addProviderTiles(providers$CartoDB.Positron, group = "Toner Lite")%>%
              setView(-95.58292, 31.94214, zoom = 7)
})



################
#### Sidebar ###
################

## Geography Filter ##
## To DO: 
## Add regions (.rmd)
## Add missing counties (.rmd)
output$SelectGeography <- renderUI({
  GeoChoices <- list()
  
  GeoChoices$Regions <- sort(unique_regions)
  GeoChoices$Counties <- sort(Counties)
  
selectizeInput("Geography","Select a Geography", choices = GeoChoices, selected = "I - East Texas", multiple = TRUE, options = list(maxItems = 5))
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
  
  fluidRow(do.call(tagList, checkbox_inputs))
})


output$SummaryStats <- renderUI({
  req(Controller$data_select)
  ## Number of selected utilities
  ## Total population served
  ## Median Household Income
  ## Percent of Color 
  
  UtilityCount <- paste("<b>", "Utility Count:", scales::number(length(unique(Controller$data_select$pwsid)), big.mark = ","),"</b>", "<br>")
  Population <- paste("<b>", "Utility Users:", scales::number(sum(Controller$data_select$estimate_total_pop), big.mark = ","),"</b>", "<br>")
  MHI <- paste("<b> ", "Avg. Median Household Income:", dollar(mean(Controller$data_select$estimate_mhi, na.rm = TRUE)),"</b>", "<br>")
  POC <- paste("<b>", "Percent of Color:", scales::percent(mean(Controller$data_select$estimate_poc_alone_per, na.rm = TRUE) / 100) ,"</b>", "<br>")
  
  tagList(
    HTML(UtilityCount),
    HTML(Population), 
    HTML(POC),
    HTML(MHI)
  )
  
})

## Variable One Select
output$VarOne <- renderUI({
selectizeInput("VarOne", "Select a variable to map:", choices = cont_choices, selected = "estimate_mhi", multiple = FALSE)

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
  
plot_ly(x =  Controller$data_select %>% pull(!!input$VarOne), type = "histogram", source = "a", nbinsx = 50, marker = list(color = "#dd7c8a") )%>% 
  config(displayModeBar = FALSE) %>%
  event_register("plotly_selected")%>%
 # add_trace(x = density$x, y = density$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density") %>% 
  layout( dragmode = "select", margin = m)
})

## Variable Two Select
output$VarTwo <- renderUI({
  tagList(
  selectInput("VarTwo", "Select a second variable:", choices = cont_choices, selected = "healthbased_violations_5yr", multiple = FALSE),
  checkboxInput("Bivariate", "Bivariate",value = FALSE)
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
  
  plot_ly(x =  Controller$data_select %>% pull(!!input$VarTwo), type = "histogram", source = "b", nbinsx = 50, marker = list(color = "#7ab3d1") )%>% 
    config(displayModeBar = FALSE) %>%
    event_register("plotly_selected")%>%
    layout(dragmode = "select",  margin = m)
})

################
#### Table #####
################
## ET ADDED v##
output$TableText <- renderText({
  paste("Click a column to sort and slide a column to expand")
})

### TABLE ###
observeEvent(Controller$data_select,ignoreInit = TRUE,{
  
  TableData <- Controller$data_select %>%
    data.frame()%>%
    select(-c(geometry)) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
    select(-c("tier", "east_tx_flag"))

  style_viols <- data_bars(
    data = TableData,
    round_edges = TRUE,
    fill_color = viridis::inferno(40),
    fill_opacity = 0.8, 
    text_position = "outside-base"
  )
  
  # percent cell styling: 
  style_pct <- data_bars(
    data = TableData,
    round_edges = TRUE,
    viridis::inferno(40),
    fill_opacity = 0.8, 
    max_value = 100,
    text_position = "outside-base"
  )
  
  # count cell styling: 
  style_count <- color_tiles(
    TableData,
    colors = viridis::mako(40),
    number_fmt = scales::comma,
    opacity = 0.5
  )
  
reactable_extras_server(data = TableData , id = "table", total_pages = round(nrow(TableData)/15,0) + 1,
                        columns = list(
                          # utility characteristics: 
                          pwsid = colDef(aggregate = "unique", 
                                         name = "ID"),
                          # east_tx_flag = colDef(aggregate = "unique"),
                          county_served = colDef(aggregate = "unique", 
                                                 name = "County"),
                          # tier = colDef(aggregate = "unique"),
                          regions = colDef(aggregate = "unique", 
                                           name = "Region"),
                          primary_source_code = colDef(aggregate = "unique", 
                                                       name = "Source"),
                          owner_type_description = colDef(aggregate = "unique", 
                                                          name = "Owner"),
                          pop_catagories = colDef(aggregate = "unique", 
                                                  name = "Pop Cat"),
                          pop_density = colDef(name = "Pop Density", 
                                               cell = style_count),
                          area_miles = colDef(name = "Area (mi)",
                                              cell = style_count),
                          # socioeconomic: 
                          estimate_mhi = colDef(name = "MHI ($)",
                                                cell = color_tiles(
                                                  TableData,
                                                  colors = viridis::mako(40),
                                                  number_fmt = scales::dollar,
                                                  opacity = 0.5
                                                )),
                          estimate_total_pop = colDef(name = "Population",
                                                      cell = style_count),
                          estimate_hisp_alone_per = colDef(name = "% Latino/a",
                                                           cell = style_pct),
                          estimate_laborforce_unemployed_per = colDef(name = "% Unemployment", 
                                                                      cell = style_pct),
                          estimate_hh_below_pov_per = colDef(name = "% Poverty",
                                                             cell = style_pct),
                          estimate_poc_alone_per = colDef(name = "%POC", 
                                                          cell = style_pct),
                          # violations:
                          paperwork_violations_10yr = colDef(name = "Non-Health, 10yr",
                                                             cell = style_viols),
                          healthbased_violations_10yr = colDef(name = "Health, 10yr",
                                                               cell = style_viols),
                          total_violations_10yr = colDef(name = "Total, 10yr",
                                                         cell = style_viols),
                          paperwork_violations_5yr = colDef(name = "Non-Health, 5yr",
                                                            cell = style_viols),
                          healthbased_violations_5yr = colDef(name = "Health, 5yr",
                                                              cell = style_viols),
                          total_violations_5yr = colDef(name = "Total, 5yr",
                                                        cell = style_viols)
                        ),
                        # defining column groups: 
                        columnGroups = list(
                          colGroup(name = "Utility", columns = c("pwsid", 
                                                                 # "east_tx_flag",  
                                                                 "county_served",  
                                                                 # "tier", 
                                                                 "regions",
                                                                 "primary_source_code", 
                                                                 "owner_type_description", "pop_catagories",
                                                                 "pop_density", "area_miles")),
                          colGroup(name = "Socioeconomic", columns = c("estimate_mhi", "estimate_total_pop", 
                                                                       "estimate_hisp_alone_per", "estimate_laborforce_unemployed_per", 
                                                                       "estimate_hh_below_pov_per", "estimate_poc_alone_per")), 
                          colGroup(name = "Violations", columns = c("healthbased_violations_5yr",
                                                                    "healthbased_violations_10yr", 
                                                                    "paperwork_violations_5yr",
                                                                    "paperwork_violations_10yr", 
                                                                    "total_violations_5yr", 
                                                                    "total_violations_10yr"))
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

################
#### Charts ####
################

################
#### Report ####
################

# code for generating report: 
# NOTE: for shinyapps.io, this needs to be published with the application,
output$Report <- downloadHandler(
  filename = "Report.html",
  content = function(file_n) {
    withProgress(message = 'Rendering, please wait!', {
      src <- normalizePath("tx-report.Rmd")
      temp_dir <- tempdir()
      owd <- setwd(temp_dir)
      on.exit(setwd(owd))
      file.copy(src, "tx-report.Rmd", overwrite = TRUE)
      params <- list(data_p = Controller$data_select)
      out <- rmarkdown::render("tx-report.Rmd",
                               params = params,
                               envir = new.env(parent = globalenv()))
      file.rename(out, file_n)
      })
  })

# download handler: 
output$downloadData <- downloadHandler(
  filename = function() {
    paste0("data", input$downloadType)
  },
  content = function(file) {
    data_not_sf <- Controller$data_select %>% 
      as.data.frame() %>%
      select(-"geometry")
    if(input$downloadType == ".csv") {
      write.csv(data_not_sf, 
                file, row.names = FALSE)
    } else if(input$downloadType == ".geojson") {
      st_write(Controller$data_select, file)
    }
    else if(input$downloadType == ".xlsx") {
      write.xlsx(data_not_sf, file)
    }
  })  
}

# Run the application 
shinyApp(ui = ui, server = server)

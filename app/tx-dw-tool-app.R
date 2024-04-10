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
###########
### UI #### 
###########
ui <- fluidPage(
  # detect(),
  # useWaitress(),
  # useShinyjs(),
  # tags$head(
  #   tags$style(HTML(".leaflet-container { background: #FFFFFF;} 
  #                   .sidebar form.well { background: transparent;border: 0px;} 
  #                   .panel-primary>.panel-heading+.panel-collapse>.panel-body{border-right: 1px solid rgba(0, 0, 0, 0.05);}
  #                   .shiny-notification {position:fixed;top: calc(15%);left: calc(15%); max-width: 300px}"))
  # ),
  # 
  sidebarLayout(
    div( id ="sidebar",
         sidebarPanel(
           style = "position: fixed; height: 82%; overflow-y: auto; margin-left: -30px;", div(style = "display:inline-block; float:right; margin-bottom: 20px"),
           width = 4,
           uiOutput("SelectGeography", style = "width: 100%"), 
           uiOutput("VarOne", style = "width: 100%"), 
         )),
           mainPanel(
             style = "margin-left: -15px;",
             leafletOutput("Map", height = "100vh"),
             width = 8),
           
           position = c("right"), fluid = TRUE),
  
)

################
#### SERVER #### 
################
server <- function(input, output) {
  
#################### 
### Data Import #### 
#################### 
tx_raw <- aws.s3::s3read_using(st_read, 
                         object = "state-drinking-water/TX/clean/app/app_test_data_simplified.geojson",
                         bucket = "tech-team-data")

################
### Variables ##
################
Controller <- reactiveValues()

Controller$data <- tx_raw 

Counties <- unique(tx_raw$county_served)

Controller$data_select <- isolate(Controller$data)

################
### Observes ###
################

## Responds to changes in 'Geography' selection
## TO DO: Fix Flashing (ONLY remove pwsids that are present currently, but not in new Controller$data_select)
observeEvent(input$Geography,{
  
removepwsid <- Controller$data_select$pwsid

Controller$data_select <- Controller$data  %>%
                          filter(county_served %in% input$Geography)

colvar <-  Controller$data_select %>% pull(!!input$VarOne)

pal <- colorNumeric(
  palette = "Blues",
  domain = colvar)

leafletProxy("Map")%>%
  removeShape(layerId = removepwsid)%>%
  addPolygons(data = Controller$data_select, 
              layerId = ~pwsid,
              label = ~htmlEscape(pwsid),
              color = ~pal(colvar),
              weight = 1.5,
              fill = "grey",
              opacity = .5)

}, ignoreInit = TRUE, ignoreNULL = TRUE)

## Responds to changes in Variable selection 
observeEvent(input$VarOne, {
  
  colvar <-  Controller$data_select %>% pull(!!input$VarOne)
  pal <- colorNumeric(
    palette = "Blues",
    domain = colvar)

  
  leafletProxy("Map")%>%
  clearShapes()%>%
  addPolygons(data = Controller$data_select,
              layerId = ~pwsid,
              label = ~htmlEscape(pwsid),
              color = ~pal(colvar),
              weight = 1.5,
              fill = "grey",
              opacity = .5)

  
}, ignoreInit = TRUE, ignoreNULL = TRUE)


################
#### Map #######
################
## TO DO 
## Add UpdateLeaflet for data_select
output$Map <- renderLeaflet({
  leaflet(options = leafletOptions(minZoom = 1, maxZoom = 10))%>%
              addProviderTiles(providers$CartoDB.Positron, group = "Toner Lite")%>%
              setView(-100.00, 31.0, zoom = 6)
})


################
#### Sidebar ###
################

## Geography Filter ##
## To DO: 
## Add regions (.rmd)
## Add missing counties (.rmd)
output$SelectGeography <- renderUI({
selectizeInput("Geography","Select a Geography", choices = Counties, selected = "ANGELINA" , multiple = TRUE)
})

## Variable Select
output$VarOne <- renderUI({
selectInput("VarOne", "Select a variable", choices = Controller$data %>% select(area_miles:total_violations_5yr) %>% colnames())
})

################
#### Table #####
################

################
#### Charts ####
################

################
#### Report ####
################
  
}




# Run the application 
shinyApp(ui = ui, server = server)

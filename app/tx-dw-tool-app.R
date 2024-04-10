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

Controller$data <- tx_raw %>%
                   data.frame()%>%
                   select(-c(geometry))

Controller$geo <-  tx_raw %>%
                   select(pwsid, geometry)


################
### Observes ###
################

################
#### Map #######
################

output$Map <- renderLeaflet({
  leaflet(options = leafletOptions(minZoom = 1, maxZoom = 10))%>%
              addPolygons(data = Controller$geo, 
                          layerId = ~pwsid,
                          label = ~htmlEscape(pwsid),
                          color = "black",
                          weight = 1.5,
                          fill = "grey",
                          opacity = .5)%>%
              addProviderTiles(providers$CartoDB.Positron, group = "Toner Lite")%>%
              setView(-95.5795, 36.8283, zoom = 4)
})


################
#### Sidebar ###
################

## Geography Filter ##
## To DO: 
## Add regions 
## Add missing counties 
output$SelectGeography <- renderUI({
selectizeInput("Geography","Select a Geography", choices = unique(Controller$data$county_served))
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

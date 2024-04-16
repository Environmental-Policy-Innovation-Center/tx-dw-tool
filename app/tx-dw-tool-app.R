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
###########
### UI #### 
###########
ui <- fluidPage(
  
  add_busy_spinner(spin = "fading-circle"),
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
           style = "position: fixed; height: 82%; width: 100%; overflow-y: auto; margin-left: -30px;", div(style = "display:inline-block; float:right; margin-bottom: 20px"),
           width = 4,
           htmlOutput("PlotlyData"),
           uiOutput("SelectGeography", style = "width: 100%"), 
           uiOutput("SelectCat",style = "width: 100%"),
           uiOutput("VarOne", style = "width: 100%"), 
           plotlyOutput("VarOneHist", width = "250px", height = "150px"),
           uiOutput("VarTwo", style = "width: 100%"), 
           plotlyOutput("VarTwoHist", width = "250px", height = "150px"),
           
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

event_one <- reactive(event_data(event = "plotly_selected", source = "a", priority = "event"))
event_two <- reactive(event_data(event = "plotly_selected", source = "b", priority = "event"))
################
### Observes ###
################

## Responds to changes in 'Geography' selection
## TO DO: Fix Flashing (ONLY remove pwsids that are present currently, but not in new Controller$data_select)

## Observe Event
observeEvent(ignoreInit = TRUE, 
             list(input$Bivariate, 
                  # this isolate prevents rerendering loop 
                  isolate(Controller$data_select), 
                  input$VarOne, 
                  input$VarTwo, 
                  input$Geography, input$Catagory, event_one(), event_two()), {
                    
## Remove pwsids not selected in new geo_select         
removepwsid <- Controller$data_select$pwsid

if(!str_detect(paste(input$Geography, collapse = "|"),"All"))
{
## selecting new geography data based on user input                
Controller$data_select <- Controller$data  %>%
                          filter(county_served %in% input$Geography)
}
else
{
  Controller$data_select <- Controller$data
}
 
## TO DO: Sort out logic here
## minimum data handling
## charts reflecting each other when shrinking data 
## event one handling
if(!is.null(event_one()))
{
  event_one_max <- max(event_one()$x, na.rm = TRUE)
  event_one_min <- min(event_one()$x, na.rm = TRUE)
}
else
{
  event_one_max <- max(Controller$data_select %>% pull(!!input$VarOne), na.rm = TRUE)
  event_one_min <- min(Controller$data_select %>% pull(!!input$VarOne), na.rm = TRUE)
}
## event two handling
if(!is.null(event_two()))
{
  event_two_max <- max(event_two()$x, na.rm = TRUE)
  event_two_min <- min(event_two()$x, na.rm = TRUE)
}
else
{
  event_two_max <- max(Controller$data_select %>% pull(!!input$VarTwo), na.rm = TRUE)
  event_two_min <- min(Controller$data_select %>% pull(!!input$VarTwo), na.rm = TRUE)
}

## selecting based on categorical filters
if(!str_detect(paste(input$Catagory, collapse = "|"),"All Types"))
{
  #print(input$Catagory)
  Data <- Controller$data_select %>%
    filter(!!as.symbol(input$VarOne) <= event_one_max)%>%
    filter(!!as.symbol(input$VarOne) >= event_one_min)%>%
    filter(!!as.symbol(input$VarTwo) <= event_two_max)%>%
    filter(!!as.symbol(input$VarTwo) >= event_two_min)%>%
    filter(if_any(where(is.character), ~str_detect(., paste(input$Catagory, collapse = "|"))))
}
else
{
Data <- Controller$data_select %>%
  filter(!!as.symbol(input$VarOne) <= event_one_max)%>%
  filter(!!as.symbol(input$VarOne) >= event_one_min)%>%
  filter(!!as.symbol(input$VarTwo) <= event_two_max)%>%
  filter(!!as.symbol(input$VarTwo) >= event_two_min) 
}

# sets the data back to Controller$data_selected for UI outputs and prevents rerendering within the massive observeEvent... 
isolate(Controller$data_select <- Data)

## If Bivariate is chosen - map color scale is different
## TO DO: can likely make this a lot shorter with passing the first leaflet changes to a var
if(input$Bivariate == TRUE)
{
  leafletProxy("Map")%>%
    clearShapes()%>%
   # removeShape(layerId = removepwsid)%>%
    clearControls()%>%
    addPolygons(data = Data,
                layerId = ~pwsid,
                label = ~htmlEscape(pwsid),
                weight = 1.5,
                fill = "grey",
                opacity = .5)%>%
    bivariatechoropleths::addBivariateChoropleth(
      map_data = Data,
      var1_name = input$VarOne,
      var2_name = input$VarTwo,
      ntiles= 3,
      var1_label = input$VarOne,
      var2_label = input$VarTwo,
      weight = 1,
      fillOpacity = 0.7,
      color = "grey")
}
else
{
  # subsetting to selected variable 
  colvar <-  Data %>% pull(!!input$VarOne)
  
  pal <- colorQuantile(
    palette = "Blues",
    domain = unique(colvar))
  
  leafletProxy("Map")%>%
    clearShapes()%>%
   # removeShape(layerId = removepwsid)%>%
    clearControls()%>%
    addPolygons(data = Data,
                layerId = ~pwsid,
                label = ~htmlEscape(pwsid),
                color = ~pal(colvar),
                weight = 1.5,
                fill = "grey",
                opacity = .75)%>%
    addLegend(pal = pal, values = colvar,  position = "bottomleft")
}
})
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
selectizeInput("Geography","Select a Geography", choices = c("All",Counties), selected = Counties[2:10] , multiple = TRUE)
})

output$SelectCat <- renderUI({
  ## Theres probably a way to do this with a function but curently need to copy and paste catagorical filter columns
  Choices <- list()
  Choices$all <- c("All Types","")
  Choices$owner_type_description <- unique(Controller$data$owner_type_description)
  Choices$primary_source_code <- unique(Controller$data$primary_source_code)
  Choices$pop_catagories <- unique(Controller$data$pop_catagories)
  Choices$tier <- unique(Controller$data$tier)

  selectizeInput("Catagory", "Select a Utility Type", choices = Choices, selected = Choices$all[1], multiple = TRUE)
})

## Variable One Select
output$VarOne <- renderUI({
selectInput("VarOne", "Select a variable", choices = Controller$data %>% select(estimate_mhi:total_violations_5yr) %>% colnames())
})

# Variable One Hist
output$VarOneHist <- renderPlotly({
  req(Controller$data_select)
  req(input$VarOne)
  
plot_ly(x =  Controller$data_select %>% pull(!!input$VarOne), type = "histogram", source = "a", nbinsx = 30)%>% 
  config(displayModeBar = FALSE) %>%
  event_register("plotly_selected")%>%
 # add_trace(x = density$x, y = density$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density") %>% 
  layout( dragmode = "select")
})

## Variable Two Select
output$VarTwo <- renderUI({
  tagList(
  selectInput("VarTwo", "Select a second variable", choices = Controller$data %>% select(pop_density:total_violations_5yr) %>% colnames() ),
  checkboxInput("Bivariate", "Bivariate",value = FALSE)
  )
})

# Variable Two Hist
output$VarTwoHist <- renderPlotly({
  req(Controller$data_select)
  req(input$VarTwo)
  
  plot_ly(x =  Controller$data_select %>% pull(!!input$VarTwo), type = "histogram", source = "b", nbinsx = 30)%>% 
    config(displayModeBar = FALSE) %>%
    event_register("plotly_selected")%>%
    # add_trace(x = density$x, y = density$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density") %>% 
    layout(dragmode = "select")
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

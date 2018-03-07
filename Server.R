library(shiny)
library(dplyr)
library(stringr)
library(leaflet)

data.911 <-read.csv("2017_Seattle_911_processed.csv", stringsAsFactors = FALSE)

# Seperate date and time in different columns
at.scene <- str_split_fixed(data.911$At.Scene.Time, " ", 2)
data.911 <- data.911 %>% 
  mutate(At.Scene.Date = as.Date(at.scene[, 1], "%m/%d/%Y"),
         At.Scene.Time = format(strptime(at.scene[, 2], "%I:%M:%S %p"), 
                                format="%H:%M"))

# Server
server <- function(input, output) {
  hourTime <- function(hour) {
    if(hour < 10) {
      return(paste0("0", hour))
    }
    return(hour)
  }

  # Filter data with selected date and time
  data <- reactive({
    data.911 <- data.911 %>% 
      filter(data.911$At.Scene.Date >= input$date[1] 
             & data.911$At.Scene.Date <= input$date[2]
             & data.911$At.Scene.Time >= paste0(hourTime(input$time[1]), ":00")
             & data.911$At.Scene.Time <= paste0(hourTime(input$time[2]), ":00"))
    return(data.911)
  })
  
   # Render description
   output$text <- renderText({
    description <- paste0("This map shows the 911 calls in Seattle during ", input$date[1],
                          " and ", input$date[2], " with at_scene time from ",
                          paste0(hourTime(input$time[1]), ":00"), " to ", 
                          paste0(hourTime(input$time[2]), ":00"), ". \n")
    return(description)
  })
  
  # Render map
  output$map <- renderLeaflet({
    map <- leaflet() %>%
      addTiles() %>%
      addCircleMarkers(lng=data()$Longitude, lat=data()$Latitude, radius = 5, 
                       color = "black", popup = data.911$Event.Clearance.Description,
                       clusterOptions = markerClusterOptions())
  })
}
shinyUI(server)
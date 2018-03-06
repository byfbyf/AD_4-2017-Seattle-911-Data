library(shiny)
library(dplyr)
library(stringr)
library(leaflet)

short <-read.csv("short.csv", stringsAsFactors = FALSE)

# Filter data
at.scene <- str_split_fixed(short$At.Scene.Time, " ", 2)

short <-  short %>% 
  mutate(At.Scene.Date = as.Date(at.scene[, 1], "%m/%d/%Y"),
         At.Scene.Time = at.scene[, 2])

# Server
server <- function(input, output) {
  hourTime <- function(hour) {
    if(hour < 10) {
      return(paste0("0", hour))
    }
    return(hour)
  }

  data <- reactive({
    short <- short %>% 
      filter(short$At.Scene.Date >= input$date[1] 
             & short$At.Scene.Date <= input$date[2]
             & short$At.Scene.Time >= paste0(hourTime(input$time[1]), ":00")
             & short$At.Scene.Time <= paste0(hourTime(input$time[2]), ":00"))
    return(short)
  })
  
   output$text <- renderText({
    paste(input$time[1],
          input$time[2],
          paste0(hourTime(input$time[1]), ":00"),
          paste0(hourTime(input$time[2]), ":00"),
    as.character(input$date[1]),
    as.character(input$date[2]), 
    nrow(data()))
  })
  
  output$map <- renderLeaflet({
    map <- leaflet() %>%
      addTiles() %>%
      addMarkers(lng=data()$Longitude, lat=data()$Latitude)
  })
}
shinyUI(server)
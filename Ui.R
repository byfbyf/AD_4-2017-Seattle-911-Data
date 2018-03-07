library(shiny)
library(leaflet)

ui <- fluidPage(
  h1("Seattle 911 Data of 2017"),
  
  tabsetPanel(
    tabPanel("Map",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput("date", "Date in 2017: ", 
                                start = "2017-1-1",
                                end = "2017-1-2",
                                min = "2017-1-1",
                                max = "2017-12-31",
                                format = "mm/dd/yy"),
                 sliderInput("time", "Time Range: ",
                             0, 24, c(0, 24), 1, 
                             round = FALSE)
               ),
               mainPanel(
                 h2("2017 Seattle 911 Events Map"),
                 textOutput("text"),
                 leafletOutput("map", width = 1000, height = 600)
               )
            )
            )
  )
)

shinyUI(ui)
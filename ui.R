library(shiny)

ui <- fluidPage(
  titlePanel("Seattle 911 Data of 2017"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "type", label = strong("Type"),
                  choices = c("All", "Other"),
                  selected = "All")
      ),
    mainPanel(
      textOutput("texts"),
      tableOutput("table")
    )
      
    )
  )

shinyUI(ui)
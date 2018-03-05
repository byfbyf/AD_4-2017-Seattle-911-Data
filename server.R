library(shiny)
source("Data.R")
  server <- function(input, output) {
    filtered <- reactive({
      result <- data.2017
      if(input$type != "All") {
        result <- select(by.types, input$type)
      }
      Filter.Month.Count.Perday <- function(mo) {
        data.month <- filter(result, Event.Clearance.Date >= month.start[mo] &
                               Event.Clearance.Date < month.start[mo + 1])
        return(1.0 *nrow(data.month)/as.integer(days_in_month(mo)))
      }
      Count.Per.Day <- sapply(1:12,Filter.Month.Count.Perday)
      df.part1 <- data.frame(month.name, Count.Per.Day,
                             stringsAsFactors = FALSE)
      output$table <- renderTable(df.part1)
      output$texts <- renderText("AAA")
      
    })
  }
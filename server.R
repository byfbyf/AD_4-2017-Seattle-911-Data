library(shiny)
library(dplyr)
library(ggplot2)
source("Data.R")

  server <- function(input, output) {
    #Intro
    output$intro <- renderUI({
      
      text <-paste0("<p>Our project is about the dataset of Seattle's 911
             data from year 2017. There are <b>", nrow(data.2017), "</b> calls
             in total. And the most common subject of the calls is <b>traffic 
             related calls</b>.</p> <p>In our project, we will have three sections:
            <li>Part 1 displays the average  daily call numbers regarding various topics 
            for different months in the year 2017. </li>
             <li> Part 2 display a map where officials responds
              to the call at secene using leaflet.</li>
             <li>Part 3 shows 
             type of calls in different areas.</li> </p>")
      HTML(text)
    })
    
    #Returns Month - Count datafram for selected type of call.
    month.data <- reactive({
      result <- data.2017
      if(input$type != "All") {
        result <- filter(data.2017, Event.Clearance.Group == input$type)
      }
      Filter.Month.Count.Perday <- function(mo) {
        data.month <- filter(result, Event.Clearance.Date >= month.start[mo] &
                               Event.Clearance.Date < month.start[mo + 1])
        return(1.0 *nrow(data.month)/as.integer(days_in_month(mo)))
      }
      Count.Per.Day <- sapply(1:12,Filter.Month.Count.Perday)
      df.part1 <- data.frame(month.name, Count.Per.Day,
                             stringsAsFactors = FALSE)
      
      
    })
    
    #Shows the average per day of the year and the maxium among the 12 month
    output$text1 <- renderText({
      month.data.type <- month.data()
      mean<-format(round(mean(month.data.type$Count.Per.Day), 2), nsmall = 2)
      
      max <- max(month.data.type$Count.Per.Day)
      max.month <- filter(month.data.type, Count.Per.Day == max)[1,1]
      paste0("During 2017, the average calls per day for ", input$type, " is ",
            mean, ".
            The maxium average calls per day is ", 
            format(round(max, 2), nsmall = 2), " on ",max.month, "." )
    })
    
    #Bar chart and line for the count of average daily calls
    output$plot1 <- renderPlot({
      month.avg.data <- month.data()
      month.avg.data$month.name <-factor(month.avg.data$month.name, 
                             levels = month.name)
      ggplot(data = month.avg.data, aes(month.name, Count.Per.Day, group = 1))+
        geom_col(fill = "#2b8cbe") + geom_line(color = "red", size=1.5) +
        ggtitle("Average Daily Call #") + 
        theme(plot.title = element_text(hjust = 0.5))+ 
        labs(x="Month", y="Average Daily Call")
      
    })

  }
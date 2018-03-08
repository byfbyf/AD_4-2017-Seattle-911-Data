library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
library(stringr)
library(lubridate)
library(plotly)
library(ggrepel)

data.2017 <-read.csv("./2017_Seattle_911_processed.csv", stringsAsFactors = FALSE)

data.2017$Event.Clearance.Date <-as_datetime(data.2017$Event.Clearance.Date)


data.2017$Event.Clearance.Date <-as.POSIXct(data.2017$Event.Clearance.Date)

month.start <- c(paste0("2017-", 1:12, "-01"), "2018-01-01")

at.scene <- str_split_fixed(data.2017$At.Scene.Time, " ", 2)

data.2017 <-  data.2017 %>%
  mutate(At.Scene.Date = as.Date(at.scene[, 1], "%m/%d/%Y"),
         At.Scene.Time = format(strptime(at.scene[, 2], "%I:%M:%S %p"), 
                                format="%H:%M"))
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
  
  hourTime <- function(hour) {
    if(hour < 10) {
      return(paste0("0", hour))
    }
    return(hour)
  }
  
  # Filter data with selected date and time
  data <- reactive({
    data.2017 <- data.2017 %>% 
      filter(data.2017$At.Scene.Date >= input$date[1] 
             & data.2017$At.Scene.Date <= input$date[2]
             & data.2017$At.Scene.Time >= paste0(hourTime(input$time[1]), ":00")
             & data.2017$At.Scene.Time <= paste0(hourTime(input$time[2]), ":00"))
    return(data.2017)
  })
  
  # Render description
  output$text <- renderText({
    description <- paste0("This map shows officers at sencen for the 911 calls
                          in Seattle during ", input$date[1],
                          " and ", input$date[2], " with the officer at scene
                          time from ",
                          paste0(hourTime(input$time[1]), ":00"), " to ", 
                          paste0(hourTime(input$time[2]), ":00"), ". \n")
    return(description)
  })
  
  # Render map
  output$map <- renderLeaflet({
    map <- leaflet() %>%
      addTiles() %>%
      addCircleMarkers(lng=data()$Longitude, lat=data()$Latitude, radius = 5, 
                       color = "black", popup = data()$Event.Clearance.Description,
                       clusterOptions = markerClusterOptions())
  })
  
  
  # select data in a specific area
  location <- reactive ({
    loc <- filter(data.2017, District.Sector == input$area) %>%
      group_by(Event.Clearance.Group) %>%
      summarize(frequency = n()) %>%
      arrange(-frequency) %>%
      mutate(percentage = round(frequency / sum(frequency) * 100, 2))
    other <- filter(loc, loc$percentage < 5.00) 
    loc <- filter(loc,loc$percentage >= 5.00)
    nr <- nrow(loc) +1
    loc[nr, 'Event.Clearance.Group'] <- 'Other'
    loc[nr, 'frequency'] <- sum(other$frequency)
    loc[nr, 'percentage'] <- sum(other$percentage)
    # loc[nr + 1, 'Event.Clearance.Group'] <- 'Total'
    # loc[nr + 1, 'frequency'] <- total
    # loc[nr + 1, 'percentage'] <- '100%'
    return(loc)
    
  })
  
  #render table of information
  output$info <- renderTable ({
    location()
  })
  
  output$total <- renderPrint ({
    t <- filter(data.2017, District.Sector == input$area) %>%
      group_by(Event.Clearance.Group) %>%
      summarize(frequency = n())
    t.s <- sum(t$frequency)
    print(paste('Total: ', t.s))
    
  })
  
  output$pie <- renderPlotly({
    # blank_theme <- theme_minimal()+
    #   theme(
    #     axis.title.x = element_blank(),
    #     axis.title.y = element_blank(),
    #     panel.border = element_blank(),
    #     panel.grid=element_blank(),
    #     axis.ticks = element_blank(),
    #     plot.title=element_text(size=14, face="bold")
    #   )

    
     plot_ly(location(), labels = ~Event.Clearance.Group, values = ~frequency, type = 'pie',
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 insidetextfont = list(color = '#FFFFFF'),
                 hoverinfo = 'text',
                 text = ~paste(frequency, ' times'),
                 marker = list(#colors = colors,
                               line = list(color = '#FFFFFF', width = 1)),
                 #The 'pull' attribute can also be used to create space between the sectors
                 showlegend = FALSE) %>%
      layout(title = 'Percentage of types of incidents in a district',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
   })
}
shinyUI(server)

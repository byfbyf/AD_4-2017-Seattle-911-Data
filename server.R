library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
library(stringr)
library(lubridate)
library(plotly)
library(ggrepel)
library(zipcode)
#read the data
data.2017 <-read.csv("./2017_Seattle_911_processed.csv", stringsAsFactors = FALSE)
#Set as time and date
data.2017$Event.Clearance.Date <-as_datetime(data.2017$Event.Clearance.Date)


data.2017$Event.Clearance.Date <-as.POSIXct(data.2017$Event.Clearance.Date)
#Date of start of the month
month.start <- c(paste0("2017-", 1:12, "-01"), "2018-01-01")

at.scene <- str_split_fixed(data.2017$At.Scene.Time, " ", 2)

#add columns of at scence time and date
data.2017 <-  data.2017 %>%
  mutate(At.Scene.Date = as.Date(at.scene[, 1], "%m/%d/%Y"),
         At.Scene.Time = format(strptime(at.scene[, 2], "%I:%M:%S %p"), 
                                format="%H:%M"))

data("zipcode")

#filter out the zip code of Seattle city
seattle.zipcode <- filter(zipcode, city == "Seattle")

server <- function(input, output) {
  #Intro
  output$intro <- renderUI({
    str1 <- paste("Our project is about the dataset of Seattle's 911
                  data from year 2017. ")
    str2 <- paste("There are <b>", nrow(data.2017), "</b> calls
                  in total. The most common subject of the calls is <b>traffic 
                  related calls.</b><br/>")
    str3 <- "<u>In our project, we will have three sections:</u>
            <li>Part 1 displays the average  daily call numbers regarding various topics 
                for different months in the year 2017. </li>
            <li>Part 2 display a map where officials responds
                to the call at secene using leaflet.</li>
            <li>Part 3 shows type of calls in different areas.</li>"
    HTML(paste(str1, str2, str3, sep = '<br/>'))
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
  
  #Description of Part 1 selector
  output$text1 <- renderText({
    text <- "There are many different subjects of calls. Choose one you are interested in 
    to see how many calls per day in each month regarding this topic."
    return(text)
  })

  
  #Shows the average per day of the year and the maxium among the 12 month
  output$text2 <- renderText({
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
  
  # Conver hour to two digits
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
  
  #Users input of zip code
  zip <- reactive({
    zipcode <- zipcode %>% 
      filter(zip == input$zipcode)
  })
  #Instruction
  output$instruction <- renderUI({
    str1 <- "You can view 911 police at scene place of any 
                             date period in 2017 by selecting the time of interest."
    str2 <- "Sliding the slide bar, you can view 911 police 
                             at scene events happened in the time period of interest."
    str3 <- "You can also text a zipcode to add a marker 
                             at the area of your interest. The default vaule is 98105."
    HTML(paste(str1, str2, str3, sep = '<br/>'))
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
  
  #Icon 
  greenLeafIcon <- makeIcon(
    iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
    iconWidth = 38, iconHeight = 95,
    iconAnchorX = 22, iconAnchorY = 94,
    shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
    shadowWidth = 50, shadowHeight = 64,
    shadowAnchorX = 4, shadowAnchorY = 62
  )
  
  # Render map
  # Add markers of at scene location and input zip code loaction
  output$map <- renderLeaflet({
    map <- leaflet() %>%
      addTiles() %>%
      addCircleMarkers(lng=data()$Longitude, lat=data()$Latitude, radius = 5, 
                       color = "black", popup = data()$Event.Clearance.Description,
                       clusterOptions = markerClusterOptions()) %>% 
      addMarkers(lng=zip()$longitude, lat=zip()$latitude, icon = greenLeafIcon)
      
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
    return(loc)
    
  })
  
  #render table of information
  output$info <- renderTable ({
    location()
  })
  
  #Total number of calls in the area
  output$total <- renderPrint ({
    t <- filter(data.2017, District.Sector == input$area) %>%
      group_by(Event.Clearance.Group) %>%
      summarize(frequency = n())
    t.s <- sum(t$frequency)
    print(paste('Total: ', t.s))
    
  })
  
  #output pie chart with plotyly
  output$pie <- renderPlotly({
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

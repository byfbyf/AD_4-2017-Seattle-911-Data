library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  h2("2017 Seattle 911 Events Map"),
  tabsetPanel(type = "tabs",
    tabPanel("Intro", htmlOutput("intro")),
    tabPanel("Monthly Average Call", 
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "type", label = strong("Type"),
                             choices = c("All", "Traffic related calls",                
                                         "Disturbances",                         
                                         "Suspicious circumstances",             
                                         "Motor vehicle collision investigation", 
                                         "False alacad",                          
                                         "Trespass",                             
                                         "Liquor violations",     
                                         "Car prowl",                      
                                         "Shoplifting",                          
                                         "Nuisance, mischief",              
                                         "Burglary",                       
                                         "Other property",                       
                                         "Behavioral health",
                                         "Auto thefts", 
                                         "Assaults",                             
                                         "Hazards",                    
                                         "Property damage",                   
                                         "Narcotics complaints",                
                                         "Property - missing, found" ,  "Fraud calls", "Threats, harassment",                  
                                         "Arrest", "Person down/injury",  "Robbery",                              
                                         "Miscellaneous misdemeanors","Persons - lost, found, missing","Animal complaints" ,                   
                                         "Bike","Weapons calls","Lewd conduct" ,                        
                                         "Failure to register (sex offender)" ,   "Prowler","Harbor calls",                        
                                         "Prostitution","Public gatherings","Drive by (no injury)",              
                                         "Other vice","Reckless burning","Homicide",                            
                                         "Vice calls"),
                             selected = "All")               ),
               mainPanel(
                 plotOutput("plot1"),
                 textOutput("text1")
                 
                                )
               )
             ),
    tabPanel("Where and When are Officers at Scene",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput("date", "Date in 2017: ", 
                                start = "2017-1-1",
                                end = "2017-1-31",
                                min = "2017-1-1",
                                max = "2017-12-31",
                                format = "mm/dd/yy"),
                 sliderInput("time", "Time Range: ",
                             0, 24, c(0, 24), 1, 
                             round = FALSE)
               ),
               mainPanel(
                 h3("2017 Seattle 911 at Scene Events Map"),
                 textOutput("text"),
                 leafletOutput("map", width = 1000, height = 600)
               )
             )
    ),
  tabPanel("Call Types in Different Areas")
)
)

shinyUI(ui)
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  theme = "style.css",
  h1(paste0("2017 Seattle "), span("911"), "Calls"),
  h4("Info201 AD Win18 Group#4: Yining Wang, Yifan Bai, Coco Ye"),
  tabsetPanel(type = "tabs",
              tabPanel("Intro", htmlOutput("intro"),
                       img(id = "img",src = "911.jpg", alt = "img")),
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
                                                   "Property - missing, found" ,  
                                                   "Fraud calls", "Threats, harassment",                  
                                                   "Arrest", "Person down/injury", 
                                                   "Robbery",                              
                                                   "Miscellaneous misdemeanors",
                                                   "Persons - lost, found, missing","Animal complaints" ,                   
                                                   "Bike","Weapons calls",
                                                   "Lewd conduct" ,                        
                                                   "Failure to register (sex offender)" ,  
                                                   "Prowler","Harbor calls",                        
                                                   "Prostitution","Public gatherings",
                                                   "Drive by (no injury)",              
                                                   "Other vice","Reckless burning",
                                                   "Homicide",                            
                                                   "Vice calls"),
                                       selected = "All"),
                           textOutput("text1")
                          ),
                         
                         mainPanel(
                           p(class = "text", "We thought that the call numbers might vary at different time
                             of the year, so we get the average calls per day regarding different
                             topics in the 12 months."),
                           textOutput("text2"),
                           plotOutput("plot1")
                           )
                         )
              ),
              tabPanel("Time and places of Officers at Scene",
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
                                       round = FALSE),
                           numericInput("zipcode", "Zipcode in Seattle:", 
                                        value = 98105, min = 98060, max = 98199),
                           htmlOutput("instruction")
                         ),
                         mainPanel(
                           h2("2017 Seattle 911 at Scene Events Map"),
                           textOutput("text"),
                           leafletOutput("map", width = 1000, height = 600)
                         )
                       )
              ),
              tabPanel("Call Types in Different Areas",
                       sidebarPanel(
                         
                         selectInput('area', 'districts displayed', c('King' = 'K','George' = 'G',
                                                                      'Edward' = 'E','David' = 'D','Charlie' = 'C',
                                                                      'William' = 'W','Nora' = 'N', 'Queen' = 'Q',
                                                                      'Frank' = 'F', 'Mary' = 'M', 'Sam' = 'S',
                                                                      'Robert' = 'R','Ocean' = 'O', 'Boy' = 'B',
                                                                      'Union' = 'U','John' = 'J','Lincoln' = 'L','99' = '99','Harbor' = 'H')),
                         hr(),
                         helpText(class = "text", 'Districts of seattle. Select one and see relevant data in that district in 2017!'),
                         h3('District Map of Seattle'),
                         p(class = "text", 'This map shows corresponding district areas of Seattle'),
                         img(id = "map", src = 'map.jpg')),
                       mainPanel (
                         h2('Call Types in Different Areas'),
                         
                         p(class = "text", 'This page investigate incidents that occur most in each district in seattle'),
                         p(class = "text", 'Below analyze which types of incidents oocur most frequently in certain area 
                           by showing its frequency and percentage of occurances'),
                         tableOutput('info'),
                         
                         h3('Total frequency'),
                         textOutput('total'),
                         hr(),
                         p(class = "text", 'The pie chart shows the percentage of 
                           each type of accidents happened in this area'),
                         plotlyOutput('pie')
                       )
              )
  )
  )

shinyUI(ui)
library('dplyr')
library('shiny')
library('ggplot2')


call <- read.csv('2017_Seattle_911.csv', stringsAsFactor = FALSE)
district <- distinct(call, District.Sector)
district <- district[-19, ]

my.ui <- fluidPage (
  
  
  sidebarLayout(
    sidebarPanel(
      # selectInput('area', 'districts displayed', district)
      selectInput('area', 'districts displayed', c('King' = 'K','George' = 'G',
                                                   'Edward' = 'E','David' = 'D','Charlie' = 'C',
                                                   'William' = 'W','Nora' = 'N', 'Queen' = 'Q',
                                                   'Frank' = 'F', 'Mary' = 'M', 'Sam' = 'S',
                                                   'Robert' = 'R','Ocean' = 'O', 'Boy' = 'B',
                                                   'Union' = 'U','John' = 'J','Lincoln' = 'L','99' = '99','Harbor' = 'H')),
      hr(),
      helpText('Districts of seattle. Select one and see relevant data in that district in 2017!')
      
      ),
    mainPanel (
      h1('Introduction'),
      
      p('This page investigate incidents that occur most in each district in seattle'),
      p('Below analyze which types of incidents oocur most frequently in certain area 
        by showing its frequency and percentage of occurances'),
      tableOutput('info'),
      
      h4('Total frequency'),
      textOutput('total'),
      
      p('The pie chart shows the percentage of each type of accidents happened in this area'),
      plotOutput('pie'),
      
      h4('District Map of Seattle'),
      p('This map shows corresponding district areas of Seattle.'),
      img(src = 'map.jpg')
    )
  )
)

shinyUI(my.ui)
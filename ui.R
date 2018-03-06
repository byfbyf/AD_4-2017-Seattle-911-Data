library(shiny)
ui <- fluidPage(
  titlePanel("Seattle 911 Data of 2017"),
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
                  selected = "All"),
      textOutput("text1")
      ),
    mainPanel(
      plotOutput("plot")
    )
      
    )
  )

shinyUI(ui)
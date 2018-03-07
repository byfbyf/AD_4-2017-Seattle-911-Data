library('dplyr')
library('shiny')
library('ggplot2')
library('scales')

my.server <- function(input, output) {
  
  location <- reactive ({
    loc <- filter(call, District.Sector == input$area) %>%
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
  
  
  output$info <- renderTable ({
    location()
  })
  
  output$total <- renderPrint ({
    t <- filter(call, District.Sector == input$area) %>%
      group_by(Event.Clearance.Group) %>%
      summarize(frequency = n())
    t.s <- sum(t$frequency)
    print(paste('Total: ', t.s))
    
  })
  
  output$pie <- renderPlot({
    blank_theme <- theme_minimal()+
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold")
      )
    ggplot(location(), aes(x="", y= frequency, fill = Event.Clearance.Group))+
      geom_bar(width = 1, stat = 'identity') + coord_polar("y", start=0) +
      scale_fill_brewer("Blues") + theme(axis.text.x=element_blank()) + blank_theme
  })
  
  
}

shinyServer(my.server)
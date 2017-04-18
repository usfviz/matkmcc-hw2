rm(list = ls())
cat('\014')

# creating the interactive shiny document

# create ggvis?
# need plot with fertility on the x and life on the y
# need bubble to be population size
# need color to be company


library(shiny)
library(ggvis)
library(ggplot2)
library(ggvis)
library(dplyr)
library(tidyr)

# sourced function
source('global.R')
source('dot_plot.R')

regs <- sort(unique(big_d$Region))
region_lookup <- lapply(letters[c(1:length(regs))], c)
names(region_lookup) <- regs

create_country_input <- function(region) {
  regionID <- region_lookup[[region]]
  tagList(
      h4(region),
      selectInput(paste0(regionID,'_'), NULL, 
                 choices = list('Countries' = c('Select All Countries', sort(unique(big_d$Country.Name[big_d$Region == region])))),
                 selected = 'Select All Countries',
                 multiple = T, 
                 selectize = T
      )
  )
}


 

# slider input animate = r
ui <- fluidPage(
  
    titlePanel("Assignment #2 - Visualization"),
    
    sidebarLayout(
          
            sidebarPanel(
              
              uiOutput('country_selection')
              ,
              
              sliderInput('year', 'Select Year', 
                          min = min(big_d$Year),
                          max = max(big_d$Year),
                          value = min(big_d$Year),
                          sep = '',
                          animate = animationOptions(interval = 100))
              , 
              sliderInput('scalar', 'Select Scale', 
                          min = 0,
                          max = 100,
                          value = 0,
                          step = 1,
                          sep = '')
              ,
              fluid = F
              
            ),
    
            mainPanel(
                tabPanel("dot", ggvisOutput('dot_plot'), width='100%', height='100%')
                      )
            
                  )
              )

server <- function(input, output) {
  
  # render the UI
  output$country_selection <- renderUI({
    
    lapply(sort(unique(big_d$Region)), create_country_input)
    
  })
  
  
  # filter the input variables
  subset_data <- reactive({
    
    subset_countries <- c()
    for (l in letters[c(1:length(regs))]) {
      input_countries <- input[[paste0(l, '_')]]
      if ('Select All Countries' %in% input_countries) {
        this_region <- regs[which(letters == l)]
        subset_countries <- c(subset_countries, this_region)
      } else {
        subset_countries <- c(subset_countries, input_countries)
      }
    }
    

    return(big_d %>% filter(Country.Name %in% subset_countries, 
                            Year == input$year))
  })
  
  # Create the plot
  reactive({
    dot_plot(subset_data(), input$scalar) %>% 
      set_options(width='auto', height=1000)
    }) %>% bind_shiny('dot_plot')

}


shinyApp(ui = ui, server = server)
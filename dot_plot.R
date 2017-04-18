dot_plot <- function(plot_data, scale_parameter) {
  
  plot_data <- plot_data[complete.cases(plot_data),]
  
  #scale the size of the dots
  plot_data$population <- 100 + plot_data$population/max(plot_data$population, na.rm = T) * scale_parameter**2.5
  
  text_output <- function(x) {
    if(is.null(x)) return(NULL)
    country.name <- big_d$Country.Label[which(big_d$ID == x$ID)]
    population_ <- big_d$population[which(big_d$ID == x$ID)]
    paste0(
             paste0('<strong>', country.name, '</strong>', '&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp'), '<br/>',
             paste0('Population: ', 
                    prettyNum(round(population_,0), big.mark = ',')), '<br/>',
             paste0('Fertility: ', round(x$fertility, 2)), '<br/>',
             paste0('Life Expectancy: ', round(x$life_exp, 2)))
  }
  
  dot_plot <- ggvis(data = plot_data, 
                   x=~life_exp, 
                   y=~fertility, 
                   fill=~Country.Name, 
                   stroke := 'black',
                   size :=~population,
                   key :=~ID
                   ) %>%
  layer_points(data = plot_data) %>%
  add_tooltip(text_output, on = c('hover')) %>%
  hide_legend(c('size', 'shape')) %>%
  add_axis(type = 'x', title = 'Life Expectancy', 
           grid = F, values = seq(0,90,by=5)) %>%
  add_axis(type = 'y', title = 'Fertility Rate', 
           grid = F, values = seq(0,9,1)) %>% 
  scale_numeric('x', domain = c(0,90)) %>%
  scale_numeric('y', domain = c(0,9))
    
  return (dot_plot)
}

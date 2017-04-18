# creating the interactive shiny document
life <- read.csv('./LIFE/life_expectancy.csv', stringsAsFactors = F)
life_notes <- read.csv('./LIFE/life_expectancy_notes.csv', stringsAsFactors = F)
fertility <- read.csv('./FART/fertility_rates.csv', stringsAsFactors = F)
fertility_notes <- read.csv('./FART/fertility_rates_notes.csv', stringsAsFactors = F)
population <- read.csv('./POP/population.csv', stringsAsFactors = F)
population_notes <- read.csv('./POP/population_notes.csv', stringsAsFactors = F)

# TO DO: ORGANIZE COUNTRIES AND COUNTRY GROUPS!
# Check that the notes datasets are equivelent - then choose one for lookup
sum(life_notes[c('Country.Code', 'Region')] != fertility_notes[c('Country.Code', 'Region')])
sum(life_notes[c('Country.Code', 'Region')] != population_notes[c('Country.Code', 'Region')])

# create a lookup for getting the region from the country code
lookup <- lapply(life_notes$Region, c)
names(lookup) <- life_notes$Country.Code

lookup_function <- function(x) {
  if (x %in% names(lookup)) {
    return(lookup[[x]])
  } 
  return('Check')
}

# set order for the factors
levels = as.numeric(1960, 2016)

# life
life$X <- NULL
colnames(life) <- gsub('X', '', colnames(life))
life$Indicator.Code <- NULL
life$Indicator.Name <- NULL
life <- life %>% gather(Year, life_exp, -Country.Name, -Country.Code) %>% 
  dplyr::mutate(Year = as.numeric(Year),
                Region = unlist(lapply(Country.Code, lookup_function)), 
                Country.Label = Country.Name,
                ID = seq_along(Country.Name))
life2 <- life
life2$ID <- life2$ID + nrow(life2)
life2$Country.Name <- unlist(lapply(life$Country.Code, lookup_function))
life <- rbind(life, life2)

# fertility
fertility$X <- NULL
colnames(fertility) <- gsub('X', '', colnames(fertility))
fertility$Indicator.Code <- NULL
fertility$Indicator.Name <- NULL
fertility <- fertility %>% gather(Year, fertility, -Country.Name, -Country.Code) %>% 
  dplyr::mutate(Year = as.numeric(Year),
                Region = unlist(lapply(Country.Code, lookup_function)), 
                Country.Label = Country.Name,
                ID = seq_along(Country.Name))
fertility2 <- fertility
fertility2$ID <- fertility2$ID + nrow(fertility2)
fertility2$Country.Name <- unlist(lapply(fertility2$Country.Code, lookup_function))
fertility <- rbind(fertility, fertility2)

# population
population$X <- NULL
colnames(population) <- gsub('X', '', colnames(population))
population$Indicator.Code <- NULL
population$Indicator.Name <- NULL
population <- population %>% gather(Year, population, -Country.Name, -Country.Code) %>% 
  dplyr::mutate(Year = as.numeric(Year),
                Region = unlist(lapply(Country.Code, lookup_function)), 
                Country.Label = Country.Name, 
                ID = seq_along(Country.Name))
population2 <- population
population2$ID <- population2$ID + nrow(population2)
population2$Country.Name <- unlist(lapply(population2$Country.Code, lookup_function))
population <- rbind(population, population2)

# pull the data together
big_d <- merge(life, fertility, by = c('Country.Name', 'Country.Code', 'Year'))
big_d <- merge(big_d, population, by = c('Country.Name', 'Country.Code', 'Year'))

# remove 'Not Classified' Country name and aggregate regions
big_d <- big_d %>% filter(!(Country.Name %in% c('', 'Check')), !(Region %in% c('', 'Check')))
big_d <- big_d %>% filter(!(Year %in% c(2015, 2016)))

# set the maximum population value
max_population <- max(big_d$population, na.rm = T)



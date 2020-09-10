library(tidycensus)
library(stringr)
library(mapview)
library(tidyverse)
library(tigris)
library(sf)

Sys.getenv("CENSUS_API_KEY")

# Searching for Household income variables
v16 <- load_variables(year = 2018, dataset = 'acs5', cache = TRUE)
hh_income <- filter(v16, str_detect(label, fixed('median household income', 
                                                 ignore_case = T)))
# Getting the data from 2010 to 2018 
ca_data <- map_df(2010:2018, function(x) {
  get_acs(geography = "tract", variables = c(median.housing.value = "B25077_001",
                                             median.household.income = "B19013_001",
                                             population = "B02001_001",
                                             total.owner.occupied.housing.units = 'B25012_002',
                                             total.renter.occupied.housing.units = 'B25012_010',
                                             median.age = 'B01002_001',
                                             median.number.rooms = 'B25021_002',
                                             median.year.built = 'B25037_002'), 
          
          state = "CA", survey = 'acs5', year = x, geometry = T, cache_table = T, output = 'wide') %>%
    mutate(year = x)
})

# Write the data
write_sf(ca_data, 'ca_data.shp')

# Changing the names
names(ca_data)
names(ca_data)[3:8] <- c("median.housing.value.estimate", "median.housing.value.moe",
                         "median.household.income.estimate", "median.household.income.moe",
                         "population.estimate", "population.moe")


# Code used to search for variables 
v16 %>% filter(str_detect(label, fixed('Estimate!!Total!!Owner-occupied housing units', 
                            ignore_case = T))) %>% View()
v16 %>% filter(name == 'B01002_001')
x <- get_acs(geography = "tract", variable = 'B25012_010',
             state = "CA", survey = 'acs5', year = 2018, geometry = T, cache_table = T) 







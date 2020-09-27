library(tidycensus)
library(stringr)
library(tidyverse)
library(tigris)
library(sf)
library(skimr)

# Searching for Household income variables
v16 <- load_variables(year = 2018, dataset = 'acs5', cache = TRUE)

# An example on how to search for the variables you are interested about.
# Replace median household income with other variables such as population, and then choose the name.
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
                                             median.year.built = 'B25037_002',
                                             married_couple_family_1_unit_structures = 'B11011_004',
                                             married_couple_family_2_or_more_unit_structures = 'B11011_005',
                                             aggregate_travel_time_to_work_minutes = 'B08013_001'
                                             ), 
          
          state = "CA", survey = 'acs5', year = x, geometry = T, output = 'wide') %>%
    mutate(year = x)
}) 

st_write(ca_data, 'ca_data.shp', driver="ESRI Shapefile")

# md HSH E - median household income
# md HSN E - median housing value
# md n E - median.number.rooms.estimate
# md yE - median Year Built
# medn ge - median.age
# popltn E - population
# ttl__r__E - total.renter.occupied.housing.units.estimate
# ttl__w___E - total.owner.occupied.housing.units

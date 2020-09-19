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

v16 %>% filter(str_detect(label, fixed('time', ignore_case = T))) %>% View()

# B11011_004
# B11011_005
x <- get_acs(geography = 'tract', variables = c(aggregate_travel_time_to_work_minutes = 'B08013_001'),
        state = "CA", survey = 'acs5', year = 2018, output = 'wide')

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

# Changing the column names
names(ca_data)[3:18] <- c("median.housing.value.estimate", "median.housing.value.moe",
    "median.household.income.estimate", "median.household.income.moe",
    "population.estimate", "population.moe", "total.owner.occupied.housing.units.estimate",
    "total.owner.occupied.housing.units.moe", 'total.renter.occupied.housing.units.estimate', 
    'total.renter.occupied.housing.units.moe',
    'median.age.estimate','median.age.moe' ,'median.number.rooms.estimate', 'median.number.rooms.moe',
    'median.year.built.estimate', 'median.year.built.moe'
    )

# Write the data to a shape file for easy access
write_sf(ca_data, 'ca_data.shp')

# md HSH E - median household income
# md HSN E - median housing value
# md n E - median.number.rooms.estimate
# md yE - median Year Built
# medn ge - median.age
# popltn E - population
# ttl__r__E - total.renter.occupied.housing.units.estimate
# ttl__w___E - total.owner.occupied.housing.units

# Glimpse the data and its Coordinate Reference System
ca_data %>% glimpse()
st_crs(ca_data) # North American Datum 1983. 








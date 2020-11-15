# Reading the data
library(tidyverse)
library(dplyr)
library(skimr)
library(sf)
library(DT)
library(readxl)
library(moderndive)
library(naniar)
library(crosstalk)
library(plotly)
library(mapview)
library(tidycensus)
library(tigris)
library(tmap)
library(tmaptools)

ca_data2 <- st_read('/Users/ivanpatel/Desktop/School/Senior Project/Data/Census/ca_data.shp')
coastline_counties <- read_xlsx('/Users/ivanpatel/Desktop/School/Senior Project/Data/coastline-counties-list.xlsx')

# Which counties are coastal and near the coast
ca_coastline_counties <- coastline_counties %>% filter(`STATE NAME` == 'California')
ca_coastline_counties <- ca_coastline_counties[, "COUNTY NAME", drop = T]
near_coast_counties <- c('Trinity County', 'Lake County', 'Napa County')


# Modifying the names
names(ca_data2)[4:27] <- c("median.housing.value.estimate", "median.housing.value.moe",
                          "population.estimate", "population.moe", 
                          "total.owner.occupied.housing.units.estimate","total.owner.occupied.housing.units.moe", 
                          'total.renter.occupied.housing.units.estimate', 'total.renter.occupied.housing.units.moe',
                          'median.age.estimate','median.age.moe' ,
                          'median.number.rooms.estimate', 'median.number.rooms.moe',
                          'median.year.built.estimate', 'median.year.built.moe', 
                          'married_couple_family_1_unit_structures.estimate', 'married_couple_family_1_unit_structures.moe', 
                          'married_couple_family_2_or_more_unit_structures.estimate', 'married_couple_family_2_or_more_unit_structures.moe', 
                          'aggregate_travel_time_to_work_minutes.estimate', 'aggregate_travel_time_to_work_minutes.moe', 
                          'year', 
                          'NAME_county', 
                          "median.county.household.income.estimate",
                          "median.county.household.income.moe"
                          
)

ca_data2 %>% select(contains('estimate')) %>% 
  st_drop_geometry() %>% skim() 

# Let's first identify the missing housing values from the original data for each year - 
# Check whether or not they are randomly distributed. 
ca_data_missing <- ca_data2 %>% filter(is.na(median.housing.value.estimate)) 

# What is the percentage of tracts that have a missing housing value for each year?
ca_data_missing %>% st_drop_geometry() %>% 
  group_by(year) %>% summarize(housing_values_missing = n(), prc_missing = (n() / 8057) * 100)

ca_data_missing %>% st_drop_geometry() %>% 
  select(contains('estimate'), year) %>% 
  group_by(year) %>% miss_var_summary() %>% ungroup() %>% 
  filter(pct_miss != 0) %>% 
  ggplot(aes(x = year, y = pct_miss, color = variable)) + geom_line()

# Which county has the most tracts with a missing housing value

# Total tracts in a given county-year
(total_tracts_by_county_year <- ca_data2 %>%  
  st_drop_geometry() %>% 
  group_by(year, NAME_county) %>% 
  summarize(num_of_tracts = n()) %>% 
  arrange(desc(num_of_tracts)))

# What perc of census tracts in a given county-year is missing?

## Getting the number of missing tracts in a given county-year
ca_data_missing %>% st_drop_geometry() %>% 
  group_by(year, NAME_county) %>% 
  summarize(num_of_tracts_with_missing_house_values = n()) %>% 
  arrange(desc(num_of_tracts_with_missing_house_values)) %>% ungroup() %>% 
  
  # Inner joining the number of missing tracts with total_tracts_by_county_year
  inner_join(total_tracts_by_county_year, by = c('year' = 'year','NAME_county' = 'NAME_county')) %>% 
  mutate(perc_of_missing_tracts = (num_of_tracts_with_missing_house_values / num_of_tracts) * 100) %>% 
  arrange(year, desc(perc_of_missing_tracts)) %>% DT::datatable()

# Making a map of the missing tracts. 
# This check is to ensure that the missing tracts are randomly distributed. 
tmap_mode('plot')
ca_data_missing  %>%
tm_shape() + tm_polygons('NAME_county', legend.show = FALSE) +
tm_facets(by = 'year')

# Identifying which census tracts are in coastal counties, near coastal counties, or not in coastal counties
# Creating a new variable coastal - Categorical
ca_data2 <- ca_data2 %>%
  mutate(Coastal = case_when(
    str_detect(NAME_county, paste(ca_coastline_counties, collapse = "|")) ~ 'Coastal',
    str_detect(NAME_county, paste(near_coast_counties, collapse = "|")) ~ 'Near Coastal',
    TRUE ~ 'Not Coastal'
  ))

# Let's prepare the data we will feed our model
model_data <- ca_data2 %>% 
              filter(!is.na(median.housing.value.estimate), 
                     !is.na(median.number.rooms.estimate), 
                     !is.na(aggregate_travel_time_to_work_minutes.estimate)) %>%
  
  mutate(total.occupied = total.owner.occupied.housing.units.estimate + total.renter.occupied.housing.units.estimate, 
         renter_occupied_perc = total.renter.occupied.housing.units.estimate / total.occupied) %>% 
  
  select(C_GEOID:NAME_tr, year, NAME_county, Coastal, contains('estimate'), 
         renter_occupied_perc, 
         -median.year.built.estimate, 
         -total.owner.occupied.housing.units.estimate, 
         -total.renter.occupied.housing.units.estimate)

name_model_data <- names(model_data)

## WHAT PERCENTAGE OF THE ORIGINAL DATA DO WE HAVE LEFT?
(nrow(model_data) / nrow(ca_data2)) * 100



# PLOTS TO EXPLORE THE DATA -----------------------------------------------

# Distribution of median housing values
model_data %>% st_drop_geometry() %>% 
  ggplot(aes(x = median.housing.value.estimate, fill = Coastal)) +
  geom_density(alpha = 0.4) +
  scale_x_continuous(labels = scales::dollar_format())

# Boxplot of housing values across the years. 
ggplot(model_data, aes(x = year, y = median.housing.value.estimate, group = year)) + 
  geom_boxplot() + scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = 'Year', y = 'Housing Values')

model_data %>% filter(median.housing.value.estimate >= 1000000, year >= 2015) %>% 
  tm_shape() + 
  tm_polygons('NAME_county', legend.show = FALSE) +
  tm_facets(by = 'year')

# Does Renter occupied percentage differ by county?
model_data %>% st_drop_geometry() %>% 
  group_by(year, Coastal) %>% summarize(mean_renter_perc = mean(renter_occupied_perc)) %>% 
  ggplot(aes(x = year, y = mean_renter_perc, color = Coastal)) +
  geom_line()



  


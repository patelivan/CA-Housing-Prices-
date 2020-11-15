# Reading the data
library(tidyverse)
library(dplyr)
library(sf)
library(psych)
library(lmtest)
library(stargazer)
library(plm)

# Linear Regression 2010-2014 V. 2015-2018 -------------------------------------------

# Reads the data and assigns readable column names 

model_data <- st_read('/Users/ivanpatel/Desktop/School/Senior Project/Data/Model/model.shp')
names(model_data) <- c("C_GEOID", "T_GEOID", "NAME_tr",                                                 
                       "year","NAME_county", "Coastal", 
                       "median.housing.value.estimate",                           
                       "population.estimate", "median.age.estimate",                                 
                       "median.number.rooms.estimate",                       
                       "married_couple_family_1_unit_structures.estimate",       
                       "married_couple_family_2_or_more_unit_structures.estimate",
                       "aggregate_travel_time_to_work_minutes.estimate", 
                       "median.county.household.income.estimate", 
                       "renter_occupied_perc", 
                       "geometry")

## Separate the years: 2010_2014:

# Filters for years 2010 to 2014, mutate log estimates, drop the geometry
ols_model_2010_2014 <- model_data %>% filter(year %in% c(2010,2011,2012,2013,2014)) %>% 
  mutate(log_house_values_estimate = log(median.housing.value.estimate), 
         log_county_income_estimate = log(median.county.household.income.estimate)) %>% 
  st_drop_geometry()
  
  # Removes the county names, tract name, GEOID's and other unnecessary columns
ols_model_2010_2014 <- ols_model_2010_2014 %>%  select(-C_GEOID:-NAME_county, 
         -married_couple_family_2_or_more_unit_structures.estimate, 
         -married_couple_family_1_unit_structures.estimate, 
         -aggregate_travel_time_to_work_minutes.estimate, 
         -median.housing.value.estimate, 
         -median.county.household.income.estimate, 
         -population.estimate)

## Do the same for 2015:2018

# Filters for years 2015 to 2018, mutate log estimates, drop the geometry
ols_model_2015_2018 <- model_data %>% filter(year %in% c(2015,2016,2017,2018)) %>% 
  mutate(log_house_values_estimate = log(median.housing.value.estimate), 
         log_county_income_estimate = log(median.county.household.income.estimate)) %>% 
  st_drop_geometry()
  
  # Removes the county names, tract name, GEOID's and other unnecessary columns
ols_model_2015_2018 <- ols_model_2015_2018 %>% select(-C_GEOID:-NAME_county, 
         -married_couple_family_2_or_more_unit_structures.estimate, 
         -married_couple_family_1_unit_structures.estimate, 
         -aggregate_travel_time_to_work_minutes.estimate, 
         -median.housing.value.estimate, 
         -median.county.household.income.estimate, 
         -population.estimate)

# I think Heteroskedasticty could be an issue in both models. 
ols_model_2010_2014 %>% 
  select(median.age.estimate:renter_occupied_perc, log_county_income_estimate, 
         log_house_values_estimate) %>% 

  pairs.panels(method = "pearson", # correlation method
             hist.col = "#00AFBB"
)

par(mfrow=c(2,3))
boxplot(data=ols_model_2010_2014, median.age.estimate~Coastal, ylab='Age', main='2010-2014')
boxplot(data=ols_model_2010_2014, median.number.rooms.estimate~Coastal, ylab='Rooms')
boxplot(data=ols_model_2010_2014, renter_occupied_perc~Coastal, ylab='Renter Perc')
boxplot(data=ols_model_2010_2014, log_house_values_estimate~Coastal, ylab='House Values')
boxplot(data=ols_model_2010_2014, log_county_income_estimate~Coastal, ylab='County Income')


ols_model_2015_2018 %>% 
  select(median.age.estimate:renter_occupied_perc, log_county_income_estimate, 
         log_house_values_estimate) %>% 
  
  pairs.panels(method = "pearson", # correlation method
               hist.col = "#00AFBB"
  )

par(mfrow=c(2,3))
boxplot(data=ols_model_2015_2018, median.age.estimate~Coastal, ylab='Age', main='2015-2018')
boxplot(data=ols_model_2015_2018, median.number.rooms.estimate~Coastal, ylab='Rooms')
boxplot(data=ols_model_2015_2018, renter_occupied_perc~Coastal, ylab='Renter Perc')
boxplot(data=ols_model_2015_2018, log_house_values_estimate~Coastal, ylab='House Values')
boxplot(data=ols_model_2015_2018, log_county_income_estimate~Coastal, ylab='County Income')

# Let's run the regressions 
model_2010_2014 <- lm(data = ols_model_2010_2014, log_house_values_estimate ~ . + 
                        renter_occupied_perc * Coastal +
                        log_county_income_estimate * Coastal +
                        log_county_income_estimate * renter_occupied_perc)

model_2015_2018 <- lm(data = ols_model_2015_2018, log_house_values_estimate ~ . + 
                        renter_occupied_perc * Coastal +
                        log_county_income_estimate * Coastal +
                        log_county_income_estimate * renter_occupied_perc)

# Computing Huber-white standard errors
rob_se <- list(sqrt(diag(vcovHC(model_2010_2014, type='HC1'))), 
               sqrt(diag(vcovHC(model_2015_2018, type='HC1'))))

# Print the model coefficients
stargazer(model_2010_2014, 
          model_2015_2018, digits = 3, se = rob_se, column.labels = c('2010-2014', '2015-2018'),
          type='html', out='seperate-ols-models.html')

# Checking the OLS assumptions of the previous model
par(mfrow = c(2, 2))
plot(model_2010_2014)

par(mfrow = c(2, 2))
plot(model_2015_2018)

# Fixed Effects Regressions ---------------------------------------------------------

# County and Time Fixed Effects
fixed_model_2010_2014 <- model_data %>% filter(year %in% c(2010,2011,2012,2013,2014)) %>% 
  mutate(log_house_values_estimate = log(median.housing.value.estimate), 
         log_county_income_estimate = log(median.county.household.income.estimate)) %>% 
  st_drop_geometry()

# Removes the county names, tract name, GEOID's and other unnecessary columns
fixed_model_2010_2014 <- fixed_model_2010_2014 %>%  select(-C_GEOID:-NAME_tr, 
                                                       -married_couple_family_2_or_more_unit_structures.estimate, 
                                                       -married_couple_family_1_unit_structures.estimate, 
                                                       -aggregate_travel_time_to_work_minutes.estimate, 
                                                       -median.housing.value.estimate, 
                                                       -median.county.household.income.estimate, 
                                                       -population.estimate)

# Filters for years 2015 to 2018, mutate log estimates, drop the geometry
fixed_model_2015_2018 <- model_data %>% filter(year %in% c(2015,2016,2017,2018)) %>% 
  mutate(log_house_values_estimate = log(median.housing.value.estimate), 
         log_county_income_estimate = log(median.county.household.income.estimate)) %>% 
  st_drop_geometry()

# Removes the county names, tract name, GEOID's and other unnecessary columns
fixed_model_2015_2018 <- fixed_model_2015_2018 %>% select(-C_GEOID:-NAME_tr, 
                                                      -married_couple_family_2_or_more_unit_structures.estimate, 
                                                      -married_couple_family_1_unit_structures.estimate, 
                                                      -aggregate_travel_time_to_work_minutes.estimate, 
                                                      -median.housing.value.estimate, 
                                                      -median.county.household.income.estimate, 
                                                      -population.estimate)

# Converting the year columns in both datasets to factor and other clean ups
fixed_model_2010_2014$year <- as.factor(fixed_model_2010_2014$year)
fixed_model_2010_2014$NAME_county <- as.factor(fixed_model_2010_2014$NAME_county)
fixed_model_2010_2014 <- fixed_model_2010_2014 %>% mutate(i=1) %>% spread(NAME_county, i, fill=0)
fixed_model_2010_2014 <- fixed_model_2010_2014 %>% mutate(i=1) %>% spread(year, i , fill=0)

fixed_model_2015_2018$year <- as.factor(fixed_model_2015_2018$year)
fixed_model_2015_2018$NAME_county <- as.factor(fixed_model_2015_2018$NAME_county)
fixed_model_2015_2018 <- fixed_model_2015_2018 %>% mutate(i=1) %>% spread(NAME_county, i, fill=0)
fixed_model_2015_2018 <- fixed_model_2015_2018 %>% mutate(i=1) %>% spread(year, i , fill=0)

# County Fixed Effects  - Will not include the coastal variable, 2010, and Alameda County
fixedmod_county_2010_2014 <- lm(log_house_values_estimate ~ . -`2010`-`Alameda County, California`
                                -Coastal -log_county_income_estimate, 
                                data=fixed_model_2010_2014)

fixedmod_county_2015_2018 <- lm(log_house_values_estimate ~ . -`2015`-`Alameda County, California`
                                -Coastal - log_county_income_estimate, 
                                data=fixed_model_2015_2018)

rob_se2 <- list(sqrt(diag(vcovHC(fixedmod_county_2010_2014, type='HC1'))), 
                sqrt(diag(vcovHC(fixedmod_county_2015_2018, type='HC1'))))

# Model' summary. Alameda county and 2010 year are excluded
stargazer(fixedmod_county_2010_2014, 
          fixedmod_county_2015_2018, digits = 3, se = rob_se, column.labels = c('2010-2014', '2015-2018'),
          type='html', out='Year_and_County_FixedEffects.html')

# Coastal Fixed Effects
fixedmod_coastal_2010_2014 <- lm(data = fixed_model_2010_2014, log_house_values_estimate ~ Coastal + median.age.estimate + 
                                   median.number.rooms.estimate + renter_occupied_perc + 
                                   log_county_income_estimate + 
                                   renter_occupied_perc * Coastal +
                                   log_county_income_estimate * Coastal +
                                   log_county_income_estimate * renter_occupied_perc +
                                   Coastal + year-1)

fixedmod_coastal_2015_2018 <- lm(data=fixed_model_2015_2018, 
                                 log_house_values_estimate ~ Coastal + median.age.estimate + 
                                   median.number.rooms.estimate + renter_occupied_perc + 
                                   log_county_income_estimate + 
                                   renter_occupied_perc * Coastal +
                                   log_county_income_estimate * Coastal +
                                   log_county_income_estimate * renter_occupied_perc + 
                                   Coastal + year-1)

# Robust standard errors list
rob_se3 <- list(sqrt(diag(vcovHC(fixedmod_coastal_2010_2014, type='HC1'))), 
                sqrt(diag(vcovHC(fixedmod_coastal_2015_2018, type='HC1'))))

# Model's Summary - 2015 and Coastal is excluded. 
stargazer(fixedmod_coastal_2010_2014, 
          fixedmod_coastal_2015_2018, digits = 3, se = rob_se, column.labels = c('2010-2014', '2015-2018'),
          type='html', out='Year_and_Coastal_FixedEffects.html')





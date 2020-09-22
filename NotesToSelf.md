# Where is the data
- In my dropbox 

# Pending Tasks
- Read about Fixed Effects

# Description of each .R file

#### Census Data Collection.R is 
- Using the Census API, it gathers data from the American Community Survey 5-year estimates for each CA census tracts from the year 2010 to 2018. 
  
  Here are the following variables:
  
  ###### Dependent or Response Varibale
  <b>median.housing.value (B25077_001)</b>: Estimate of the median housing values
  
  ###### Independent Variables
  <b>median.household.income (B19013_001)</b>: Estimate of the median household income in the past 12 months (IN 2018 Inflation-adjusted dollars)
  
  <b>population (B02001_001)</b>: Total Population
  
  <b>total.owner.occupied.housing.units (B25012_002)</b>: Total Owner-occupied housing units
  
  <b>total.renter.occupied.housing.units (B25012_010)</b>: Total Renter-occupied housing units
  
  <b>median.age (B01002_001)</b>: Median Age
  
  <b>median.number.rooms (B25021_002)</b>: Median number of rooms in Owner occupied househols
  
  <b>median.year.built (B25037_002)</b>: Median year structure built - Owner occupied
  
  
  ### Scatter Plot Matrix
  Link: https://public.tableau.com/profile/ivan.patel#!/vizhome/ScatterPlotMatrix-HousingValuesandothervaribales-CACensusTracts2010-2018/Dashboard1?publish=yes
  
  The goal of this scatter plot matrix is to check whether or not there is a linear relationship between median housing values and the independent variables, and whether or not the some independent variables are related with one another. 
  
#### EDA.rmd
Contains the RMarkdown code used to create the html file found here: https://rpubs.com/patelivan74/664522

 
  
  
  
  
  
  
  
  
  
  
  
   

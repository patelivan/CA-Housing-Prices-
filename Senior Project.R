library(sf)
library(tidyverse)
library(dplyr)
library(mapview)

# Read the CA Data and CA coastline data - Make sure they both have the same CRS as CA Data
# Then, write CA coastline data to desktop. Please save the lines of code where you chage the CRS to github - data collection.R
# In ArcGIS Pro, generate the near table or just do the near analysis. 
# If the units still don't make sense, look up the issue. 

ca_data <- st_read("C:/Users/bhumi/Desktop/CA Data/ca_data.shp")
ca_coastline <- st_read("C:/Users/bhumi/Desktop/Coastline_data/coastn83.shp")

house_crs <- st_crs(ca_data, asText = T)
coastline_crs <- st_crs(ca_coastline, asText = T)

# Do both datasets have the same CRS?
house_crs == coastline_crs

# transform ca_coastline's crs so that both sf objects have the same CRS.
ca_coastline <- st_transform(ca_coastline, crs = house_crs)

# Let's check the CRS of both objects again
st_crs(ca_data) == st_crs(ca_coastline)

# Always a good idea to plot the geometries. Is the data what we expected?
plot(st_geometry(ca_data))
plot(st_geometry(ca_coastline), add = T, col = 'red') # Yes!!

# Write the coastline data
write_sf(ca_coastline, 'ca_coastline.shp')

# Allthough writing the data generates over 50 warnings, data that was read in the beginning and the data that was written are identical!. 
coast <- st_read("C:/Users/bhumi/Desktop/ca_coastline.shp")
identical(ca_coastline, coast)

# Centroids of each census tract
ca_data_centroid <- st_centroid(ca_data)

# The dataset is so large that it will take ArcGIS Pro at least 6 hours to compute the distances from each census tracts to CA's coastline. 
# Calculating distances in planar projection results in unknown units. And using a GEODESC projection crashed my computer. 

# Thus, the next best approach is to import a susbet of the data into ArcGIS Pro, and calculate the needed distances to the coastline.  
# I split the dataset into different years, calculated the distances for all 9 years, and then rbinded them back to re-create the large dataset.
# This large datasets include two additional varibales, NEARDIST_ID, and NEARDIST which is calculated in miles. 

# ca_2010 <- ca_data %>% filter(year == 2010)
# st_write(ca_2010, 'ca_2010.shp')

# Read the subsets
ca_2018 <- st_read("C:/Users/bhumi/Desktop/CA Data/ca_2018.shp")
ca_2017 <- st_read("C:/Users/bhumi/Desktop/CA Data/ca_2017.shp")
ca_2016 <- st_read("C:/Users/bhumi/Desktop/CA Data/ca_2016.shp")
ca_2015 <- st_read("C:/Users/bhumi/Desktop/CA Data/ca_2015.shp")
ca_2014 <- st_read("C:/Users/bhumi/Desktop/CA Data/ca_2014.shp")
ca_2013 <- st_read("C:/Users/bhumi/Desktop/CA Data/ca_2013.shp")
ca_2012 <- st_read("C:/Users/bhumi/Desktop/CA Data/ca_2012.shp")
ca_2011 <- st_read("C:/Users/bhumi/Desktop/CA Data/ca_2011.shp")
ca_2010 <- st_read("C:/Users/bhumi/Desktop/CA Data/ca_2010.shp")

# Combining all the subsets 
final_data <- rbind(ca_2010, ca_2011, ca_2012, ca_2013, ca_2014, ca_2015, ca_2016, ca_2017, ca_2018)
st_write(final_data, 'ca_final.shp')


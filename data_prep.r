library(sf)
library(tidyverse)
library(dplyr)

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

# Let's check the CRS of both object again
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

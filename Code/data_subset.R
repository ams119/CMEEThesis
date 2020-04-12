#!/bin/env Rscript
# Author: Anne Marie Saunders
# Script: data_subset.R
# Desc: Using ff to load in giant data and subset to a more workable size
# Arguments: ../../Hefty_Data/vectdyn_fulldata.csv
# Date: 04/02/19

# Load in packages
library(sf)
library(rgdal)
library(maps)
library(sp)
library(readr)
library(tidyverse)
library(ff)
library(ffbase)
library(tmap)
library(tmaptools)

# Load in a sample of the vectdyn data
sample = read.csv("../../Hefty_Data/vectdyn_fulldata.csv", header = T, nrows = 500)

# Find the classes of each column 
sampleclasses = sapply(sample, class)
sampleclasses
# Latitude and Longitude are both numeric- switch to factor to better deal with data type errors
classesswitch = sampleclasses
classesswitch[10:11] = "factor"

# Read in full data frame as ff data frame with column classes specified
# Time because it's fun
start = Sys.time()
df1 = read.csv.ffdf(file = "../../Hefty_Data/vectdyn_fulldata.csv", sep = ",", colClasses = classesswitch)
end = Sys.time()
end-start

# Pull out location-related columns
Latitudes = as.numeric(levels(df1$Latitudes)[df1$Latitudes[]])
Longitudes = as.numeric(levels(df1$Longitudes)[df1$Longitudes[]])
Locations = as.character(levels(df1$Locations)[df1$Locations[]])
points = data.frame(Locations, Longitudes, Latitudes)

# Pull out only distinct locations of trap data
locs <- distinct(points, Locations, Longitudes, Latitudes)

# Set the crs
project_crs <- "+proj=longlat +WGS84 (EPSG: 4326) +init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

# Create locs as sf object
locs = st_as_sf(locs, coords = c("Longitudes", "Latitudes"), crs = project_crs, agr = "constant")

rm(points)

# Obtain USA state map from maps package
# FIX_ME this is not working
usa = st_as_sf(map("state", fill = TRUE, plot = FALSE))
usa = st_set_crs(usa, project_crs)

# Obtain Florida from the USA map
state = subset(usa, grepl("florida", usa$ID))
usa <- st_buffer(usa, 0)

# Plot locations 
tm_shape(usa) +
  tm_polygons() + 
  tm_shape(locs) +
  tm_dots()

tm_shape(state) +
  tm_polygons() + 
  tm_shape(locs) +
  tm_dots()

# Let's say we just want the points from Florida:
locs_fd = locs[state,]

tm_shape(usa) +
  tm_polygons() + 
  tm_shape(locs_fd) +
  tm_dots()

# Now find which study sites are relevant to this subset
sites = unique(locs_fd$loc)

# Pull data points matching these sites from ffdf
fd_data = df1[df1$Locations %in% sites,]

# Save as csv
write.csv(fd_data, "../Data/fdcounts.csv", row.names = F)




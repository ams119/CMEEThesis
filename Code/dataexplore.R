#!/bin/env Rscript
# Author: Anne Marie Saunders
# Script: dataexplore.R
# Desc: Just practice using ff package for large data
# Arguments: ../../../vectdyn_fulldata.csv
# Date: 04/02/19

# Load in ff package
library(ff)
library(ffbase)
library(sf)
library(rgdal)
library(maps)
library(ggplot2)
library(sp)

# Find column classes: first read in a sample of the data
sample = read.csv("../../Hefty_Data/vectdyn_fulldata.csv", header = T, nrows = 500)

# Find the classes of each column 
sampleclasses = sapply(sample, class)
sampleclasses
# Latitude and Longitude are both numeric- switch to factor to better deal with messups
classesswitch = sampleclasses
classesswitch[10:11] = "factor"

# Read in data frame specifying columns to classes
# Time because it's fun
start = Sys.time()
df1 = read.csv.ffdf(file = "../../Hefty_Data/vectdyn_fulldata.csv", sep = ",", colClasses = classesswitch)
end = Sys.time()
end-start

# Extract some walton data for play
df2 = df1[df1$Locations == "Walton (Florida (United States))",]
spots = as.character(df2$Locations[])
indx = which(spots == "Walton (Florida (United States))")
walton = df2[indx,]
write.csv(walton, "../Data/waltondata.csv", row.names = F)

# Create numeric vectors of just lat/long columns
lats = as.numeric(levels(df1$Latitudes)[df1$Latitudes[]])
longs = as.numeric(levels(df1$Longitudes)[df1$Longitudes[]])
indx = seq(1:nrow(df1))
loc = as.character(levels(df1$Locations)[df1$Locations[]])
points = data.frame(indx, longs, lats, loc)

# # Create numeric vectors from lat and long columns
# df1$latnum = as.ff(as.numeric(levels(df1$Latitudes)[df1$Latitudes[]]))
# df1$longnum = as.ff(as.numeric(levels(df1$Longitudes)[df1$Longitudes[]]))
# 
# # Get just the Florida Data
# # Start with a rough chop: 24.6-31.3* latitude and -79.5- -87.9* longitude
# # Select for Latitude
# florida = df1[df1$latnum < 31.3,]
# florida = florida[florida$latnum > 24.3,]
# 
# # Select for Longitude
# florida = florida[florida$longnum < -79.5,]
# florida = florida[florida$longnum > -87.9,]

# Isolate from points frame
florida = points[points$lats < 31.3,]
florida = florida[florida$lats > 24.3,]

florida = florida[florida$longs < -79.5,]
florida = florida[florida$longs > -87.9,]

# Obtain florida from maps package
usa = st_as_sf(map("state", fill = TRUE, plot = FALSE))
state = subset(usa, grepl("florida", usa$ID))

# Set project crs
project_crs <- "+proj=longlat +WGS84 (EPSG: 4326) +init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
state = st_set_crs(state, project_crs)

# take first a sample of points within florida
fd.sample = florida[1:1000,]
row.names(fd.sample) = NULL

# Add a point that will be clearly outside FD
fd.sample = rbind(fd.sample, c(50000000, -85.22, 31.2))

# Make sf object
pointssf <- st_as_sf(fd.sample, coords = c("longs", "lats"))

# Set crs
pointssf = st_set_crs(pointssf, project_crs)

#pointsdf = spTransform(pointsdf, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# st_crs(pointssf) = "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

#pointsdf = SpatialPointsDataFrame(data.frame(fd.sample$longs,fd.sample$lats), 
#                                  data = fd.sample, proj4string = CRS("+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))


# Plot the state
plot(state, asp = 1)
plot(pointssf, pch = 19, col = "red", add = TRUE, cex = 1, asp = 1)

# plot(pointssf, pch = 19, col = "red", cex = 0.5, asp = 1)
# plot(state, add = TRUE)
# points(points.sample$longs, points.sample$lats, pch = 19, col = "red")
# plot(usa)

ggplot(data = state) +
  geom_sf() +
  geom_point(data = fd.sample, aes(x = longs, y = lats), size = 0.8, 
             shape = 23, fill = "red") +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE) + theme_bw()


# Obtain County Level
counties = st_as_sf(map("county", plot = FALSE, fill = T, interior = TRUE))
counties = subset(counties, grepl("florida", counties$ID))
plot(counties)
st_write(counties, "../../Hefty_Data/fdcounties.shp")

# Create the state boundary by making a union of counties
state = st_union(counties[-67])

# Cut only Florida points from df1

#Plot
ggplot(data = state) +
  geom_sf() +
  geom_point(data = florida, aes(x = longnum, y = latnum), size = 4, 
             shape = 23, fill = "darkred")
#+ coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

plot(state)
points(longs, lats, col = "red")



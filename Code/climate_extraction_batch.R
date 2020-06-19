# ---
# title: "climate_extraction_batch.R"
# author: "Matthew J Watts, adapted by Anne Marie Saunders"
# date: "18/04/2020"
# output: html_document
# ---
#   
#   ```{r setup, include=FALSE}
# knitr::opts_chunk$set(echo = TRUE)
# ```
# 
# ## GENERAL GUIDELINES
# 
# CPC Global Temperature data provided by the NOAA/OAR/ESRL PSD, Boulder, Colorado, USA, from their Web site at https://www.esrl.noaa.gov/psd/ 
#   
#   In order to download the maximum daily temperature in degrees celsius (tmax) datasets (in order for workflow to complete), please follow the link below: 
#   
#   Index of ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_temp/
#   
#   Store these netCDF files in a directory:
#   data/tmax/
#   
#   
#   CPC Global Unified Precipitation (inches) data provided by the NOAA/OAR/ESRL PSD,	Boulder, Colorado, USA, from their Web site at https://www.esrl.noaa.gov/psd/
#   
#   In order to download the daily precipitation datasets (in order for workflow to complete), please follow the link below: 
#   ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_precip/
#   
#   Store these netCDF files in a directory:
#   data/precip/
#   
#   
#   ```{r}

library(tidyverse)
library(lubridate)
library(readr)
library(sf)
library(rgdal)
library(maps)
library(ggplot2)
library(sp)

project_crs <- "+proj=longlat +WGS84 (EPSG: 4326) +init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

################################################################################
#Import VectorBase Dataset
################################################################################

# load abundance data and adjust colnames to match Matt's

print(paste("Now starting climate extraction for", as.character(vector_abun$Location[1])))

# Adjust column names to match Matt's style
colnames(vector_abun) = gsub("\\.", " ", colnames(vector_abun))

# Extract the year from each date
vector_abun$Year <- format(as.Date(vector_abun$`Collection date range`), "%Y")

# Clear the workspace
rm(list=setdiff(ls(), c("vector_abun", "project_crs", "p", "counties", "fddata")))



###########################################################################################################
#You can download this data set from here https://gadm.org/, really useful for regional data visualization
###########################################################################################################

# gadm <- st_read("F:/OneDrive - Imperial College London/DataSets/GeoData/GeoDataAdminUnits/gadm28.shp/gadm28.shp")
# 
# walton_shp <- dplyr::filter(gadm, NAME_1 == "Florida" & NAME_2 == "Walton") 
# 
# library(tmap)
# tm_shape(walton_shp) +
#   tm_polygons() + 
#   tm_shape(vector_abun) +
#   tm_dots()
# 

###################################################################################################################
#EXTRACTION TMAX - Note that the CPC climate data spans from 1979 to 2019,remove observations outside of this range
###################################################################################################################
#Load packages
library(RNetCDF)
library(raster)
library(rgdal)
library(sf)
library(sp)

project_crs <- "+proj=longlat +WGS84 (EPSG: 4326) +init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

# create a vector of years in the data set (we use this to select tmax files)
years <- dplyr::distinct(vector_abun, Year)
years <- as.vector(years$Year)


# get unique trap locations and create as sf object
traps <- dplyr::distinct(vector_abun, Latitudes, Longitudes)
traps = st_as_sf(traps, coords = c("Longitudes", "Latitudes"), crs = project_crs, remove = FALSE)

# Set the working directory to point to the tmax.nc files
setwd("~/Documents/Hefty_Data/tmax")

# create start message
print (paste(Sys.time(),"start"))

# count the number of rows in the traps dataframe so we can create an empty df
df_count <- NROW(traps)

# Create a new dataframe with the same number of rows as there are traps 
tmaxAll <- as.data.frame(matrix(0, ncol = 0, nrow = df_count))

# Copy the geometry to the dataframe
tmaxAll$Latitudes <- traps$Latitudes
tmaxAll$Longitudes <- traps$Longitudes

# loop through each year in the years vector and look for matching file names in the Wd
for (y in 1:length(years))
{
  # create file pattern
  pat <-paste("tmax.",years[y], sep="")
  
  year <- years[y]
  
  #list the files matching the pattern
  listfiles <- list.files(pattern = pat)
  
  # create a loop that runs through every matching file in the wb
  for(f in listfiles)
  {
    
    # create a raster brick, each file in the brick will represent a day of temperature sampling
    tmax = brick(f) ## this is where tmp data folder is being filled up
    
    # rotate the raster brick to convert to conventional -180 to 180 longitudes
    tmax <- rotate(tmax) # took 1 gig
    # replace -999 values with NA 
    tmax <- reclassify(tmax, cbind(-999, NA))
    # create a day variable from the brick from each layer, representing each day in the year 
    days <- nlayers(tmax) 
    # ensure the Coordinate Reference System in the temp data and trap sites are matching
    shp = st_transform(traps, crs(tmax))
    #Extract data from each layer in the brick
    for (i in 1:nlayers(tmax))
    {
      print (paste(Sys.time(),"extracting-", "dateset:", f, ",year",year, ",Day:",i))
      #print(c(i, f))
      tmaxEx <- raster::extract(tmax[[i]], 
                                as_Spatial(shp),
                                method = 'bilinear', 
                                fun = mean,
                                na.rm = T)
      # create a dataframe out of the extracted data 
      tmaxexdf <- as.data.frame(tmaxEx)
      # name the column name by year
      colnames(tmaxexdf) <- paste(year, i, sep = "-")
      # bind newly created dataframe to the tmaxall, each loop in the cycle will create a new column
      tmaxAll <- cbind(tmaxAll,tmaxexdf)
    }
    
    # Find raster files in tmp directory
    files_to_remove = list.files("/tmp/", pattern = "\\.gri", recursive = T, full.names = T)
    files_to_remove = c(files_to_remove,
                        list.files("/tmp/", pattern = "\\.grd", recursive = T, full.names = T))
    
    # remove these unneeded raster files
    file.remove(files_to_remove)
  }}

# convert the dataset to the correct format 
max_temp <- tmaxAll %>% gather(date, max_temp, -Latitudes, -Longitudes)

# Now we have temperature for every day of each trap location

print (paste(Sys.time(),"finished Run tmax"))

# if you are dealing with a big dataset it would be a good idea to save this as a csv and re import it later

write.csv(max_temp, file = "max_temp.csv", row.names=FALSE)

rm(list=setdiff(ls(), c("vector_abun", "project_crs", "p", "fddata", "counties")))


################################################################################
#EXTRACTION Precip - same steps as above
################################################################################
#Load packages
library(RNetCDF)
library(raster)
library(rgdal)
library(sf)
library(sp)

project_crs <- "+proj=longlat +WGS84 (EPSG: 4326) +init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

# create a vector of years in the data set 
#years <- st_set_geometry(vector_abun, NULL)
years <- dplyr::distinct(vector_abun, Year)
years <- as.vector(years$Year)

# get unique trap locations and create as sf object
traps <- dplyr::distinct(vector_abun, Latitudes, Longitudes)
traps = st_as_sf(traps, coords = c("Longitudes", "Latitudes"), crs = project_crs, remove = FALSE)

# Set the working directory to point to the tmax.nc files
setwd("~/Documents/Hefty_Data/precip")

print (paste(Sys.time(),"start"))

df_count <- NROW(traps)

precAll <- as.data.frame(matrix(0, ncol = 0, nrow = df_count))

precAll$Latitudes <- traps$Latitudes

precAll$Longitudes <- traps$Longitudes

for (y in 1:length(years))
{
  pat<-paste("precip.",years[y], sep="")
  year <- years[y]
  listfiles <- list.files(pattern = pat)
  for(f in listfiles)
  {
    precip = brick(f)
    precip <- rotate(precip)
    precip <- reclassify(precip, cbind(-999, NA))
    days <- nlayers(precip) 
    shp = st_transform(traps, crs(precip))
    for (i in 1:nlayers(precip))
    {
      print (paste(Sys.time(),"extracting-", "dateset:", f, ",year",year, ",Day:",i))
      precipEx <- raster::extract(precip[[i]], 
                                  as_Spatial(shp),
                                  method = 'bilinear',
                                  fun = mean,
                                  na.rm = T)
      precipexdf <- as.data.frame(precipEx)
      colnames(precipexdf) <- paste(year, i, sep = "-")
      precAll <- cbind(precAll,precipexdf)
    }
    # Find raster files in tmp directory
    files_to_remove = list.files("/tmp/", pattern = "\\.gri", recursive = T, full.names = T)
    files_to_remove = c(files_to_remove,
                        list.files("/tmp/", pattern = "\\.grd", recursive = T, full.names = T))
    
    # remove these unneeded raster files
    file.remove(files_to_remove)
  }}
precip <- precAll %>% gather(date, precip, -Latitudes, -Longitudes)

print (paste(Sys.time(),"finished Run precip"))

rm(list=setdiff(ls(), c("vector_abun", "project_crs", "precip", "p", "fddata", "counties")))

# Read temp data back in
max_temp = read.csv("~/Documents/Hefty_Data/tmax/max_temp.csv", header = T, stringsAsFactors = F)

# join climate datasets together by lat / long and date
clim_df <- dplyr::inner_join(max_temp, precip, by = c("Latitudes", "Longitudes", "date"))

#convert year-Day of the year into standard date format
clim_df$date <- as.Date(clim_df$date, format="%Y-%j")

# Ensure date is formatted as date
vector_abun$`Collection date range` = as.Date(vector_abun$`Collection date range`)
colnames(vector_abun)[colnames(vector_abun) == 'date'] <- 'Collection date range'

vector_abun_clim_time_series <-
  vector_abun %>%
  dplyr::rename(date = `Collection date range`)  %>%
  dplyr::select(Species, Latitudes, Longitudes, date, Year, Locations, `Specimens collected`)  %>%
  dplyr::right_join(clim_df, by = c("Latitudes" = "Latitudes", "Longitudes" = "Longitudes", "date" = "date"))

### Visualize
locale = na.omit(vector_abun_clim_time_series$Locations)[1]
locale = sub("\\(.*", "", locale)
  
ggplot(vector_abun_clim_time_series, aes(x=date, y=`Specimens collected`)) +
  geom_line() + ggtitle(paste(locale, ", Abundance", sep = ""))

ggplot(vector_abun_clim_time_series, aes(x=date, y=max_temp)) +
  geom_line() + ggtitle(paste(locale, ", Temp", sep = ""))

ggplot(vector_abun_clim_time_series, aes(x=date, y=precip)) +
  geom_line() + ggtitle(paste(locale, ", Precip", sep = ""))

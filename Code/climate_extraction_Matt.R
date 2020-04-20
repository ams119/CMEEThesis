# ---
# title: "Extracting climate data from spatial points"
# author: "Matthew J Watts (adapted from work written by Deraj Wilson-Aggarwal"
# date: "16/03/2020"
# output: html_document
# ---
#   
# ```{r setup, include=FALSE}
# knitr::opts_chunk$set(echo = TRUE)
# ```
# 
# ## GENERAL GUIDELINES
# 
# CPC Global Temperature data provided by the NOAA/OAR/ESRL PSD, Boulder, Colorado, USA, from their Web site at https://www.esrl.noaa.gov/psd/ 
#   
# In order to download the maximum daily temperature in degrees celsius (tmax) datasets (in order for workflow to complete), please follow the link below: 
#   
# Index of ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_temp/
#   
# Store these netCDF files in a directory:
# data/tmax/
#   
#   
# CPC Global Unified Precipitation (inches) data provided by the NOAA/OAR/ESRL PSD,	Boulder, Colorado, USA, from their Web site at https://www.esrl.noaa.gov/psd/
#   
# In order to download the daily precipitation datasets (in order for workflow to complete), please follow the link below: 
# ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_precip/
#   
# Store these netCDF files in a directory:
# data/precip/

library(tidyverse)
library(lubridate)


################################################################################
#Import VectorBase Dataset
################################################################################

library(readr)
# load abundance data

fddata = read.csv("~/Documents/Hefty_Data/mosquitoAbun/fdcounts2.csv")

vector_abun <- fddata[fddata$Locations == "Walton (Florida (United States))",] # done: Hern, Collier, Manatee, Orange, St Johns, Lee

rm(list=setdiff(ls(), c("vector_abun")))

colnames(vector_abun) = gsub("\\.", " ", colnames(vector_abun))

########################################
# Convert to dates to the correct format
########################################

# Convert the date from a character data format to a date format. Note that the date "%Y-%m-%d") will need to be edited appropriately, depending on the temporal fidelity i.e. specific subset reported in days, month or weeks

vector_abun$date_conv <- format(as.Date(vector_abun$`Collection date range`), "%Y-%m-%d")
vector_abun$date_conv <- as.Date(vector_abun$`Collection date range`)

####################
# Visualise the data 
####################

# Create a year field from the dates, we will use this later for the climate extraction
vector_abun$year <- format(as.Date(vector_abun$`Collection date range`), "%Y")

# We will only select a subset of data to speed the process up
vector_abun <- dplyr::filter(vector_abun, year %in% c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"))

ggplot(vector_abun, aes(x=date_conv, y=`Specimens collected`)) +
  geom_line() + 
  xlab("") + 
  scale_x_date(date_labels = "%Y")

#```

# As you can observe from this plot, mosquito 
# data is only being collected part of the #year, which will have implication for your 
# analysis later on.

#```{r}

###################################################################################################################
#EXTRACTION TMAX - Note that the CPC #climate data spans from 1979 to 2019,
#remove observations outside of this range
###################################################################################################################
#Load packages
library(RNetCDF)
library(raster)
library(rgdal)
library(sf)
library(sp)

project_crs <- "+proj=longlat +WGS84 (EPSG: 4326) +init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

# create a vector of years in the data set (we use this to select tmax files)
years <- dplyr::distinct(vector_abun, year)
years <- as.vector(years$year)

# get unique trap locations
traps <- dplyr::distinct(vector_abun, Latitudes, Longitudes)

# get unique trap locations
traps = st_as_sf(traps, coords = c("Longitudes", "Latitudes"), crs = project_crs,  remove = FALSE)

# Set the working directory to point to the tmax.nc files
setwd("~/Documents/Hefty_Data/tmax")

# create start message
print (paste(Sys.time(),"start"))

# count the number of rows in the traps dataframe so we can create an empty df
df_count <- NROW(traps)

# Create a new dataframe with the same number of rows as there are traps 
tmaxAll <- data.frame(matrix(0, ncol = 0, nrow = df_count))

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
    tmax = brick(f)
    # rotate the raster brick to convert to conventional -180 to 180 longitudes
    tmax <- rotate(tmax)
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
                                fun = mean,
                                na.rm = T)
      # create a dataframe out of the extracted data 
      tmaxexdf <- as.data.frame(tmaxEx)
      # name the column name by year
      colnames(tmaxexdf) <- paste(year, i, sep = "-")
      # bind newly created dataframe to the tmaxall, each loop in the cycle will create a new column
      tmaxAll <- cbind(tmaxAll,tmaxexdf)
      
    }
  }}

# convert the dataset to the correct format 
max_temp <- tmaxAll %>% gather(date, max_temp, -Latitudes, -Longitudes)

# Now we have temperature for every day of each trap location

print (paste(Sys.time(),"finished Run tmax"))

# if you are dealing with a big dataset it would be a good idea to save this as a csv and re import it later
write.csv(max_temp, file = "~/Documents/Hefty_Data/max_temp.csv", row.names=FALSE)

# Remove everything from the environment besides input and output
#rm(list=setdiff(ls(), c("vector_abun", "max_temp")))
rm(list=setdiff(ls(), c("vector_abun")))

################################################################################
#EXTRACTION Precip - same steps as above
################################################################################
library(RNetCDF)
library(raster)
library(rgdal)
library(sf)
library(sp)

project_crs <- "+proj=longlat +WGS84 (EPSG: 4326) +init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

# create a vector of years in the data set (we use this to select precip files)
years <- dplyr::distinct(vector_abun, year)
years <- as.vector(years$year)

# get unique trap locations
traps <- dplyr::distinct(vector_abun, Latitudes, Longitudes)

# Convert long / lats to to an sf sptatial object for precip extraction
traps = st_as_sf(traps, coords = c("Longitudes", "Latitudes"), crs = project_crs,  remove = FALSE)

setwd("~/Documents/Hefty_Data/precip")

#Load packages
library(RNetCDF)
library(raster)
library(rgdal)

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
    days <- nlayers(precip) 
    shp = st_transform(traps, crs(precip))
    for (i in 1:nlayers(precip))
    {
      print (paste(Sys.time(),"extracting-", "dateset:", f, ",year",year, ",Day:",i))
      precipEx <- raster::extract(precip[[i]], 
                                  as_Spatial(shp),
                                  fun = mean,
                                  na.rm = T)
      precipexdf <- as.data.frame(precipEx)
      colnames(precipexdf) <- paste(year, i, sep = "-")
      precAll <- cbind(precAll,precipexdf)
      
    }
  }}

precip <- precAll %>% gather(date, precip, -Latitudes, -Longitudes)

print (paste(Sys.time(),"finished Run precip"))

rm(list=setdiff(ls(), c("vector_abun", "precip")))

#######################################
# Check the climate data
#######################################

# Read temp data back in
max_temp = read.csv("~/Documents/Hefty_Data/max_temp.csv", header = T, stringsAsFactors = F)

# Get summaries
summary(max_temp)
summary(precip)

####################
# Visualise the data 
####################

ggplot(max_temp, aes(x=date, y=max_temp)) +
  geom_line()

ggplot(precip, aes(x=date, y=precip)) +
  geom_point() 

########################################################################
#Join the climate data to main dataset
#########################################################################

# join climate datasets together by lat / long and date
clim_df <- dplyr::inner_join(max_temp, precip, by = c("Latitudes", "Longitudes", "date"))


#convert year-Day of the year into standard date format
clim_df$date <- as.Date(clim_df$date, format="%Y-%j")


# For the sake of this tutorial we will simplify the 
# abundance dataset - consider now that we have two data sets, 
# we need to join them together. This could be done it in 
# different ways depending on what analysis we want to achieve  
# i.e. a standard linear regression or time series analysis. 

# For a linear regression we can join the matching mosquito 
# abundance observations to the matching climate observations.

vector_abun_clim <-
  vector_abun %>%
  dplyr::rename(date = date_conv)  %>%
  dplyr::select(Species, Latitudes, Longitudes, date, year, Locations, `Specimens collected`)  %>%
  dplyr::left_join(clim_df, by = c("Latitudes", "Longitudes", "date"))



# If we are creating a time series we might want to join 
# the mosquito abundance observations to the matching 
# climate observations, but instead we keep all climate observations 
# instead of dropping the non-matching ones. 
# We then might interpolate mosquito observations. 

vector_abun_clim_time_series <-
  vector_abun %>%
  dplyr::rename(date = date_conv)  %>%
  dplyr::select(Species, Latitudes, Longitudes, date, year, Locations, `Specimens collected`)  %>%
  dplyr::right_join(clim_df, by = c("Latitudes" = "Latitudes", "Longitudes" = "Longitudes", "date" = "date"))

# Inspect datasets by plotting them

ggplot(vector_abun_clim, aes(x=date, y=`Specimens collected`)) +
  geom_line()

ggplot(vector_abun_clim, aes(x=date, y=max_temp)) +
  geom_line()

ggplot(vector_abun_clim, aes(x=date, y=precip)) +
  geom_line()

ggplot(vector_abun_clim_time_series, aes(x=date, y=`Specimens collected`)) +
  geom_line() 

ggplot(vector_abun_clim_time_series, aes(x=date, y=max_temp)) +
  geom_line() 

ggplot(vector_abun_clim_time_series, aes(x=date, y=precip)) +
  geom_line() 


filename1 = paste("~/Documents/Hefty_Data/Extracted_Data/", as.character(vector_abun_clim$Locations[1]),
                  "_clim_TS.csv", sep = "")
filename2 = paste("~/Documents/Hefty_Data/Extracted_Data/", as.character(vector_abun_clim$Locations[1]),
                  "_clim.csv", sep = "")
write.csv(vector_abun_clim_time_series, file = filename1, row.names = F)
write.csv(vector_abun_clim, file = filename2, row.names = F)

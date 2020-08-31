#!/bin/env Rscript
# Author: Anne Marie Saunders
# Script: floridats.R
# Desc: extract time series of climate data for a group of locations
# Arguments: ../../../fdcounts_cleaned.csv
# Date: 04/16/19

# Read in abundace dataset
fddata = read.csv("../Data/fdcounts_cleaned.csv", header = TRUE, stringsAsFactors = FALSE)

# Create vector of locations
counties = unique(fddata$Locations)

# Create folder to store extracted time series
dir.create("../Data/Extracted_Data/")

# Extract climate for each location and save time series as csv
for(p in 1:length(counties)){
  # Subset to a single location
  vector_abun = fddata[fddata$Locations == counties[p],]
  
  # Run climate extraction for this location
  source("climate_extraction.R")
  
  # Define filename for csv
  filename = paste("../Data/Extracted_Data/", locale, "_clim_TS.csv", sep = "")
  
  # Save csv
  write.csv(vector_abun_clim_time_series, file = filename, row.names = FALSE)
  
  # Remove almost everything from the environment
  rm(list=setdiff(ls(), c("p", "counties", "fddata")))
}



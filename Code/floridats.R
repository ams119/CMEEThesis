#!/bin/env Rscript
# Author: Anne Marie Saunders
# Script: floridats.R
# Desc: extract time series of climate data for a group of locations
# Arguments: ../../../fdcounts2.csv
# Date: 04/16/19

# Read in abundace dataset
fddata = read.csv("~/Documents/Hefty_Data/mosquitoAbun/fdcounts2.csv", header = T, stringsAsFactors = F)

# Create vector of locations
counties = unique(fddata$Locations)

# Extract climate for each location and save time series as csv
for(p in 1:length(counties)){
  vector_abun = fddata[fddata$Locations == counties[p],]
  source("~/Documents/CMEEThesis/Code/climate_extraction_batch.R")
  filename = paste("~/Documents/Hefty_Data/Extracted_Data/", locale,
                    "_clim_TS.csv", sep = "")
  write.csv(vector_abun_clim_time_series, file = filename, row.names = F)
  rm(list=setdiff(ls(), c("p", "counties", "fddata")))
}

# Check that csvs look as expected
# Add in whatever checks desired

for(i in 1:length(counties)){
  d = read.csv(paste0("~/Documents/Hefty_Data/Extracted_Data/", counties[i], "_clim_TS.csv"), nrows = 20000, header = T, stringsAsFactors = F)
  cat(paste("\n", counties[i], "\n"))
  str(d)
  print(quantile(d$precip))
}

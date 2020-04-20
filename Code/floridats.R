#!/bin/env Rscript
# Author: Anne Marie Saunders
# Script: floridats.R
# Desc: extract time series of climate data for a group of locations
# Arguments: ../../../vectdyn_fulldata.csv
# Date: 04/16/19

fddata = read.csv("~/Documents/Hefty_Data/mosquitoAbun/fdcounts.csv", header = T, stringsAsFactors = F)

counties = unique(fddata$Locations)
i = 1
for(i in 1:length(counties)){
  print(counties[i])
  vector_abun = fddata[fddata$Locations == counties[i],]
  source("~/Documents/CMEEThesis/Code/climate_extraction_Matt.R")
  filename = paste("~/Documents/CMEEThesis/Data/", counties[i], ".csv", sep = "")
  write.csv(vector_abun, file = filename, row.names = F) # change to time series
  rm(list=setdiff(ls(), c("i", "counties", "fddata")))
}

# correct extraction to save all necessary objects
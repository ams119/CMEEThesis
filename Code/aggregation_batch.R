#!/bin/env Rscript
# Author: Anne Marie Saunders
# Script: aggregation_batch.R
# Desc: Use aggregation script on a set of datasets
# Arguments: 
# Date: 04/27/20

#library(gridExtra)

## Begin loop to aggregate files
files = list.files(path = "../Data/Extracted_Data/", pattern = "_TS.csv")

for(i in 1:length(files)){
  cat(paste("Now aggregating data for", sub(".csv", "", files[i]), "\n"))
  mapped = read.csv(paste0("../Data/Extracted_Data/", files[i]), header = T, stringsAsFactors = F)
  source("aggregation.R")
  write.csv(weekly, paste0("../Data/Extracted_Data/Aggregated/", locale, "_weekly.csv"), row.names = F)
  write.csv(biweekly, paste0("../Data/Extracted_Data/Aggregated/", locale, "_biweekly.csv"), row.names = F)
  write.csv(monthly, paste0("../Data/Extracted_Data/Aggregated/", locale, "_monthly.csv"), row.names = F)
  
  ### Plot ###
  setwd("../Results/AggByLocation/")
  
  # Create a directory to store plots
  dir.create(paste0("../Results/AggByLocation/",locale))
  directory = paste0("../Results/AggByLocation/", locale)
  
  ## Create plots of all species aggregated at weekly, biweekly, and monthly time scales ## 
  
  # Weekly
  jpeg(paste0(directory, "/aallspecs_weekly.jpeg"))
  print(ggplot(weekly, aes(x=date_dw, y = `Specimens.collected`)) +
      geom_line(col = "darkgreen") + xlab("") +
      scale_x_date(date_labels = "%Y") +
      ggtitle(paste0(locale, ", all species, Weekly Mean")) +
      theme_bw() + labs(y = "Mosquitoes Collected",
                        x = "Time"))
  dev.off()
  
  ## Biweekly
  
  # Create manual tick marks and axis labels
  brks = seq(0, 27 * length(yrs), by = 27)
  next_year = as.character(as.numeric(yrs[length(yrs)]) + 1)

  jpeg(paste0(directory, "/aallspecs_biweekly.jpeg"))
  print(ggplot(biweekly, aes(x=ids, y = `Specimens.collected`)) +
    geom_line(col = "blue") + xlab("") +
    scale_x_continuous("Time", breaks = brks, labels = c(yrs, next_year)) +
    ggtitle(paste(locale,"all species, Biweekly Mean")) +
    theme_bw() + labs(y = "Mosquitoes Collected"))
  dev.off()
  
  # Monthly 
  jpeg(paste0(directory, "/aallspecs_monthly.jpeg"))
  print(ggplot(monthly, aes(x=date_dm, y = `Specimens.collected`)) +
    geom_line(col = "purple") + xlab("") +
    scale_x_date(date_labels = "%Y") +
    ggtitle(paste0(locale, " all species, Monthly Mean")) +
    theme_bw() + labs(y = "Mosquitoes Collected",
                     x = "Time"))
  dev.off()
  
  # Loop through each species, making 3 plots for each
  for(j in 1:length(species)){
    
    name = species[j]
    
    # Weekly
    jpeg(paste0(directory, "_", name, "_weekly.jpeg"))
    print(ggplot(weekly, aes_string(x= "date_dw", y = name)) +
      geom_line(col = "darkgreen") + xlab("") +
      scale_x_date(date_labels = "%Y") +
      ggtitle(paste(locale, name, "Weekly")) +
      theme_bw() + labs(y = "Mosquitoes Collected",
                        x = "Time"))
    dev.off()
    
    # Bi-weekly
    jpeg(paste0(directory, "_", name, "_biweekly.jpeg"))
    print(ggplot(biweekly, aes_string(x= "ids", y = name)) +
      geom_line(col = "blue") + xlab("") +
      scale_x_continuous("Time", breaks = brks, labels = c(yrs, next_year)) +
      ggtitle(paste(locale, name, "Biweekly")) +
      theme_bw() + labs(y = "Mosquitoes Collected"))
    dev.off()
    
    # Monthly
    jpeg(paste0(directory, "_", name, "_monthly.jpeg"))
    print(ggplot(monthly, aes_string(x="date_dm", y = name)) +
      geom_line(col = "purple") + xlab("") +
      scale_x_date(date_labels = "%Y") +
      ggtitle(paste(locale, name, "Monthly")) +
      theme_bw() + labs(y = "Mosquitoes Collected",
                        x = "Time"))
    dev.off()
  }
  
  rm(list=setdiff(ls(), c("i", "files", "continue")))
}


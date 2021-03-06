#!/bin/env Rscript
# Author: Anne Marie Saunders
# Script: aggregation_batch.R
# Desc: Use aggregation script on a set of datasets
# Arguments: 
# Date: 04/27/20

## Begin loop to aggregate files
files = list.files(path = "../Data/Extracted_Data/", pattern = "_TS.csv")

# Remove Collier data- only records 1 family group
files = files[-grep(pattern = "Collier", files)]

# Create a data frame for storing which species in each location were aggregated
morphological_aggregations = data.frame(rep(NA, length(files)), rep(NA, length(files)))
colnames(morphological_aggregations) = c("Location", "Species")

# Create directory for storing aggregated data files
dir.create("../Data/Extracted_Data/Aggregated/")

# For each county time series
for(i in 1:length(files)){
  cat(paste("Now aggregating data for", sub(".csv", "", files[i]), "\n"))
  
  # Read in the data
  mapped = read.csv(paste0("../Data/Extracted_Data/", files[i]), header = TRUE, stringsAsFactors = FALSE)
  
  # Aggregate for this location
  source("aggregation.R")
  
  # Record which morphological groups were grouped at this location
  morphological_aggregations[i,"Location"] = locale
  
  # Save csvs of monthly, bimonthly, and monthly data
  write.csv(weekly, paste0("../Data/Extracted_Data/Aggregated/", locale, "_weekly.csv"), row.names = FALSE)
  write.csv(biweekly, paste0("../Data/Extracted_Data/Aggregated/", locale, "_biweekly.csv"), row.names = FALSE)
  write.csv(monthly, paste0("../Data/Extracted_Data/Aggregated/", locale, "_monthly.csv"), row.names = FALSE)
  
  ### Plot ###

  # Create a directory to store plots
  dir.create(paste0("../Results/AggByLocation/",locale))
  directory = paste0("../Results/AggByLocation/", locale)
  
  ## Create plots of all species aggregated at weekly, biweekly, and monthly time scales ## 
  
  # Weekly
  jpeg(paste0(directory, "/aallspecs_weekly.jpeg"))
  print(ggplot(weekly, aes(x=date, y = `Specimens.collected`)) +
      geom_line(col = "darkgreen") + xlab("") +
      scale_x_date(date_labels = "%Y") +
      ggtitle(paste0(locale, ", all species, Weekly Mean")) +
      theme_bw() + labs(y = "Mosquitoes Collected",
                        x = "Time"))
  dev.off()
  
  ## Biweekly
  
  biweekly$date_d = as.Date(biweekly$date, format = "%Y-%m-%d")

  jpeg(paste0(directory, "/aallspecs_biweekly.jpeg"))
  print(ggplot(biweekly, aes(x=date_d, y = `Specimens.collected`)) +
    geom_line(col = "blue") + xlab("") +
    scale_x_date(date_labels = "%Y") +
    ggtitle(paste(locale,"all species, Biweekly Mean")) +
    theme_bw() + labs(y = "Mosquitoes Collected", x = "Time"))
  dev.off()
  
  # Monthly 
  jpeg(paste0(directory, "/aallspecs_monthly.jpeg"))
  print(ggplot(monthly, aes(x=date, y = `Specimens.collected`)) +
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
    jpeg(paste0(directory, "/", name, "_weekly.jpeg"))
    print(ggplot(weekly, aes_string(x= "date", y = name)) +
      geom_line(col = "darkgreen") + xlab("") +
      scale_x_date(date_labels = "%Y") +
      ggtitle(paste(locale, name, "Weekly")) +
      theme_bw() + labs(y = "Mosquitoes Collected",
                        x = "Time"))
    dev.off()
    
    # Bi-weekly
    jpeg(paste0(directory, "/", name, "_biweekly.jpeg"))
    print(ggplot(biweekly, aes_string(x= biweekly$date_d, y = name)) +
      geom_line(col = "blue") + xlab("") +
      scale_x_date(date_labels = "%Y") +
      ggtitle(paste(locale, name, "Biweekly")) +
      theme_bw() + labs(y = "Mosquitoes Collected"))
    dev.off()
    
    # Monthly
    jpeg(paste0(directory, "/", name, "_monthly.jpeg"))
    print(ggplot(monthly, aes_string(x="date", y = name)) +
      geom_line(col = "purple") + xlab("") +
      scale_x_date(date_labels = "%Y") +
      ggtitle(paste(locale, name, "Monthly")) +
      theme_bw() + labs(y = "Mosquitoes Collected",
                        x = "Time"))
    dev.off()
  }
  
  rm(list=setdiff(ls(), c("i", "files", "morphological_aggregations")))
}

cat("\n Here is the summary of species in each location that were aggregated to morphological groups")
print(morphological_aggregations)

# Save the morphological aggregations summary to results
write.csv(morphological_aggregations, "../Results/morphological_groups.csv", row.names = FALSE)

#!/bin/env Rscript
# Author: Anne Marie Saunders
# Script: aggregation_batch.R
# Desc: Use aggregation script on a set of datasets
# Arguments: 
# Date: 04/27/20

library(gridExtra)
setwd("~/Documents/Hefty_Data/Extracted_Data/")

# Define safety function
continue = function() {
  answer = readline('Do you wish to continue? (y/n)')
  
  if(substr(answer, 1, 1) == "n"){
    cat("Skipping to next location in the loop\n")
    return(1)
  }
  
  if(substr(answer, 1, 1) == "y"){
    cat("You'd better know what you're doing, then!\n")
    return(0)
  }
  
  else{
    cat("Answer given was not an option. Please try again.\n")
    continue()
  }
}

## Begin loop to aggregate files
files = list.files(pattern = "_TS.csv")

#pdf(file = "Aggregated/all_aggregations.pdf", width = 8.5, height = 11)
for(i in 1:length(files)){
  cat(paste("Now aggregating data for", sub(".csv", "", files[i]), "\n"))
  setwd("~/Documents/Hefty_Data/Extracted_Data/")
  mapped = read.csv(files[i], header = T, stringsAsFactors = F)
  source("~/Documents/CMEEThesis/Code/aggregation_sepspecs.R")
  setwd("~/Documents/Hefty_Data/Extracted_Data/Aggregated")
  write.csv(weekly, paste0(locale, "_weekly.csv"), row.names = F)
  write.csv(biweekly, paste0(locale, "_biweekly.csv"), row.names = F)
  write.csv(monthly, paste0(locale, "_monthly.csv"), row.names = F)
 # grid.arrange(plot_w, plot_bw, plot_m, nrow = 3)
  
  ### Plot ###
  setwd("~/Documents/Visualizations/AggByLocation/")
  
  # Make sure you won't be overwriting files if you don't want to!
  safetycheck = list.files(include.dirs = T)
  result = 0
  
  if(locale %in% safetycheck){
    cat("These files likely already exist. Continuing will overwrite these files.\n\n")
    result = continue()
  }
  
  if(result != 0){next}
  
  # Create a directory to store plots
  dir.create(locale)
  setwd(paste0("~/Documents/Visualizations/AggByLocation/", locale))
  
  ## Create plots of all species aggregated at weekly, biweekly, and monthly time scales ## 
  
  # Weekly
  jpeg("aallspecs_weekly.jpeg")
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

  jpeg("aallspecs_biweekly.jpeg")
  print(ggplot(biweekly, aes(x=ids, y = `Specimens.collected`)) +
    geom_line(col = "blue") + xlab("") +
    scale_x_continuous("Time", breaks = brks, labels = c(yrs, next_year)) +
    ggtitle(paste(locale,"all species, Biweekly Mean")) +
    theme_bw() + labs(y = "Mosquitoes Collected"))
  dev.off()
  
  # Monthly 
  jpeg("aallspecs_monthly.jpeg")
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
    jpeg(paste0(name, "_weekly.jpeg"))
    print(ggplot(weekly, aes_string(x= "date_dw", y = name)) +
      geom_line(col = "darkgreen") + xlab("") +
      scale_x_date(date_labels = "%Y") +
      ggtitle(paste(locale, name, "Weekly")) +
      theme_bw() + labs(y = "Mosquitoes Collected",
                        x = "Time"))
    dev.off()
    
    # Bi-weekly
    jpeg(paste0(name, "_biweekly.jpeg"))
    print(ggplot(biweekly, aes_string(x= "ids", y = name)) +
      geom_line(col = "blue") + xlab("") +
      scale_x_continuous("Time", breaks = brks, labels = c(yrs, next_year)) +
      ggtitle(paste(locale, name, "Biweekly")) +
      theme_bw() + labs(y = "Mosquitoes Collected"))
    dev.off()
    
    # Monthly
    jpeg(paste0(name, "_monthly.jpeg"))
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
#dev.off()

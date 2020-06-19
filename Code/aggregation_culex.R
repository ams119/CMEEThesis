#!/bin/env Rscript
# Author: Anne Marie Saunders
# Script: aggregation_culex.R
# Desc: Aggregate all culex species for a set of locations
# Arguments: aggregated time series of abundance and climate, ex. Collier_clim_TS.csv
# Date: 05/25/20

library(gridExtra)
library(ggplot2)

# Set working directory to already aggregated files
setwd("~/Documents/Hefty_Data/Extracted_Data/Aggregated")

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

# Have a look at files you might want to use
files = list.files()

# Keep my best florida datasets: Lee, Manatee, Walton, St Johns, Orange. 
# Manatee and Lee are probably the most stationary
locations = sort(c("Manatee", "Lee", "Walton", "Saint Johns", "Orange"))
# Make sure they're ogranized alphabetically in a matrix for easy extraction
files = sort(grep(paste0(locations,collapse="|"), files, value=TRUE))
files = matrix(files, ncol = 3, nrow = length(locations), byrow = T)

# Now loop through files, making
for(i in 1:length(locations)){
  setwd("~/Documents/Hefty_Data/Extracted_Data/Aggregated")
  cat(paste("Now aggregating Culex data for", sub(".csv", "", locations[i]), "\n"))
  
  # Read each of the relevant aggregations for this location
  biweekly_ts = read.csv(files[i, 1], header = T, stringsAsFactors = F)
  monthly_ts = read.csv(files[i, 2], header = T, stringsAsFactors = F)
  weekly_ts = read.csv(files[i, 3], header = T, stringsAsFactors = F)
  
  # Convert date columns to date format
  weekly_ts$date_dw = as.Date(weekly_ts$date_dw)
  monthly_ts$date_dm = as.Date(monthly_ts$date_dm)
  
  # Find the columns of culex species
  culex_cols = grep("Culex", colnames(biweekly_ts), value = F, ignore.case = T)
  
  # Sum together each row of the culex columns to make a culex total
  biweekly_ts$culex.sum = rowSums(biweekly_ts[,culex_cols], na.rm = T)
  monthly_ts$culex.sum = rowSums(monthly_ts[,culex_cols], na.rm = T)
  weekly_ts$culex.sum = rowSums(weekly_ts[,culex_cols], na.rm = T)
  
  # Take the mean of each row of the culex columns to find culex means
  biweekly_ts$culex.mean = rowMeans(biweekly_ts[,culex_cols], na.rm = T)
  monthly_ts$culex.mean = rowMeans(monthly_ts[,culex_cols], na.rm = T)
  weekly_ts$culex.mean = rowMeans(weekly_ts[,culex_cols], na.rm = T)
  
  
  #### Plotting ####
  ## Plot graphs of mean and sum for all culex species
  
  # Change wd
  setwd("~/Documents/Visualizations/Culex/")
  
  # Create a folder for this location
  # Make sure you won't be overwriting files if you don't want to!
  safetycheck = list.files(include.dirs = T)
  result = 0
  
  if(locations[i] %in% safetycheck){
    cat("These files likely already exist. Continuing will overwrite these files.\n\n")
    result = continue()
  }
  
  if(result != 0){next}
  
  # Create a directory to store plots
  dir.create(locations[i])
  setwd(paste0("~/Documents/Visualizations/Culex/", locations[i]))
  
  # Plot mean:
  # Weekly
  jpeg("allculex_mean_weekly.jpeg")
  print(ggplot(weekly_ts, aes(x=date_dw, y = culex.mean)) +
          geom_line(col = "darkgreen") + xlab("") +
          scale_x_date(date_labels = "%Y") +
          ggtitle(paste0(locations[i], ", all species, Weekly Mean")) +
          theme_bw() + labs(y = "Mosquitoes Collected",
                            x = "Time"))
  dev.off()
  
  ## Biweekly
  
  # Create manual tick marks and axis labels
  yrs = unique(biweekly_ts$Year)
  brks = seq(0, 27 * length(yrs), by = 27)
  next_year = as.character(as.numeric(yrs[length(yrs)]) + 1)
  
  jpeg("allculex_mean_biweekly.jpeg")
  print(ggplot(biweekly_ts, aes(x=ids, y = culex.mean)) +
          geom_line(col = "blue") + xlab("") +
          scale_x_continuous("Time", breaks = brks, labels = c(yrs, next_year)) +
          ggtitle(paste(locations[i],"all species, Biweekly Mean")) +
          theme_bw() + labs(y = "Mosquitoes Collected"))
  dev.off()
  
  # Monthly 
  jpeg("allculex_mean_monthly.jpeg")
  print(ggplot(monthly_ts, aes(x=date_dm, y = culex.mean)) +
          geom_line(col = "purple") + xlab("") +
          scale_x_date(date_labels = "%Y") +
          ggtitle(paste0(locations[i], " all species, Monthly Mean")) +
          theme_bw() + labs(y = "Mosquitoes Collected",
                            x = "Time"))
  dev.off()
  
  ## Plot sum:
  # Weekly
  jpeg("allculex_sum_weekly.jpeg")
  print(ggplot(weekly_ts, aes(x=date_dw, y = culex.sum)) +
          geom_line(col = "darkgreen") + xlab("") +
          scale_x_date(date_labels = "%Y") +
          ggtitle(paste0(locations[i], ", all species, Weekly Sum")) +
          theme_bw() + labs(y = "Mosquitoes Collected",
                            x = "Time"))
  dev.off()
  
  ## Biweekly
  
  # Create manual tick marks and axis labels
  brks = seq(0, 27 * length(yrs), by = 27)
  next_year = as.character(as.numeric(yrs[length(yrs)]) + 1)
  
  jpeg("allculex_sum_biweekly.jpeg")
  print(ggplot(biweekly_ts, aes(x=ids, y = culex.sum)) +
          geom_line(col = "blue") + xlab("") +
          scale_x_continuous("Time", breaks = brks, labels = c(yrs, next_year)) +
          ggtitle(paste(locations[i],"all species, Biweekly Sum")) +
          theme_bw() + labs(y = "Mosquitoes Collected"))
  dev.off()
  
  # Monthly 
  jpeg("allculex_sum_monthly.jpeg")
  print(ggplot(monthly_ts, aes(x=date_dm, y = culex.sum)) +
          geom_line(col = "purple") + xlab("") +
          scale_x_date(date_labels = "%Y") +
          ggtitle(paste0(locations[i], " all species, Monthly Sum")) +
          theme_bw() + labs(y = "Mosquitoes Collected",
                            x = "Time"))
  dev.off()
  
  ## Now plot each culex species:
  for(j in 1:length(culex_cols)){
    
    name = colnames(weekly_ts)[culex_cols[j]]
    
    # Weekly
    jpeg(paste0(name, "_weekly.jpeg"))
    print(ggplot(weekly_ts, aes_string(x= "date_dw", y = name)) +
            geom_line(col = "darkgreen") + xlab("") +
            scale_x_date(date_labels = "%Y") +
            ggtitle(paste(locations[i], name, "Weekly")) +
            theme_bw() + labs(y = "Mosquitoes Collected",
                              x = "Time"))
    dev.off()
    
    # Bi-weekly
    jpeg(paste0(name, "_biweekly.jpeg"))
    print(ggplot(biweekly_ts, aes_string(x= "ids", y = name)) +
            geom_line(col = "blue") + xlab("") +
            scale_x_continuous("Time", breaks = brks, labels = c(yrs, next_year)) +
            ggtitle(paste(locations[i], name, "Biweekly")) +
            theme_bw() + labs(y = "Mosquitoes Collected"))
    dev.off()
    
    # Monthly
    jpeg(paste0(name, "_monthly.jpeg"))
    print(ggplot(monthly_ts, aes_string(x="date_dm", y = name)) +
            geom_line(col = "purple") + xlab("") +
            scale_x_date(date_labels = "%Y") +
            ggtitle(paste(locations[i], name, "Monthly")) +
            theme_bw() + labs(y = "Mosquitoes Collected",
                              x = "Time"))
    dev.off()
  }
}

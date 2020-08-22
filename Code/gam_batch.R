#!/bin/env Rscript
# Author: Anne Marie Saunders
# Script: gam_batch.R
# Desc: This script uses the functions from gam_functions.R and runs them iteratively on all location and temporal aggregation files. It then compiles all locations into 3 ts_datasets of weekly, biweekly, and monthly ts_data
# Arguments: 
# Date: 07/28/20

# Time the script
start = Sys.time()

# Load functions from gam_functions.R
source("gam_functions.R")
library(tidyverse)

# Create directory for storing GAM plots
dir.create("../Results/GAM_Plots/")

# Create directory for storing distribution histograms
dir.create("../Results/Fit_Plots/")

# Create directory for storing predicted abundance plots
dir.create("../Results/Pred_Plots/")

# List all aggregated files 
path = "../Data/Extracted_Data/Aggregated/"
files = list.files(path)

# Only include focal locations
locations = sort(c("Manatee", "Lee", "Walton", "Saint_Johns", "Orange"))
files = files[grep(pattern = paste(locations, collapse = "|"), x = files)]

# Organize through alphebetisation into matrix by location for rows and by temporal scale for columns
files = matrix(sort(files), nrow = length(unique(locations)), byrow = TRUE)

# Loop through each temporal scale
for(i in 1:ncol(files)){
  
  # Create empty list to store all location output tables at the temporal scale
  output_list = vector("list", length = nrow(files))
  
  # Define the time scale for these datasets
  scale = c("biweekly", "monthly", "weekly")[i]
  
  # Loop through each location in this temporal scale
  for(j in 1:nrow(files)){
    
    # Read in ts_dataset
    ts_data = read.csv(paste0(path, files[j,i]), header = TRUE, stringsAsFactors = FALSE)
    
    # Identify the species names in this dataset
    species = colnames(ts_data[10:(dim(ts_data)[2]-3)])
    
    # Create vectors identifying the lags at this temporal scale
    lags = make_laglists(scale = scale)
    
    # Create a data frame of lagged meteorological values
    lag_table = make_lag_table(temp = ts_data$temp_mean, precip = ts_data$precip_days, lags = lags)
    
    # Create empty output matrix
    output = make_output(lags = lags, species = species)
    
    # Add a column describing the location
    output$Location = locations[j]
    
    # Fit univariate models and store in this output table
    output = fit_univariate_GAMs(ts_data = ts_data, output = output, lags = lags, lag_table = lag_table, species = species, scale = scale)
    
    # Fit multivariate models and store in this output table
    output = fit_multivariate_GAMs(ts_data = ts_data, output = output, lags = lags, lag_table = lag_table, species = species, scale = scale)
    
    # Append this output table to the total output list for this time scale
    output_list[[j]] = output
  }
  
  # Join each list together into one dataframe for this temporal scale
  output_df  = bind_rows(output_list)
  
  # Save as csv
  write.csv(output_df, file = paste0("../Results/GAM_", scale, ".csv"), row.names = FALSE)
  
  
}

# Print total time this took
Sys.time() - start

#!/bin/env Rscript
# Author: Anne Marie Saunders
# Script: generalize_gam.R
# Desc: This script trains a gam on the most comprehensive dataset and then tests it on other locations
# Arguments: 
# Date: 08/04/20

# Load libraries
library("tidyverse")
source("gam_functions.R")

# Load the monthly data output
output = read.csv("../Results/GAM_monthly.csv", header = T, stringsAsFactors = F)

# Load time series data for each location and store in list

counties = sort(c("Manatee", "Saint_Johns", "Orange", "Walton", "Lee"))

ts_data = vector("list", length = length(counties))

for(i in 1:length(counties)){
  ts_data[[i]] = read_csv(paste0("../Data/Extracted_Data/Aggregated/", counties[i], "_monthly.csv"))
}

# Add column to store the reference dataset for each row
output$Reference = rep(NA, nrow(output))

# Add column to store the MAE (mean absolute error) for the fit of the reference on this dataset
output$Ref_MAE = rep(NA, nrow(output))

# Add column to store the MB (mean bias) for the fit of the reference on this dataset
output$Ref_MB = rep(NA, nrow(output))

# Add column to store the MAPE for the fit of the reference on this dataset
output$Ref_MAPE = rep(NA, nrow(output))

# Add column to store the nRMSE for the fit of the reference on this dataset
output$Ref_nRMSE = rep(NA, nrow(output))

# Find indexes where zero inflation is greater than 90%
#indexes = which(output$z_inflation_pct <= 90)

# Find species that occur in 4 or 5 locations
focal_species = output %>% count(Species, sort = T) %>% filter(n > 3) %>% pull(Species)

# Find indexes of rows with less than 91% zero inflation and are one of these species
indexes = which(output$Species %in% focal_species)

# Assign reference dataset for each species
for(i in 1:length(focal_species)){
  
  cat("\n********************************************************\nBeginning reference fitting for ", 
      focal_species[i], "\n")
  
  # Take dataset of just this focal species
  species_i = output %>% filter(Species == focal_species[i])
  
  # Find the index of the location for this species with the largest number of data points
  ref_location = which(counties == species_i %>% filter(nr_total_obs == max(nr_total_obs)) %>% pull(Location))
  
  # Record this location in the output table
  output$Reference[which(output$Species == focal_species[i])] = counties[ref_location]
  
  # Train the reference gam # 
  # Find the index in the output data frame of the reference dataset
  r = which(output$Species == focal_species[i] & output$Location == counties[ref_location])
  
  # Prepare lags and lag table for the reference dataset
  # Create vectors identifying the lags at this temporal scale
  lags = make_laglists(scale = "monthly")
  
  # Create a data frame of lagged meteorological values using the location-indexed data frame from the ts_data list
  lag_table = make_lag_table(temp = ts_data[[ref_location]]$temp_mean, precip = ts_data[[ref_location]]$precip_days, lags = lags)
  
  # Prepare best fit x and y variables. Use r (output index) and ref_location (time series data index) to find correct values
  ref_vars = prep_variables(temp = lag_table[[output$Best_Temp[r]]], precip = lag_table[[output$Best_Precip[r]]], abundance = ts_data[[ref_location]][[focal_species[i]]])
  
  # Set precip k, ensuring degrees of freedom does not exceed the number of unique values
  precip_k = 10
  if(length(unique(ref_vars$precip)) < 10){precip_k = length(unique(ref_vars$precip))}
  
  # Try fitting the gam
  ref_gam = try(gam(abundance ~ s(temp, bs = 'cr', k = 10) + s(precip, k = precip_k, bs = 'cr'), data = ref_vars, family = Gamma(link = "log"), method = "REML", select = T), silent = T)
  
  # Skip this whole species group and move to the next one if the reference gam doesn't work
  if(class(ref_gam)[1] == "try-error"){
    cat("\n Warning: Reference gam failed- skipping to next species group\n")
    next
  }    
  
  cv_scores = k_fold_cross_validate(vars = ref_vars, precip_k = precip_k, type = "nonAR", nr_folds = 10)
  output$Ref_MAE[r] = cv_scores[1]
  output$Ref_MB[r] = cv_scores[3]
  
  ## Find the size of the smallest dataset at this lag ##
  # First find location index of smallest dataset
  small_location = which(counties == species_i %>% filter(nr_total_obs == min(nr_total_obs)) %>% pull(Location))
  
  # Then prep the lag table
  lag_table = make_lag_table(temp = ts_data[[small_location]]$temp_mean, precip = ts_data[[small_location]]$precip_days, lags = lags)
  
  # Then count the number of rows in the dataframe of the variables at this lag
  size_limit = nrow(prep_variables(temp = lag_table[[output$Best_Temp[r]]], precip = lag_table[[output$Best_Precip[r]]], abundance = ts_data[[small_location]][[focal_species[i]]]))
  
  for(j in 1:nrow(species_i)){
    
    
    # Find the index in the output data frame of this comparison dataset
    c = which(output$Species == focal_species[i] & output$Location == species_i$Location[j])
    
    # If we've selected the same index as the reference location, skip to the next dataset
    if(r == c){
      next
    }
    # Find index of the location matching this dataset
    comp_location = which(output$Location[c] == counties)
    
    # Create a data frame of lagged meteorological values using the location-indexed data frame from the ts_data list
    lag_table = make_lag_table(temp = ts_data[[comp_location]]$temp_mean, precip = ts_data[[comp_location]]$precip_days, lags = lags)
    
    # Prepare best fit x and y variables. Use r to find the best lags according to the reference with this lag table and use abundance data from this species
    comp_vars = prep_variables(temp = lag_table[[output$Best_Temp[r]]], precip = lag_table[[output$Best_Precip[r]]], abundance = ts_data[[comp_location]][[focal_species[i]]])
    
    # Create list to store RMSE
    nRMSE = rep(NA, 100)
    
    # For RMSE comparison, all datasets must be same size. Average multiple trials of random samples of each dataset
    # At the size of the smallest dataset based on this lag (size_limit)
    
    # Repeat calculation many times sampling the size limit of the smallest data 
    for(h in 1:200){
    
      var_indx = sample(1:length(comp_vars$abundance), size = size_limit, replace = FALSE)      
      
      # Predict abundance using these new location specific temp and precip values
      pred = predict.gam(ref_gam, comp_vars[var_indx,], se.fit = T)
      
      nRMSE[h] = sqrt((sum((comp_vars$abundance[var_indx] - exp(pred$fit))^2))/size_limit)/IQR(comp_vars$abundance[var_indx])
      
    }
    
    # Then do predictions for the entire available dataset to find other measures of fit
    pred = predict.gam(ref_gam, comp_vars, se.fit = T)
    
    # Calculate the mean absolute error of this prediction
    output$Ref_MAE[c] = round(mean(sum(abs(exp(pred$fit) - comp_vars$abundance))/length(pred$fit), na.rm = T), 3)
    
    # Calculate the normalised of this prediction
    output$Ref_nRMSE[c] = round(mean(nRMSE, na.rm = T), 3)
    
    # Calculate the MAPE of this prediction
    output$Ref_MAPE[c] = round(sum(abs(exp(pred$fit)-comp_vars$abundance))/sum(comp_vars$abundance), 3)
    
    # Record the mean bias
    output$Ref_MB[c] = round(sum(exp(pred$fit) - comp_vars$abundance)/length(pred$fit),3)
    
  }
}

write_csv(output, "../Results/GAM_monthly_generalised.csv")

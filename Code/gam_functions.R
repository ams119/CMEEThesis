#!/bin/env Rscript
# Author: Anne Marie Saunders
# Script: gam_functions.R
# Desc: This script contains the functions necessary for model fitting and selection 
# Arguments: 
# Date: 07/25/20

#### Libraries ####
library(mgcv)

#### Functions ####

# This function pre-processes precipitation, temperature, and abundance values for fitting with GAM
# by creating autoregressive x variable and removing rows with NA values in any of the x or y variables.
# It also prepares abundance variables for logging by adding 1. It returns adjusted temperature, 
# precipitation, and abundance vectors as well as a new autoregressive abundance vector
prep_variables = function(temp, precip, abundance, years){
  
  # Create the vector of autoregressive abundance
  AR1 = c(NA, abundance[-length(abundance)])
  
  # Find NA indexes in all variables 
  find_nas = which(is.na(AR1) | is.na(abundance) | is.na(temp) | is.na(precip))
  
  # remove these indexes from all variables
  AR1 = AR1[-find_nas]
  abundance = abundance[-find_nas]
  temp = temp[-find_nas]
  precip = precip[-find_nas]
  years = years[-find_nas]
  
  return(data.frame(abundance, temp, precip, AR1, years))
  
}

# This function makes appropriate column names with length according to the temporal scale
make_laglists = function(scale){
  if(scale == 'weekly'){
    templags = paste0(rep('temp_lag', 13), 0:12)
    preciplags = paste0(rep('precip_lag', 13), 0:12)
  }
  
  if(scale == 'biweekly'){
    templags = paste0(rep('temp_lag', 6), 0:5)
    preciplags = paste0(rep('precip_lag', 6), 0:5)
  }
  
  if(scale == 'monthly'){
    templags = paste0(rep('temp_lag', 3), 0:2)
    preciplags = paste0(rep('precip_lag', 3), 0:2)
  }
  
  return(list(temp = templags, precip = preciplags))
}

# Make an empty output data frame for each location
make_output = function(lags, species){
  
  # Create desired column names
  columns = c("Species", lags$temp, lags$precip, "nr_total_obs", "nr_bestfit_obs", "nr_nonzero_obs", "p", 
              "z_inflation_pct", "Best_Temp", "AIC_wt_temp", "Best_Precip", "AIC_wt_precip", "Multi_DevianceExplained", "Multi_AIC", 
              "Multi_MAE", "Multi_NMAE", "Multi_MB", "Multi_Blocks","Multi_SignifVariables", "MultiAR_DevianceExplained", 
              "MultiAR_AIC", "MultiAR_MAE", "MultiAR_NMAE", "MultiAR_MB", "MultiAR_Blocks","MultiAR_SignifVariables")
  
  # Create an empty dataframe with of appropriate size
  output = data.frame(matrix(NA, nrow = length(species), ncol = length(columns)))
  
  # Give descriptive column names
  colnames(output) = columns
  
  # Name each row with species names 
  output$Species = species
  
  # Return completed output table
  return(output)
  
}

# Make a data table of lagged temperature and precipitation values
make_lag_table = function(temp, precip, lags){
  
  # Create empty data frame to be filled by lagged values
  lag_table = data.frame(matrix(NA, nrow = length(temp), ncol = length(lags$temp)*2))
  
  # Set column names to the type of lag
  colnames(lag_table) = c(lags$temp, lags$precip)
  
  # In each of the temperature columns add sequentially NAs at start and remove values from end to create lags
  for(i in 1:length(lags$temp)){
    lag = i-1
    lag_table[,i] = c(rep(NA, lag), temp[1:(length(temp)-lag)])
  }
  
  # Do the same in each of the precipitation columns- these begin in the length(lags$temp)+1 column
  for(i in 1:length(lags$precip)){
    lag = i-1
    lag_table[,length(lags$temp) + i] = c(rep(NA, lag), precip[1:(length(precip)-lag)])
  }
  
  # Return completed lag table
  return(lag_table)
}

fit_univariate_GAMs = function(ts_data, output, lags, lag_table, species, scale){
  
  ## Fit univariate models with each species abundance vector ##
  for(i in 1:length(species)){
    
    cat(paste0("\n*********************************************************\n\nNow evaluating univariate GAMs for species ", i, ', ', species[i], " at ", scale, " ", output$Location[i], '\n'))
    
    output$nr_total_obs[i] = sum(!is.na(ts_data[[species[i]]]), years = ts_data$Year)
    
    # Create a loose guess at z_inflation_pct for this dataset
    output$z_inflation_pct[i] = round(sum(ts_data[[species[i]]] == 0, na.rm = T)/output$nr_total_obs[i]*100)
    
    # Highly zero inflated data takes forever to converge and we will cut it out anyway: just skip analysis to save time
    if(output$z_inflation_pct[i] >= 95){next}
    
    # Conduct univariate GAMs for each lags of each meteorological variable
    for(j in 1:length(lags$temp)){
      
      # Prepare all x and y variables
      vars = prep_variables(temp = lag_table[,j], precip = lag_table[,length(lags$temp) + j], abundance = ts_data[[species[i]]], years = ts_data$Year)
      
      # Max number of knots (k) is = to the number of unique data points (discrete days of rainfall) 
      # Max number of basis functions is equal to k-1
      # Default k will be 10, but datasets with fewer unique values 10 than this will be adjusted accordingly
      precip_k = 10
      if(length(unique(vars$precip)) < 10){precip_k = length(unique(vars$precip))}
      
      # Fit temperature univariate model
      temp_gam = try(gam(abundance ~ s(temp, k = 10, bs = 'cr'), data = vars, family = tw, method = "REML"), silent = TRUE)
      if(class(temp_gam)[1] != "try-error"){
        output[[lags$temp[j]]][i] = AIC(temp_gam)
      }

      # Fit precipitation univariate model
      precip_gam = try(gam(abundance ~ s(precip, k = precip_k, bs = 'cr'), data = vars, family = tw, method = "REML"), silent = TRUE)
      
      if(class(precip_gam)[1] != "try-error"){
        output[[lags$precip[j]]][i] = AIC(precip_gam)
      }
      
      
    }
    
  }
  
  # Find row minimums for temperature and precipitation in output by finding the column name of the max.col
  # when output is multiplied by -1
  output$Best_Temp = colnames(output[,colnames(output) %in% lags$temp])[max.col(-output[,colnames(output) %in% lags$temp])]
  output$Best_Precip = colnames(output[,colnames(output) %in% lags$precip])[max.col(-output[,colnames(output) %in% lags$precip])]
  
  return(output)
}

fit_multivariate_GAMs = function(ts_data, output, lags, lag_table, species, scale){
  
  ## Fit multiivariate models with each species abundance vector ##
  for(i in 1:length(species)){
    
    cat(paste0("\n*********************************************************\n\nNow evaluating multivariate GAMs for species ", i, ', ', species[i], " at ", scale, " ", output$Location[i], '\n'))
    
    # Skip datasets where we didn't calculate best fit lags
    if(isTRUE(output$z_inflation_pct[i] >= 95)){next}
    
    # Prepare x and y variables. We'll use the best fit lags of temp and precip for each species 
    vars = prep_variables(temp = lag_table[[output$Best_Temp[i]]], precip = lag_table[[output$Best_Precip[i]]], abundance = ts_data[[species[i]]], years = ts_data$Year)
    
    # Record the minimum number of observations in this dataset with this best fit lag
    output$nr_bestfit_obs[i] = length(vars$abundance)
    
    # Record the number of non-zero observations 
    output$nr_nonzero_obs[i] = length(which(vars$abundance != 0))
    
    # Re-calculate the zero inflation of this dataset with this best fit lag
    output$z_inflation_pct[i] = round(sum(vars$abundance == 0)/output$nr_bestfit_obs[i]*100)
    
    # Now skip the highly zero inflated datasets that aren't going to be used in analysis
    if(isTRUE(output$z_inflation_pct[i] >= 90)){next}
    
    # Calculate the akaike weights of the best fit temperature and precipitation lags
    output = akaike_weight(output_data = output, lags = lags, i = i)
    
    # Max number of basis splines is = to the number of unique data points (discrete days of rainfall) 
    # Thus max k (number of knots) is equal to nr of unique values + 1. 
    # Default will be 10, but datasets with fewer unique values 9 than this will be adjusted accordingly
    precip_k = 10
    if(length(unique(vars$precip)) < 10){precip_k = length(unique(vars$precip))}
    
    # Fit non-autoregressive multivariate model where smooth terms can be penalized out
    multi_gam = try(gam(abundance ~ s(temp, bs = 'cr', k = 10) + s(precip, k = precip_k, bs = 'cr'), data = vars, family = tw, method = "REML", select = TRUE), silent = TRUE)
    if(class(multi_gam)[1] != "try-error"){
      # Record the power index of this model
      output$p[i] = round(as.numeric(str_extract(pattern = "[0-9]\\.[0-9]*", summary(multi_gam)$family$family)), 1)
      # Record the AIC of this model
      output$Multi_AIC[i] = multi_gam$aic
      # Record deviance explained
      output$Multi_DevianceExplained[i] = round(100*summary(multi_gam)$dev.expl, 1)
      # Record which predictive terms are significant according p-value with alpha = 0.05
      output$Multi_SignifVariables[i] = paste0(c('temp', 'precip')[which(summary(multi_gam)$s.pv <= 0.05)], collapse = ",")
      # Perform block cross validation to get CV scores (MAE and MAAPE)
      cv_scores = block_cross_validate(vars = vars, precip_k = precip_k, type = "nonAR")
      output$Multi_MAE[i] = cv_scores[1]
      output$Multi_NMAE[i] = cv_scores[2]
      output$Multi_MB[i] = cv_scores[3]
      output$Multi_Blocks[i] = cv_scores[4]
      
      # Plot only if zero inflation < 90
      
    
      png(filename = paste0("../Results/GAM_Plots/", scale, output$Location[i], species[i], ".png"), height = 900, width = 900, units = 'px')
      plot(multi_gam, rug = TRUE, page = 1, residuals = TRUE, main = paste(scale, output$Location[i], species[i]))
      dev.off()
      
      png(filename = paste0("../Results/Fit_Plots/", scale, output$Location[i], species[i], "dist.png"), height = 900, width = 900, units = 'px')
      hist(vars$abundance, col = "lightblue", main = paste("Histogram of log(abundance, z inflation =", output$z_inflation_pct[i]))
      dev.off()
      
      png(filename = paste0("../Results/Fit_Plots/", scale, output$Location[i], species[i], "modelcheck_tw.png"), height = 900, width = 900, units = 'px')
      plot_gam_check(multi_gam)
      dev.off()
      
      multi_gam_gamma = try(gam(abundance+1 ~ s(temp, bs = 'cr', k = 10) + s(precip, k = precip_k, bs = 'cr'), data = vars, family = Gamma(link = "log"), method = "REML", select = TRUE), silent = TRUE)
      if(class(multi_gam_gamma)[1] != "try-error"){
        png(filename = paste0("../Results/Fit_Plots/", scale, output$Location[i], species[i], "modelcheck_gamma.png"), height = 900, width = 900, units = 'px')
        plot_gam_check(multi_gam_gamma)
        dev.off()
      }
        
    }
  
    # Fit autoregressive multivariate model where smooth terms can be penalized out
    multiAR_gam = try(gam(abundance ~ s(temp, k = 10, bs = 'cr') + s(precip, k = precip_k, bs = 'cr') + s(log(AR1 + 0.000001), k = 10, bs = 'cr'), select = TRUE, data = vars, family = tw,  method = "REML"), silent = TRUE)
    if(class(multiAR_gam)[1] != "try-error"){
      # Record AIC of this model
      output$MultiAR_AIC[i] = multiAR_gam$aic
      # Record the deviance explained
      output$MultiAR_DevianceExplained[i] = round(100*summary(multiAR_gam)$dev.expl, 1)
      # Record which predictive terms are significant according p-value with alpha = 0.05
      output$MultiAR_SignifVariables[i] = paste0(c('temp', 'precip', 'AR1')[which(summary(multiAR_gam)$s.pv <= 0.05)], collapse = ",")
      # Perform k-fold cross validation to get CV scores
      cv_scores = block_cross_validate(vars = vars,  precip_k = precip_k, type = "nonAR")
      output$MultiAR_MAE[i] = cv_scores[1]
      output$MultiAR_NMAE[i] = cv_scores[2]
      output$MultiAR_MB[i] = cv_scores[3]
      output$MultiAR_Blocks[i] = cv_scores[4]
      
    }
  }
  
  return(output)
}


# Create a function to perform k fold cross validation for AR and non AR models
block_cross_validate = function(vars, precip_k, type){
  
  # Get vector of unique years
  yrs = unique(vars$years)
  
  # Count the number of blocks -> equal to the number of years
  nr_blocks = length(yrs)
  
  # Create vector to store MAE explained in each test
  MAE = rep(NA, nr_blocks)
  
  # Create vector to store NMAE explained in each test
  NMAE = rep(NA, nr_blocks)
  
  # Create vector to store MB explained in each test
  MB = rep(NA, nr_blocks)
  
  ## Do block cross validation
  for(i in 1:nr_blocks){
    
    # Split vars into test and train by iteratively choosing 1 year
    test = vars %>% filter(years == yrs[i])
    train = vars %>% filter(years != yrs[i])
    
    # If the test set is unusually small- less than 50% the average size of each year
    # Do not calculate MAE with this year
    if(nrow(test) < 0.5 * nrow(vars)/nr_blocks){next}
    
    if(type == "AR"){
      # Train AR gam model using training set
      gam = try(gam(abundance ~ s(temp, k = 10, bs = 'cr') + s(precip, k = precip_k, bs = 'cr') + s(log(AR1), k = 10, bs = 'cr'), 
              select = TRUE, data = train, family = tw,  method = "REML"), silent = TRUE)
    }
    
    if(type == "nonAR"){
      # Train a non-AR gam model using the training set of data
      gam = try(gam(abundance ~ s(temp, k = 10, bs = 'cr') + s(precip, k = precip_k, bs = 'cr'), 
                    select = TRUE, data = train, family = tw,  method = "REML"), silent = TRUE)
    }
    
    if(class(gam)[1] != "try-error"){
      pred = predict.gam(gam, test, se.fit = TRUE)
      
      # Record MAE of this test/train set in the vector
      MAE[i] = sum(abs(exp(pred$fit) - test$abundance))/length(pred$fit)
      
      # Record the mean bias
      MB[i] = sum(exp(pred$fit) - test$abundance)/length(test$abundance)
      
      # Record NMAE
      NMAE[i] = sum(abs(exp(pred$fit) - test$abundance))/sum(log(test$abundance))
    }
    
  }
  
  # Return a vector of the MAE, NMAE, MB, and number of folds used in averaging
  return(c(round(mean(MAE, na.rm = TRUE), 3), round(mean(NMAE, na.rm = TRUE), 3), round(mean(MB, na.rm = TRUE), 3), sum(!is.na(MAE))))
  
}

akaike_weight = function(output_data, lags, i){
  # Find just the Akaike weight of the best fit model
  # Find the numerator: for best fit lag, delta AIC = 0.  exp(-1/2 delta AIC). 
  num = exp(-1/2 * 0) # is equal to 1
  
  # Find the temperature denominator: sum of exp(-1/2 delta AIC) for each lag
  denom = sum(exp(-1/2 * (output_data[i,which(colnames(output_data) %in% lags$temp)] - min(output_data[i,which(colnames(output_data) %in% lags$temp)]))))
  
  # Store in table
  output_data$AIC_wt_temp[i] = round(num/denom, 3) *100
  
  # Find the temperature denominator: sum of exp(-1/2 delta AIC)
  denom = sum(exp(-1/2 * (output_data[i,which(colnames(output_data) %in% lags$precip)] - min(output_data[i,which(colnames(output_data) %in% lags$precip)]))))
  
  # Store in table
  output_data$AIC_wt_precip[i] = round(num/denom, 3) *100
  
  return(output_data)
}

plot_gam_check = function(gam_object){
  
  # Re-formats gam.check() so that diagnostic plots are displayed on one screen
  # From Gavin Simpson on Stack Exchange, who copied from gam.check() source code
  
  par(mfrow=c(2,2))
  resid <- residuals(gam_object, type = "deviance")
  linpred <- napredict(gam_object$na.action, gam_object$linear.predictors)
  observed.y <- napredict(gam_object$na.action, gam_object$y)
  
  qq.gam(gam_object, rep = 0, level = 0.9, type = "deviance", rl.col = 2, 
         rep.col = "gray80", main = gam_object$family$family)
  hist(resid, xlab = "Residuals", main = "Histogram of residuals")
  plot(linpred, resid, main = "Resids vs. linear pred.", 
       xlab = "linear predictor", ylab = "residuals")
  plot(fitted(gam_object), observed.y, xlab = "Fitted Values", 
       ylab = "Response", main = "Response vs. Fitted Values")
  
  # Make sure to run dev.off() after this function
}

# library(mgcv)
# library(caret)
# 
# set.seed(0)
# 
# dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
# 
# b <- train(y ~ x0 + x1 + x2 + x3, 
#            data = dat,
#            method = "gam",
#            trControl = trainControl(method = "LOOCV", number = 1, repeats = 1),
#            tuneGrid = data.frame(method = "GCV.Cp", select = FALSE)
# )
# 
# print(b)
# summary(b$finalModel)

# plot best fit temp and precip plots to check
# output = fit_univariate_GAM(data = data, scale = scale)

# for(i in 1:nrow(output)){
#   if(!is.na(output$Best_Precip[i]) & !is.na(output$Best_Temp[i])){
#     tmplag = which(lags$temp == output$Best_Temp[i])
#     pptlag = which(lags$precip == output$Best_Precip[i])
# 
#     vars = prep_variables(temp = lag_table[,tmplag], precip = lag_table[,length(lags$temp) + pptlag], abundance = data[[species[i]]])
# 
#     precip_df = 100
#     if(length(unique(vars$precip)) < 100){precip_df = length(unique(vars$precip))}
# 
#     png(filename = paste0("../Results/testfits/", species[i], "gamma1p5_largek_cubic.png"), height = 400, width = 1000, units = 'px')
#     par(mfrow = c(1, 2))
#     # Fit temperature univariate model
#     temp_gam = try(gam(abundance ~ s(temp, k = 100, bs = 'cr'), data = vars, family = Gamma(link = "log"), method = "REML", gamma = 2), silent = TRUE)
#     if(class(temp_gam)[1] != "try-error"){
#       plot(temp_gam, rug = TRUERUE, shift = coef(temp_gam)[1], residuals = TRUE, ylim = c(range(log(vars$abundance))[1], range(log(vars$abundance))[2]), main = paste0(species[i], ", Temp"))
#       points(log(vars$abundance)~vars$temp, pch = 19, cex = 0.7, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.3))
#     }
#     
#     # Fit precipitation univariate model
#     precip_gam = try(gam(abundance ~ s(precip, k = precip_df, bs = 'cr'), data = vars, family = Gamma(link = "log"), method = "REML", gamma = 1.5), silent = TRUE)
# 
#     if(class(precip_gam)[1] != "try-error"){
#       plot(precip_gam, rug = TRUE, shift = coef(precip_gam)[1], residuals = TRUE, ylim = c(range(log(vars$abundance))[1], range(log(vars$abundance))[2]), main = paste0(species[i], ", Precip"))
#       points(log(vars$abundance)~vars$precip, pch = 19, cex = 0.7, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.3))
# 
#     dev.off()
#     }
#   }
# }


# k fold cross validation:
# k_fold_cross_validate = function(vars, precip_k, type, nr_folds){
#   
#   ## Create the folds ##
#   # Calculate the size of each fold
#   fold_size = length(vars$abundance)%/%nr_folds
#   # And calculate how many remainder data points there will be that don't fit evenly in folds
#   fold_remainder = length(vars$abundance)%%nr_folds
#   
#   # Set seed so that I'll have the same (random) shuffling every time
#   set.seed(27082020)
#   
#   # Shuffle the indexes of the datasets
#   shuffle = sample(1:length(vars$abundance), size = length(vars$abundance), replace = FALSE)
#   
#   # Divide up the indexes into rows for each fold, evenly distributing remainders
#   indexes = matrix(c(shuffle, rep(NA, nr_folds-fold_remainder)), nrow = nr_folds, ncol = fold_size + 1)
#   
#   # Create vector to store MAE explained in each test
#   MAE = rep(NA, nr_folds)
#   
#   # Create vector to store NMAE explained in each test
#   NMAE = rep(NA, nr_folds)
#   
#   # Create vector to store MB explained in each test
#   MB = rep(NA, nr_folds)
#   
#   ## Do k-fold cross validation
#   for(i in 1:nr_folds){
#     
#     # Split vars into test and train by using the ith row of indexes to find the testing set of data
#     test = vars[na.omit(indexes[i,]),]
#     train = vars[-na.omit(indexes[i,]), ]
#     
#     # Set precip knots to the max possible for this testing set
#     #precip_knots = 10
#     #if(length(unique(train$precip)) < 10){precip_knots = length(unique(train$precip))}
#     
#     if(type == "AR"){
#       # Train AR gam model using training set
#       gam = try(gam(abundance ~ s(temp, k = 10, bs = 'cr') + s(precip, k = precip_k, bs = 'cr') + s(log(AR1), k = 10, bs = 'cr'), 
#                     select = TRUE, data = train, family = tw,  method = "REML"), silent = TRUE)
#     }
#     
#     if(type == "nonAR"){
#       # Train a non-AR gam model using the training set of data
#       gam = try(gam(abundance ~ s(temp, k = 10, bs = 'cr') + s(precip, k = precip_k, bs = 'cr'), 
#                     select = TRUE, data = train, family = tw,  method = "REML"), silent = TRUE)
#     }
#     
#     if(class(gam)[1] != "try-error"){
#       pred = predict.gam(gam, test, se.fit = TRUE)
#       
#       # Record MAE of this test/train set in the vector
#       MAE[i] = sum(abs(exp(pred$fit) - test$abundance))/length(pred$fit)
#       
#       # Record the mean bias
#       MB[i] = sum(exp(pred$fit) - test$abundance)/length(test$abundance)
#       
#       # Record NMAE
#       NMAE[i] = sum(abs(exp(pred$fit) - test$abundance))/sum(log(test$abundance))
#     }
#     
#   }
#   
#   # Return a vector of the MAE, NMAE, MB, and number of folds used in averaging
#   return(c(round(mean(MAE, na.rm = TRUE), 3), round(mean(NMAE, na.rm = TRUE), 3), round(mean(MB, na.rm = TRUE), 3), sum(!is.na(MAE))))
#   
# }
#!/usr/bin/env python3

""" This module is a wrapper to glm_analysis.py and will run and plot datasets at all temporal aggregations """

# Author: Anne Marie Saunders (amsaunders279@gmail.com)
# Script: glm_batch.py
# Desc: This module is a wrapper to glm_analysis.py and will run and plot datasets at all temporal aggregations
# Arguments: time series of mosquito abundance with temperature and preciptiation time series
# Date: 06/18/20

# Import modules
import pandas as pd
import numpy as np
import statsmodels.api as sm
import statsmodels.formula.api as smf
import matplotlib
matplotlib.use('Agg')   
import matplotlib.pyplot as plt
from os import listdir
import re

# Define functions
def plot_GLM(xvar, axs, title, k):

    """ This function takes an array of the x variable, a number representing which subplot to use, a title describing the type of variable, and an iteration step to plot a subplot of the best-fit GLM model"""

    # Do pre-processing for x and y variables
    abun, xvar2 = prep_variables(dat[species[k]], xvar)
    
    # Define the best fit model
    model = sm.GLM(abun, xvar2, family = sm.families.Gamma(link = sm.genmod.families.links.log()), missing = 'drop').fit()

    # Use the range of the xvar to create an even array of x values    
    xpred = np.linspace(min(xvar), max(xvar), 60)
    
    # Generate polynomial features for these values
    xpred2 = PolynomialFeatures(2).fit_transform(xpred.reshape(-1,1))

    # Predict abundance using array of x values
    ypred = model.predict(xpred2)

    sig_title = title + ', p =' + str(round(model.pvalues[1],3)) + ', p2 =' + str(round(model.pvalues[2],3)) + ', R2 = ' + str(round(1- model.deviance/model.null_deviance, 3))

    # Add a subplot to the figure for this x variable
    axs.scatter(xvar2[:,1], np.log(abun), color = 'gray') # works
    axs.plot(xpred2[:,1], np.log(ypred), color = 'blue', linewidth = 1)
    axs.set_title(sig_title)
    axs.set(ylabel="log(Mean Abundance + 1)", xlabel=title)

# Run analysis on weekly data
#dat = pd.read_csv("~/Documents/Hefty_Data/Extracted_Data/Aggregated/Collier_weekly.csv")
#import glm_analysis
# for i in range(len(output)):

#     if isinstance(output.loc[i, 'Best_TempLag'], str):

#         # Create figure
#         fig, (ax1, ax2, ax3)= plt.subplots(1, 3)
#         fig.set_figheight(4)
#         fig.set_figwidth(21)
#         fig.suptitle(output.loc[i, 'Species'])

#         xvar = lag_table[output.loc[i, 'Best_TempLag']]
#         title = 'Max Daily Temp'

#         plot_GLM(xvar, ax1, title, k)

#         xvar = lag_table[output.loc[i, 'Best_PrecipDaysLag']]
#         title = 'Precipitating Days'

#         plot_GLM(xvar, ax2, title, k)

#         xvar = lag_table[output.loc[i, 'Best_PrecipMeanLag']]
#         title = 'Mean Daily Precip'

#         plot_GLM(xvar, ax3, title, k)

#         fig.savefig('../Results/GLM_fit_' + scale + '_' + species[k] + '.png', format = 'png')

#         plt.close('all')

# # Save csv of output
# output.to_csv("../Results/Lee_" + scale + "_output2.csv")

##### Main script #####

# Define locations for which to run models
locations = re.compile("|".join(['Manatee', 'Orange', 'Lee ', 'Saint Johns', 'Walton']))

# Find filenames of aggregated data
files = listdir("../Data/Extracted_Data/Aggregated")

# Sort the filenames of aggregations of desired location
files = sorted([f for f in files if locations.match(f)])

# Create a matrix of files to run: rows are locations, columns are temporal aggregations (biweekly, monthly, weekly)
files = np.array(files).reshape((len(files)//3, 3))

# Create an array of temporal scale names that matches the files matrix to be accessed by index in the loop
scale_names = np.array(['biweekly', 'monthly', 'weekly'])

#  for each temporal aggregation
for time_agg in range(files.shape[1]):

    # assign correct scale name
    scale = scale_names[time_agg]

    # Create an empty list to store output data
    outputs_list = []

    # for each location:
    for location in range(files.shape[0]):

        # Load in biweekly data
        dat = pd.read_csv("../Data/Extracted_Data/Aggregated/" + files[location][time_agg])

        # Create lagged x variables and run glm fits. Returns output table and table of variable lags
        output, lag_table = fit_glm(dat, scale) 

        # Assign a column indicating location source of dataset
        output['Location'] = np.repeat(files[location][time_agg].split("_")[0], output.shape[0]) 

        # Append output to the list of outputs
        outputs_list.append(output)

        #########################
        #### Plot if desired ####
        #########################
        
        # for k in range(len(output)):

        # if isinstance(output.loc[k, 'Best_TempLag'], str):

        #     # Create figure
        #     fig, (ax1, ax2, ax3)= plt.subplots(1, 3)
        #     fig.set_figheight(4)
        #     fig.set_figwidth(21)
        #     fig.suptitle(output.loc[k, 'Species'])

        #     xvar = lag_table[output.loc[k, 'Best_TempLag']]
        #     title = 'Max Daily Temp'

        #     plot_GLM(xvar, ax1, title, k)

        #     xvar = lag_table[output.loc[k, 'Best_PrecipDaysLag']]
        #     title = 'Precipitating Days'

        #     plot_GLM(xvar, ax2, title, k)

        #     xvar = lag_table[output.loc[k, 'Best_PrecipMeanLag']]
        #     title = 'Mean Daily Precip'

        #     plot_GLM(xvar, ax3, title, k)

        #     fig.savefig('../Results/GLM_fit_' + scale + '_' + species[k] + '.png', format = 'png')

        #     plt.close('all')

    # Join each dataset to 1 large data frame
    total_output = pd.concat(outputs_list)

    # Save a data frame at each temporal resolution to a csv
    total_output.to_csv("../Results/" + scale + "_output.csv")
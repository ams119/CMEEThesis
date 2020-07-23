#!/usr/bin/env python3

""" This module is a wrapper to glm_analysis.py and will run and plot datasets at all temporal aggregations """

# Author: Anne Marie Saunders (amsaunders279@gmail.com)
# Script: glm_batch.py
# Desc: This module is a wrapper to glm_analysis.py and will run and plot datasets at all temporal aggregations
# Arguments: time series of mosquito abundance with temperature and preciptiation time series
# Date: 06/18/20

# Import modules
import statsmodels.api as sm
import pandas as pd
import numpy as np
import matplotlib 
matplotlib.use('Agg') # Enable plotting as png
import matplotlib.pyplot as plt # For plotting
from os import listdir # to access files in system
import re # For string regex
from gam_functions import fit_GAM, prep_variables, make_laglists, plot_GAM # import functions made for plotting, model fitting, and variable pre-processing

# use class system?

##### Main script #####

# Define locations for which to run models
locations = re.compile("|".join(['Manatee', 'Orange', 'Lee', 'Saint_Johns', 'Walton']))

# Find filenames of aggregated data
files = listdir("../Data/Extracted_Data/Aggregated")

# Sort the filenames of aggregations of desired location
files = sorted([f for f in files if locations.match(f)])

# Create a matrix of files to run: rows are locations, columns are temporal aggregations (biweekly, monthly, weekly)
files = np.array(files).reshape((len(files)//3, 3))

# Create an array of temporal scale names that matches the files matrix to be accessed by index in the loop
scale_names = np.array(['biweekly', 'monthly', 'weekly'])

# time_agg, location = 0, 0

# Set the range of penalization factors for gridsearch during gam fitting:
set_lambda = np.logspace(0, 5, 11)

#  for each temporal aggregation
for time_agg in range(files.shape[1]):

    # assign correct scale name
    scale = scale_names[time_agg]

    # Create an empty list to store output data
    outputs_list = []

    # for each location:
    for location in range(files.shape[0]):

        # Find county name: 
        county = re.split("_[bwm]", files[location][time_agg])[0]

        # Announce analysis step
        print('\n***********************************\n***********************************\nNow analysing ' 
            + scale + ' ' + county 
            + ' data\n***********************************\n***********************************\n')

        # Load in biweekly data
        dat = pd.read_csv("../Data/Extracted_Data/Aggregated/" + files[location][time_agg])

        # Create lagged x variables and run glm fits. Returns output table and table of variable lags
        output, lag_table, species = fit_GAM(dat, scale, set_lambda) 

        # Assign a column indicating location source of dataset. Take this location name from the filename
        output['Location'] = np.repeat(county, output.shape[0]) 
    

        #########################
        #### Plot if desired ####
        #########################

        # Create columns to store GCV values of best fit model
        output['Temp_GCV'] = np.repeat(np.NaN, output.shape[0])
        output['PrecipDays_GCV'] = np.repeat(np.NaN, output.shape[0])
        output['PrecipMean_GCV'] = np.repeat(np.NaN, output.shape[0])

        # Create columns to store estimated degrees of freedom of best fit model
        output['Temp_edof'] = np.repeat(np.NaN, output.shape[0])
        output['PrecipDays_edof'] = np.repeat(np.NaN, output.shape[0])
        output['PrecipMean_edof'] = np.repeat(np.NaN, output.shape[0])

        # Create columns to store p values of best fit model
        output['Temp_p'] = np.repeat(np.NaN, output.shape[0])
        output['PrecipDays_p'] = np.repeat(np.NaN, output.shape[0])
        output['PrecipMean_p'] = np.repeat(np.NaN, output.shape[0])
        
        for k in range(output.shape[0]):

            if isinstance(output.loc[k, 'Best_TempLag'] or output.loc[k, 'Best_PrecipDaysLag'] or output.loc[k, 'Best_PrecipMeanLag'], str):

                fig, (ax1, ax2, ax3)= plt.subplots(1, 3)
                fig.set_figheight(4)
                fig.set_figwidth(21)
                fig.suptitle(output.loc[k, 'Species'])

                if isinstance(output.loc[k, 'Best_TempLag'], str):

                    # Create figure
                    xvar = lag_table[output.loc[k, 'Best_TempLag']]
                    title = 'Max ' + scale + ' Temp'

                    gam = plot_GAM(dat, species, xvar, ax1, title, k, set_lambda)
                    output.loc[k, 'Temp_GCV'] = gam.statistics_['GCV']
                    output.loc[k, 'Temp_edof'] = gam.statistics_['edof']
                    output.loc[k, 'Temp_p'] = gam.statistics_['p_values'][0]


                if isinstance(output.loc[k, 'Best_PrecipDaysLag'], str):

                    xvar = lag_table[output.loc[k, 'Best_PrecipDaysLag']]
                    title = 'Precipitating Days'

                    gam = plot_GAM(dat, species, xvar, ax2, title, k, set_lambda)
                    output.loc[k, 'PrecipDays_GCV'] = gam.statistics_['GCV']
                    output.loc[k, 'PrecipDays_edof'] = gam.statistics_['edof']
                    output.loc[k, 'PrecipDays_p'] = gam.statistics_['p_values'][0]

                if isinstance(output.loc[k, 'Best_PrecipMeanLag'], str):    

                    xvar = lag_table[output.loc[k, 'Best_PrecipMeanLag']]
                    title = 'Mean ' + scale + ' Precip'

                    gam = plot_GAM(dat, species, xvar, ax3, title, k, set_lambda)
                    output.loc[k, 'PrecipMean_GCV'] = gam.statistics_['GCV']
                    output.loc[k, 'PrecipMean_edof'] = gam.statistics_['edof']
                    output.loc[k, 'PrecipMean_p'] = gam.statistics_['p_values'][0]

                fig.savefig('../Results/GAM_Plots/' + scale + '_' + output['Location'][0] 
                    + '_' + species[k] + '.png', format = 'png')


                plt.close('all')

        # Append output to the list of outputs
        outputs_list.append(output)

    # Join each dataset to 1 large data frame
    total_output = pd.concat(outputs_list)

    # Save a data frame at each temporal resolution to a csv
    total_output.to_csv("../Results/GAM_output_" + scale + ".csv", index = False)

    
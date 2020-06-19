#!/usr/bin/env python3

""" This module is a wrapper to glm_analysis.py and will run and plot datasets at all temporal aggregations """

# Author: Anne Marie Saunders (amsaunders279@gmail.com)
# Script: glm_batch.py
# Desc: This module is a wrapper to glm_analysis.py and will run and plot datasets at all temporal aggregations
# Arguments: time series of mosquito abundance with temperature and preciptiation time series
# Date: 06/18/20

# Import modules
import pandas as pd
import matplotlib
matplotlib.use('Agg')   
import matplotlib.pyplot as plt

# Define functions
def plot_GLM(xvar, axs, title):
    # Do pre-processing for x and y variables
    abun, xvar2 = prep_variables(dat[species[i]], xvar)
    
    # Define the best fit model
    model = sm.GLM(abun, xvar2, family = sm.families.Gamma(link = sm.genmod.families.links.log()), missing = 'drop').fit()

    # Use the range of the xvar to create an even array of x values    
    xpred = np.linspace(min(xvar), max(xvar), 60)
    
    # Generate polynomial features for these values
    xpred2 = PolynomialFeatures(2).fit_transform(xpred.reshape(-1,1))

    # Predict abundance using array of x values
    ypred = model.predict(xpred2)

    sig_title = title + ', p =' + str(round(model.pvalues[1],4)) + ', p2 =' + str(round(model.pvalues[2],4)) + ', R2 = ' + str(1- model.deviance/model.null_deviance)

    # Add a subplot to the figure for this x variable
    axs.scatter(xvar2[:,1], np.log(abun), color = 'gray') # works
    axs.plot(xpred2[:,1], np.log(ypred), color = 'blue', linewidth = 1)
    axs.set_title(sig_title)

# Run analysis on weekly data
#dat = pd.read_csv("~/Documents/Hefty_Data/Extracted_Data/Aggregated/Collier_weekly.csv")
#import glm_analysis
for i in range(len(output)):

    if isinstance(output.loc[i, 'Best_TempLag'], str):

        # Create figure
        fig, (ax1, ax2, ax3)= plt.subplots(1, 3)
        fig.set_figheight(4)
        fig.set_figwidth(21)
        fig.suptitle(output.loc[i, 'Species'])

        xvar = lag_table[output.loc[i, 'Best_TempLag']]
        title = 'Max Temp'

        plot_GLM(xvar, ax1, title)

        xvar = lag_table[output.loc[i, 'Best_PrecipDaysLag']]
        title = 'Precip Days'

        plot_GLM(xvar, ax2, title)

        xvar = lag_table[output.loc[i, 'Best_PrecipMeanLag']]
        title = 'Precip Mean'

        plot_GLM(xvar, ax3, title)

        fig.savefig('../Results/GLM_fit_' + scale + '_' + species[i] + '.png', format = 'png')

        plt.close('all')

# Save csv of output
output.to_csv("../Results/Lee_" + scale + "_output2.csv")

# Run analysis on biweekly data
dat = pd.read_csv("~/Documents/Hefty_Data/Extracted_Data/Aggregated/Collier_weekly.csv")
import glm_analysis.py
for i in range(len(output)):

    if isinstance(output.loc[i, 'Best_TempLag'], str):

        # Create figure
        fig = plt.figure()

        xvar = lag_table[output.loc[i, 'Best_TempLag']]
        plt_nr = 1

        plot_GLM(xvar, plt_nr)

        xvar = lag_table[output.loc[i, 'Best_PrecipDaysLag']]
        plt_nr = 2

        plot_GLM(xvar, plt_nr)

        xvar = lag_table[output.loc[i, 'Best_PrecipMeanLag']]
        plt_nr = 3

        plot_GLM(xvar, plt_nr)

        fig.savefig('../Results/GLM_fit_' + scale + '_' + species[i] + '.png', format = 'png')

        plt.close('all')
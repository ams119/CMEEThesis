#!/usr/bin/env python3

""" This module will create functions for lagged time series of 
metoerological data and run glms of these lagged predictor variables with 
logged abundance at a weekly time scale. The AICs of each 
predictor lag will be stored in an output table. """

# Author: Anne Marie Saunders (amsaunders279@gmail.com)
# Script: glm_analysis.py
# Desc:This module will create functions for lagged time series of metoerological data and run glms of these lagged predictor variables with logged abundance at a weekly time scale. The AICs of each predictor lag will be stored in an output table.
# Arguments: time series of mosquito abundance with temperature and preciptiation time series
# Date: 06/11/20

####  Imports ####
import statsmodels.api as sm
import pandas as pd 
import numpy as np
import matplotlib.pyplot as plt # move to batch, probably
from pygam import GammaGAM

#### Functions ####

def make_laglists(variable_str, scale):
    
    """ This function takes the scale of the data and 
    a string describing the variable and returns a list of 
    strings to make column names of the appropriate lags. """

    if scale == 'weekly':
        lags = [variable_str + '_lag' + str(i) for i in range(6)]
    
    if scale == 'biweekly':
        lags = [variable_str + '_lag' + str(i) for i in range(4)]
    
    if scale == 'monthly':
        lags = [variable_str + '_lag' + str(i) for i in range(3)]    
    
    return lags


def prep_variables(yvar, xvar):

    """ Pre-processes x and y variables for GAM by 
    removing x and y variable NAs and adding 1 
    to y variable for logging"""

    # Find indexes of NaN values in x and y variables
    yvar_index = yvar.index[yvar.apply(np.isnan)]
    xvar_index = xvar.index[xvar.apply(np.isnan)]

    # Remove rows in xvar and yvar that are NaN in x and/or y
    yvar = yvar.drop(yvar_index.union(xvar_index))
    xvar = xvar.drop(yvar_index.union(xvar_index))

    # Add 1 to to yvar to avoid -Inf values when it is logged
    yvar = yvar + 1
    
    return yvar, xvar

def fit_GAM(dat, scale, set_lambda):

    """ This function takes a pandas data frame of species and x variable time series data 
    and a "scale" variable describing the temporal aggregation (weekly, biweekly, or monthly).
    A table of lagged x variables is created and univariate quadratic GLMS are 
    run with each lag and species abundance. The function then returns and output 
    table of AIC values of these models and the table of lagged variables. """

    #### Create ouput table for AIC values of univariate models ####

    # Obtain row names: list of species from data frame. Columns depend on temporal scale

    if scale == 'weekly':
        species = dat.columns[9:(dat.shape[1]-4)]

    if scale == 'biweekly':
        species = dat.columns[9:(dat.shape[1]-3)]

    if scale == 'monthly':
        species = dat.columns[9:(dat.shape[1]-3)]

    # Create column names 

    temp_lags = make_laglists('temp', scale)
    precipday_lags = make_laglists('precipdays', scale)
    precipmean_lags = make_laglists('precipmean', scale)

    colnames = ["Species"] + temp_lags + precipday_lags + precipmean_lags 

    # Create a matrix of with row for every species and column for every AIC value, populated with NAs
    matrix = np.empty((len(species), len(colnames) + 2))

    # Populate the matrix with NaN
    matrix[:] = np.NaN

    # Convert to data frame
    output = pd.DataFrame(matrix, columns = colnames + ['nr_obs'] + ['z_inflation_pct'])

    # Populate first column with species names
    output['Species'] = species.tolist()

    ## Create empty data frame for storing lagged values
    matrix = np.empty((dat.shape[0], len(colnames)))
    matrix[:] = np.NaN 

    lag_table = pd.DataFrame(matrix, columns = ['date'] + colnames[1:])

    # Populate lag table

    # include the date as the first column, according to temporal scale
    if scale == 'weekly':
        lag_table['date'] = dat['date_w']

    if scale == 'biweekly':
        sep = '.'
        # Create a string column of YYYY.BW
        lag_table['date'] = date_bw = [n + '.' + m for m,n in zip(dat['bw_num'].apply(str),dat['Year'].apply(str))]

    if scale == 'monthly':
        lag_table['date'] = dat['date_m']

    # For each incremental lag level, shift column up by 1 and add 1 NaN at the end
    for i in range(len(temp_lags)): # for maximum temperature
        lag_table.loc[0:(lag_table.shape[0]-1-i),[temp_lags[i]]] = dat.loc[i:dat.shape[0],['temp_mean']].values

    for i in range(len(precipday_lags)): # for number of days where it rained
        lag_table.loc[0:(lag_table.shape[0]-1-i),[precipday_lags[i]]] = dat.loc[i:dat.shape[0],['precip_days']].values

    for i in range(len(precipmean_lags)): # for mean precipitation
        lag_table.loc[0:(lag_table.shape[0]-1-i),[precipmean_lags[i]]] = dat.loc[i:dat.shape[0],['precip_mean']].values



    ############################################
    # Find Best Fit Time Lags for Each Species #
    ############################################

    for i in range(len(species)):

        #import ipdb; ipdb.set_trace()
        
        print('\n-------------------------------\n\nNow evaluating species ' 
            + str(i) + ', ' + species[i] + '\n', sep = " ")

        # Record the number of observations for this dataset
        output.loc[i, 'nr_obs'] = np.isfinite(dat[species[i]]).sum()  

        # Calculate the degree of zero-inflation in this data
        output.loc[i, 'z_inflation_pct'] = round((dat[species[i]] == 0).sum()/len(dat[species[i]])*100, 1)

        # Conduct univariate models for temperature # 

        for j in range(len(temp_lags)):

            abun, temp = prep_variables(dat[species[i]], lag_table[temp_lags[j]])
            
            gam = GammaGAM(max_iter = 10, lam = 60).gridsearch(temp[:,None], abun, n_splines = np.array([5,10,15,20]))
                
            output.loc[i, [temp_lags[j]]] = gam.statistics_['AIC']

        # Conduct univariate model for precipitation days
        
        for j in range(len(precipday_lags)):

            abun, precipday = prep_variables(dat[species[i]], lag_table[precipday_lags[j]])
            
            gam = GammaGAM(max_iter = 10, lam = 60).gridsearch(precipday[:,None], abun, n_splines = np.array([5,10,15,20]))
                
            output.loc[i, [precipday_lags[j]]] = gam.statistics_['AIC']

        # conduct univariate model for mean precipitation

        for j in range(len(precipmean_lags)):

            abun, precipmean = prep_variables(dat[species[i]], lag_table[precipmean_lags[j]])
            
            gam = GammaGAM(max_iter = 10, lam = 60).gridsearch(precipmean[:,None], abun, n_splines = np.array([5,10,15,20]))
                
            output.loc[i, [precipmean_lags[j]]] = gam.statistics_['AIC']

            
    # Find the best fit lags for each type of meteorological variable by choosing the column with the smallest AIC

    temp_end = 1 + len(temp_lags)
    output['Best_TempLag'] = output.iloc[:,1:temp_end].idxmin(axis = 1, skipna = True)

    pptday_end = temp_end + len(precipday_lags)
    output['Best_PrecipDaysLag'] = output.iloc[:,temp_end:pptday_end].idxmin(axis = 1, skipna = True)

    pptmean_end = pptday_end + len(precipday_lags)
    output['Best_PrecipMeanLag'] = output.iloc[:,pptday_end:pptmean_end].idxmin(axis = 1, skipna = True)

    output['Best_PrecipLag'] = output.iloc[:,temp_end:pptmean_end].idxmin(axis = 1, skipna = True)

    return output, lag_table, species



def plot_GAM(dat, species, xvar, axs, title, k, set_lambda):

    """ This function takes an array of the x variable, a number representing which subplot to use, 
    a title describing the type of variable, and an iteration step to plot a subplot of the best-fit GLM model"""

    # Do pre-processing for x and y variables
    abun, xvar2 = prep_variables(dat[species[k]], xvar)
    
    # Define the best fit model
    gam = GammaGAM(max_iter = 10, lam = 60).gridsearch(xvar2[:,None], abun, n_splines = np.array([5,10,15,20]))

    XX = gam.generate_X_grid(term = 0)

    sig_title = title + ', lambda = ' + str(gam.lam[0][0]) + ', n_splines = ' + str(gam.n_splines)

    # Add a subplot to the figure for this x variable
    axs.scatter(xvar2, np.log(abun), color = 'gray') 

    # Plot GAM Curve
    axs.plot(XX, np.log(gam.predict(XX)), color = 'blue', linewidth = 1)

    # Plot 95% confidence intervals
    axs.plot(XX, np.log(gam.confidence_intervals(XX)), color = 'black', ls = '--')

    axs.set_title(sig_title)
    axs.set(ylabel="log(Mean Abundance + 1)", xlabel=title)

    return gam
# seaborn seems to create a plot with a beautiful fit but I believe it is doing this with linear regression not GLM (not taking into account gamma dist of data)
# import seaborn as sns
# matplotlib.use('TkAgg')

# df = pd.DataFrame(data = np.column_stack((temp[:,1], np.log(abun))), columns = ['temp', 'log_abun'])
# sns.lmplot(x= 'temp', y= 'log_abun', data = df,order=2, ci=None, scatter_kws={"s": 80})
# plt.show()

# # distribution plot
# sns.distplot(xvar, hist = True, kde = True,kde_kws = {'linewidth': 3},)
# plt.show()


#!/usr/bin/env python3

""" This module will create lagged time series of metoerological data and run glms of these lagged predictor variables with logged abundance at a weekly time scale. The AICs of each predictor lag will be stored and output into a csv. """

# Author: Anne Marie Saunders (amsaunders279@gmail.com)
# Script: glm_analysis.py
# Desc: Use glms to find best fit lags of temperature and precipitation to predict mosquito abundance at weekly time scale
# Arguments: time series of mosquito abundance with temperature and preciptiation time series
# Date: 06/11/20

####  Imports ####
import statsmodels.api as sm
import statsmodels.formula.api as smf
import pandas as pd 
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

#### Functions ####

def prep_variables(yvar, xvar):

    """ Pre-processes x and y variables for GLM by removing x and matching y variable NAs, adding 1 to y variable for logging, and creating polynomial features such as the constant and squared term"""

    NaNs = pd.isna(xvar).sum()
    # Remove the trailing mismatch in xvar and yvar where xvar = NaN
    if NaNs > 0:
        yvar, xvar = yvar[:-NaNs,], xvar[:-NaNs,]

    # Generate degree 2 polynomial features- constant and squared term- for the x variable
    # Create 2d array with columns: constant (1), xvar, and xvar squared
    xvar = np.column_stack((np.ones(len(xvar)), xvar, xvar**2))

    # Add 1 to to yvar to avoid -Inf values when it is logged
    abun = yvar + 1
    
    return abun, xvar

def fit_glm(dat, scale):
    #### Create ouput table for AIC values of univariate models ####
    # Create column names 
    temp_lags = ['temp_lag0', 'temp_lag1', 'temp_lag2', 'temp_lag3', 'temp_lag4', 'temp_lag5']

    precipday_lags = ['precipdays_lag0', 'precipdays_lag1', 'precipdays_lag2', 'precipdays_lag3', 'precipdays_lag4', 'precipdays_lag5']

    precipmean_lags = ['precipmean_lag0', 'precipmean_lag1', 'precipmean_lag2', 'precipmean_lag3', 'precipmean_lag4', 'precipmean_lag5']

    # Adjust lag lists and species lists based on temporal scale

    if scale == 'weekly':
        species = dat.columns[9:(dat.shape[1]-4)]

    if scale == 'biweekly':
        species = dat.columns[9:(dat.shape[1]-3)]
        temp_lags = temp_lags[:4]
        precipday_lags = precipday_lags[:4]
        precipmean_lags = precipmean_lags[:4]

    if scale == 'monthly':
        species = dat.columns[9:(dat.shape[1]-3)]
        temp_lags = temp_lags[:2]
        precipday_lags = precipday_lags[:2]
        precipmean_lags = precipmean_lags[:2]

    colnames = ["Species"] + temp_lags + precipday_lags + precipmean_lags 

    # Create a matrix of with row for every species and column for every AIC value, populated with NAs
    matrix = np.empty((len(species), len(colnames) + 2))
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
        
        print('\n-------------------------------\n\nNow evaluating species ' + str(i) + ', ' + species[i] + '\n', sep = " ")

        # Record the number of observations for this dataset
        output.loc[i, 'nr_obs'] = np.isfinite(dat[species[i]]).sum()  

        # Calculate the degree of zero-inflation in this data
        output.loc[i, 'z_inflation_pct'] = round((dat[species[i]] == 0).sum()/len(dat[species[i]])*100, 1)

        # Conduct univariate models for temperature # 

        for j in range(len(temp_lags)):

            abun, temp = prep_variables(dat[species[i]], lag_table[temp_lags[j]])
            
            try:
                model = sm.GLM(abun, temp, family = sm.families.Gamma(link = sm.genmod.families.links.log()), missing = 'drop').fit()

                output.loc[i, [temp_lags[j]]] = model.aic

            except:
                output.loc[i, [temp_lags[j]]] = np.NaN

        # Conduct univariate model for precipitation days
        
        for j in range(len(precipday_lags)):

            abun, precipday = prep_variables(dat[species[i]], lag_table[precipday_lags[j]])
            
            try: 
                model = sm.GLM(abun, precipday, family = sm.families.Gamma(link = sm.genmod.families.links.log()), missing = 'drop').fit()

                output.loc[i, [precipday_lags[j]]] = model.aic

            except:
                output.loc[i, [precipday_lags[j]]] = np.NaN


        # conduct univariate model for mean precipitation

        for j in range(len(precipmean_lags)):

            abun, precipmean = prep_variables(dat[species[i]], lag_table[precipmean_lags[j]])
            
            try:
                model = sm.GLM(abun, precipmean, family = sm.families.Gamma(link = sm.genmod.families.links.log()), missing = 'drop').fit(method = 'ncg')

                output.loc[i, [precipmean_lags[j]]] = model.aic

            except:
                output.loc[i, [precipmean_lags[j]]] = np.NaN


    # Find the best fit lags for each type of meteorological variable by choosing the column with the smallest AIC

    temp_end = 1 + len(temp_lags)
    output['Best_TempLag'] = output.iloc[:,1:temp_end].idxmin(axis = 1, skipna = True)

    pptday_end = temp_end + len(precipday_lags)
    output['Best_PrecipDaysLag'] = output.iloc[:,temp_end:pptday_end].idxmin(axis = 1, skipna = True)

    pptmean_end = pptday_end + len(precipday_lags)
    output['Best_PrecipMeanLag'] = output.iloc[:,pptday_end:pptmean_end].idxmin(axis = 1, skipna = True)

    output['Best PrecipLag'] = output.iloc[:,temp_end:pptmean_end].idxmin(axis = 1, skipna = True)

    return output, lag_table

# seaborn seems to create a plot with a beautiful fit but I believe it is doing this with linear regression not GLM (not taking into account gamma dist of data)
# import seaborn as sns
# matplotlib.use('TkAgg')

# df = pd.DataFrame(data = np.column_stack((temp[:,1], np.log(abun))), columns = ['temp', 'log_abun'])
# sns.lmplot(x= 'temp', y= 'log_abun', data = df,order=2, ci=None, scatter_kws={"s": 80})
# plt.show()

# # distribution plot
# sns.distplot(xvar, hist = True, kde = True,kde_kws = {'linewidth': 3},)
# plt.show()


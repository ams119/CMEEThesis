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

    """ Pre-processes x and y variables for GLM by 
    removing x and matching y variable NAs, adding 1 
    to y variable for logging, and creating polynomial 
    features such as the constant and squared term"""

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

    """ This function takes a pandas data frame of species and x variable time series data 
    and a "scale" variable describing the temporal aggregation (weekly, biweekly, or monthly).
    A table of lagged x variables is created and univariate quadratic GLMS are 
    run with each lag and species abundance. The function then returns and output 
    table of AIC values of these models and the table of lagged variables. """
    
    #### Create ouput table for AIC values of univariate models ####
    # Create column names 

    temp_lags = make_laglists('temp', scale)
    precipday_lags = make_laglists('precipdays', scale)
    precipmean_lags = make_laglists('precipmean', scale)

    colnames = ["Species"] + temp_lags + precipday_lags + precipmean_lags 

    # Obtain list of species from data frame: columns depend on temporal scale

    if scale == 'weekly':
        species = dat.columns[9:(dat.shape[1]-4)]

    if scale == 'biweekly':
        species = dat.columns[9:(dat.shape[1]-3)]

    if scale == 'monthly':
        species = dat.columns[9:(dat.shape[1]-3)]


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
        
        print('\n-------------------------------\n\nNow evaluating species ' 
            + str(i) + ', ' + species[i] + '\n', sep = " ")

        # Record the number of observations for this dataset
        output.loc[i, 'nr_obs'] = np.isfinite(dat[species[i]]).sum()  

        # Calculate the degree of zero-inflation in this data
        output.loc[i, 'z_inflation_pct'] = round((dat[species[i]] == 0).sum()/len(dat[species[i]])*100, 1)

        # Conduct univariate models for temperature # 

        for j in range(len(temp_lags)):

            abun, temp = prep_variables(dat[species[i]], lag_table[temp_lags[j]])
            
            try:
                model = sm.GLM(abun, temp, family = sm.families.Gamma(link = sm.genmod.families.links.log()), 
                    missing = 'drop').fit()

                output.loc[i, [temp_lags[j]]] = model.aic

            except:
                output.loc[i, [temp_lags[j]]] = np.NaN

        # Conduct univariate model for precipitation days
        
        for j in range(len(precipday_lags)):

            abun, precipday = prep_variables(dat[species[i]], lag_table[precipday_lags[j]])
            
            try: 
                model = sm.GLM(abun, precipday, family = sm.families.Gamma(link = sm.genmod.families.links.log()), 
                    missing = 'drop').fit()

                output.loc[i, [precipday_lags[j]]] = model.aic

            except:
                output.loc[i, [precipday_lags[j]]] = np.NaN


        # conduct univariate model for mean precipitation

        for j in range(len(precipmean_lags)):

            abun, precipmean = prep_variables(dat[species[i]], lag_table[precipmean_lags[j]])
            
            try:
                model = sm.GLM(abun, precipmean, family = sm.families.Gamma(link = sm.genmod.families.links.log()), 
                    missing = 'drop').fit(method = 'ncg')

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

    output['Best_PrecipLag'] = output.iloc[:,temp_end:pptmean_end].idxmin(axis = 1, skipna = True)

    return output, lag_table, species



def plot_GLM(dat, species, xvar, axs, title, k):

    """ This function takes an array of the x variable, a number representing which subplot to use, 
    a title describing the type of variable, and an iteration step to plot a subplot of the best-fit GLM model"""

    # Do pre-processing for x and y variables
    abun, xvar2 = prep_variables(dat[species[k]], xvar)
    
    try:
        # Define the best fit model
        model = sm.GLM(abun, xvar2, family = sm.families.Gamma(link = sm.genmod.families.links.log()), 
            missing = 'drop').fit()

        # Use the range of the xvar to create an even array of x values    
        xpred = np.linspace(min(xvar), max(xvar), 60)

        # Generate degree 2 polynomial features- constant and squared term- for the x variable
        # Create 2d array with columns: constant (1), xvar, and xvar squared
        xpred2 = np.column_stack((np.ones(len(xpred)), xpred, xpred**2))

        # Predict abundance using array of x values
        ypred = model.predict(xpred2)

        sig_title = title + ', p =' + str(round(model.pvalues[1],3)) + ', p2 =' + str(round(model.pvalues[2],3)) + ', R2 = ' + str(round(1- model.deviance/model.null_deviance, 3))

        # Add a subplot to the figure for this x variable
        axs.scatter(xvar2[:,1], np.log(abun), color = 'gray') 
        axs.plot(xpred2[:,1], np.log(ypred), color = 'blue', linewidth = 1)
        axs.set_title(sig_title)
        axs.set(ylabel="log(Mean Abundance + 1)", xlabel=title)

    except ValueError:
        axs.scatter(xvar2[:,1], np.log(abun), color = 'gray') 
        axs.set_title(title + " : Model for Plotting Failed")
        axs.set(ylabel="log(Mean Abundance + 1)", xlabel=title)

# seaborn seems to create a plot with a beautiful fit but I believe it is doing this with linear regression not GLM (not taking into account gamma dist of data)
# import seaborn as sns
# matplotlib.use('TkAgg')

# df = pd.DataFrame(data = np.column_stack((temp[:,1], np.log(abun))), columns = ['temp', 'log_abun'])
# sns.lmplot(x= 'temp', y= 'log_abun', data = df,order=2, ci=None, scatter_kws={"s": 80})
# plt.show()

# # distribution plot
# sns.distplot(xvar, hist = True, kde = True,kde_kws = {'linewidth': 3},)
# plt.show()


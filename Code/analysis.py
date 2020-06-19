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
from sklearn.preprocessing import PolynomialFeatures

#### Functions ####

def prep_variables(yvar, xvar):

    NaNs = pd.isna(xvar).sum()
    # sklearn doesn't like NaN values, so remove the trailing mismatch in xvar and yvar where xvar = NaN
    if NaNs > 0:
        yvar, xvar = yvar[:-NaNs,], xvar[:-NaNs,]

    # Generate degree 2 polynomial features- constant and squared term- for the x variable
    xvar = PolynomialFeatures(2).fit_transform(xvar.values.reshape(-1,1))

    # Add 1 to to yvar to avoid -Inf values when it is logged
    abun = yvar + 1
    
    return abun, xvar

#### Main Code #####

# Read in data at weekly resolution
#dat = pd.read_csv("~/Documents/Hefty_Data/Extracted_Data/Aggregated/Collier_biweekly.csv")

# Determine which scale the data is aggregated to
if 'date_w' in dat.columns.to_list():
    scale = 'weekly'    

if 'bw_num' in dat.columns.to_list():
    scale = 'biweekly'

if 'date_m' in dat.columns.to_list():
    scale = 'monthly'


#### Create ouput table for AIC values of univariate models ####
# Create column names 
temp_lags = ['temp_lag0', 'temp_lag1', 'temp_lag2', 'temp_lag3', 'temp_lag4', 'temp_lag5', 'temp_lag6', 'temp_lag7', 'temp_lag8', 'temp_lag9']

precipday_lags = ['precipdays_lag0', 'precipdays_lag1', 'precipdays_lag2', 'precipdays_lag3', 'precipdays_lag4', 'precipdays_lag5', 'precipdays_lag6', 'precipdays_lag7', 'precipdays_lag8', 'precipdays_lag9']

precipmean_lags = ['precipmean_lag0', 'precipmean_lag1', 'precipmean_lag2', 'precipmean_lag3', 'precipmean_lag4', 'precipmean_lag5', 'precipmean_lag6', 'precipmean_lag7', 'precipmean_lag8', 'precipmean_lag9']

# Adjust lag lists and species lists based on temporal scale

if scale == 'weekly':
    species = dat.columns[9:(dat.shape[1]-4)]

if scale == 'biweekly':
    species = dat.columns[9:(dat.shape[1]-3)]
    temp_lags = temp_lags[:6]
    precipday_lags = precipday_lags[:6]
    precipmean_lags = precipmean_lags[:6]

if scale == 'monthly':
    species = dat.columns[9:(dat.shape[1]-3)]
    temp_lags = temp_lags[:3]
    precipday_lags = precipday_lags[:3]
    precipmean_lags = precipmean_lags[:3]

colnames = ["Species"] + temp_lags + precipday_lags + precipmean_lags 

# Create a matrix of with row for every species and column for every AIC value, populated with NAs
matrix = np.empty((len(species), len(colnames) + 1))
matrix[:] = np.NaN

# Convert to data frame
output = pd.DataFrame(matrix, columns = colnames + ['nr_obs'])

# Populate first column with species names
output['Species'] = species.tolist()



## Create empty data frame for storing lagged values
matrix = np.empty((dat.shape[0], len(colnames)))
matrix[:] = np.NaN 

lag_table = pd.DataFrame(matrix, columns = ['date'] + colnames[1:])

# Populate lag table

# include the date as the first column, according to temporal scale
if scale = 'weekly':
    lag_table['date'] = dat['date_w']

if scale = 'biweekly':
    sep = '.'
    # Create a string column of YYYY.BW
    lag_table['date'] = date_bw = [n + '.' + m for m,n in zip(dat['bw_num'].apply(str),dat['Year'].apply(str))]

if scale = 'monthly':
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
    
    # Record the number of observations for this dataset
    output.loc[i, 'nr_obs'] = np.isfinite(dat[species[i]]).sum()  

    # Conduct univariate models for temperature # 

    for j in range(len(temp_lags)):

        abun, temp = prep_variables(dat[species[i]], lag_table[temp_lags[j]])
        
        try:
            model = sm.GLM(abun, temp, family = sm.families.Gamma(link = sm.genmod.families.links.log), missing = 'drop').fit()

            output.loc[i, [temp_lags[j]]] = model.aic

        except:
            output.loc[i, [temp_lags[j]]] = np.NaN

    # Conduct univariate model for precipitation days
    
    for j in range(len(precipday_lags)):

        abun, precipday = prep_variables(dat[species[i]], lag_table[precipday_lags[j]])
        
        try: 
            model = sm.GLM(abun, precipday, family = sm.families.Gamma(link = sm.genmod.families.links.log), missing = 'drop').fit()

            output.loc[i, [precipday_lags[j]]] = model.aic

        except:
            output.loc[i, [precipday_lags[j]]] = np.NaN


    # conduct univariate model for mean precipitation

    for j in range(len(precipmean_lags)):

        abun, precipmean = prep_variables(dat[species[i]], lag_table[precipmean_lags[j]])
        
        try:
            model = sm.GLM(abun, precipmean, family = sm.families.Gamma(link = sm.genmod.families.links.log), missing = 'drop').fit(method = 'ncg')

            output.loc[i, [precipmean_lags[j]]] = model.aic

        except:
            output.loc[i, [precipmean_lags[j]]] = np.NaN


# Find the best fit lags for each type of meteorological variable
output['Best_TempLag'] = output.iloc[:,1:11].idxmin(axis = 1, skipna = True)

output['Best_PrecipDaysLag'] = output.iloc[:,11:21].idxmin(axis = 1, skipna = True)

output['Best_PrecipMeanLag'] = output.iloc[:,21:31].idxmin(axis = 1, skipna = True)

output['Best PrecipLag'] = output.iloc[:,11:31].idxmin(axis = 1, skipna = True)

# seaborn seems to create a plot with a beautiful fit but I believe it is doing this with linear regression not GLM (not taking into account gamma dist of data)
#import seaborn as sns

#df = pd.DataFrame(data = np.column_stack((temp[:,1], np.log(abun))), columns = ['temp', 'log_abun'])
#sns.lmplot(x= 'temp', y= 'log_abun', data = df,order=2, ci=None, scatter_kws={"s": 80})
#plt.show()

# distribution plot
#sns.distplot(xvar, hist = False, kde = True,kde_kws = {'linewidth': 3},)
#plt.show()'


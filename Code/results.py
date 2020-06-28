#!/usr/bin/env python3

""" This script examines the outputs of the univariate models of meteorological variables and abundance"""

# Author: Anne Marie Saunders (amsaunders279@gmail.com)
# Script: results.py
# Arguments: output_weekly.csv, output_biweekly.csv, output_monthly.csv
# Date: 06/26/20

# Imports
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

###### Main Code ######

# Load csvs of model fit results
weekly = pd.read_csv('../Results/output_weekly.csv')
biweekly = pd.read_csv('../Results/output_biweekly.csv')
monthly = pd.read_csv('../Results/output_monthly.csv')

data = [weekly, biweekly, monthly]

indexes = list(set(biweekly['Location']))

weekly_means = []
biweekly_means = []
monthly_means = []

weekly_days = []
biweekly_days = []
monthly_days = []

# https://stackoverflow.com/questions/53228762/matplotlib-double-bar-graph-with-pandas-series/53229815  
for location in range(indexes):
    wk_data = weekly.loc[weekly['Location'] == indexes[location],]
    bw_data = biweekly.loc[biweekly['Location'] == indexes[location]]
    mnth_data = monthly.loc[monthly['Location'] == indexes[location]]

    # Counts instances where precip means wins
    weekly_means.append(len(wk_data[wk_data['Best PrecipLag'].str.contains('mean', na = False)]))
    biweekly_means.append(len(bw_data[bw_data['Best PrecipLag'].str.contains('mean', na = False)]))
    monthly_means.append(len(mnth_data[mnth_data['Best PrecipLag'].str.contains('mean', na = False)]))

    # Counts instances where precip days wins
    weekly_days.append(len(wk_data[wk_data['Best PrecipLag'].str.contains('days', na = False)]))
    biweekly_days.append(len(bw_data[bw_data['Best PrecipLag'].str.contains('days', na = False)]))
    monthly_days.append(len(mnth_data[mnth_data['Best PrecipLag'].str.contains('days', na = False)]))

wk_df = pd.DataFrame({"Number of Days Wins":weekly_days, "Number of Mean Wins":weekly_means})
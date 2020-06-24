#!/bin/env Rscript
# Author: Anne Marie Saunders
# Script: fix_dates.R
# Desc: fix annoying date entries
# Arguments: ~/Documents/Hefty_Data/mosquitoAbun/fdcounts.csv
# Date: 04/17/20

# Import libraries
library(stringr)

# Load in dataset
fddata = read.csv("../Data/fdcounts.csv", stringsAsFactors = F)

# Check the date column is character
class(fddata$Collection.date.range) == 'character'

# find the rows that are formatted simply
indexes = grep("^\\d\\d\\d\\d-\\d\\d-\\d\\d$", fddata$Collection.date.range)

# Remove these indexes to find problematic date formats
probs = fddata[-indexes,]

# Isolate these to only unique instances
uprobs = unique(probs$Collection.date.range)

# Examine what some of these problematic formats look like
uprobs

# As of June 2020, only 5 formats of problematic dates in data:
TOprobs = grep("^\\d\\d\\d\\d-\\d\\d-\\d\\d TO \\d\\d\\d\\d-\\d\\d-\\d\\d$", probs$Collection.date.range)
commaprobs = grep("^\\d\\d\\d\\d-\\d\\d-\\d\\d,\\d\\d\\d\\d-\\d\\d-\\d\\d$", probs$Collection.date.range)
three_TOprobs = grep("^\\d\\d\\d\\d-\\d\\d-\\d\\d,\\d\\d\\d\\d-\\d\\d-\\d\\d TO \\d\\d\\d\\d-\\d\\d-\\d\\d$", probs$Collection.date.range)
three_commaprobs = grep("^\\d\\d\\d\\d-\\d\\d-\\d\\d,\\d\\d\\d\\d-\\d\\d-\\d\\d,\\d\\d\\d\\d-\\d\\d-\\d\\d$", probs$Collection.date.range)
four_commaprobs = grep("^\\d\\d\\d\\d-\\d\\d-\\d\\d,\\d\\d\\d\\d-\\d\\d-\\d\\d,\\d\\d\\d\\d-\\d\\d-\\d\\d,\\d\\d\\d\\d-\\d\\d-\\d\\d$", probs$Collection.date.range)

# Make sure that this equals zero- signals that all problematic formats have been captured successfully
nrow(probs) == length(TOprobs) + length(commaprobs) + 
  length(three_TOprobs) + length(three_commaprobs) + length(four_commaprobs)

#### Explore Characteristics of Problematic Dates #####

# Check trap type of each format
table(probs$Collection.protocols[TOprobs])

table(probs$Collection.protocols[commaprobs])

table(probs$Collection.protocols[three_TOprobs])

table(probs$Collection.protocols[three_commaprobs])

table(probs$Collection.protocols[four_commaprobs])

# Check the collection duration days of problematic dates
unique(probs$Collection.duration..days.)

## I want to check how many are single day intervals
# first extract from and to dates from the TO probs
dates = strsplit(probs$Collection.date.range[TOprobs], split = " TO ")
from = sapply(dates, "[[" , 1)
to = sapply(dates, "[[" , 2)

# Create a data frame of start and end dates
dates = data.frame(from, to)

# now find the number of days between between from and to columns by taking the difference
dates$diff = round(difftime(as.POSIXct(dates$to), as.POSIXct(dates$from), units = "days"))
dates = data.frame(dates, probs[TOprobs,])

# Check that these differences match collection duration days
sum(dates$diff != probs$Collection.duration..days.) == 0

# Reduce down to a single observation per trapping event instead of per species
allspecs = 
  dates %>%
  dplyr::select("to", "from", "diff", "Specimens.collected", "Latitudes", "Locations", "Longitudes", "Collection.protocols", "Attractants") %>%
  group_by(Locations, Latitudes, Longitudes, from, to, diff, Collection.protocols, Attractants) %>%
  summarise(Specimens.collected = mean(Specimens.collected)) %>%
  arrange(Latitudes, Longitudes, from, to)

# Check frequencies of different interval lengths
freqs = table(allspecs$diff)
freqs

# Look at locations where these are happening
unique(dates$Locations) # All locations

#### Fixing ####
# This is specific to florida data but should be adjusted according needs at other locations

# Just take the first date
fixed = gsub("\\s.*", "", fddata$Collection.date.range)

# Check that every date is accounted for 
indexes = grep("^\\d\\d\\d\\d-\\d\\d-\\d\\d$", fixed)

# every date now matches the pattern, so sub in this column and save
fddata$Collection.date.range = fixed
write.csv(fddata, "../Data/fdcounts_datescleaned.csv")

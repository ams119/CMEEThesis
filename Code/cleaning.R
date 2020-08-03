#!/bin/env Rscript
# Author: Anne Marie Saunders
# Script: cleaning.R
# Desc: reformat irregular date ranges and exclude data from uncommon trap types.
# Arguments: ~/Documents/Hefty_Data/mosquitoAbun/fdcounts.csv
# Date: 04/17/20

# Import libraries
library(stringr)
library(tidyverse)

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
  dplyr::group_by(Locations, Latitudes, Longitudes, from, to, diff, Collection.protocols, Attractants) %>%
  dplyr::summarise(Specimens.collected = mean(Specimens.collected)) %>%
  dplyr::arrange(Latitudes, Longitudes, from, to)

# Check how many trapping events were attributed to each trap type
table(allspecs$Collection.protocols)

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

# every date now matches the pattern, so sub in this column
fddata$Collection.date.range = fixed

#### Set abundance to NA for all but the most common trap type for each location ####

# First aggregate data to each trapping event (every day at every trap) so we can find the most common trap:
trap_event = 
  fddata %>%
  dplyr::select("Collection.date.range", "Specimens.collected", "Latitudes", "Locations", "Longitudes", "Collection.protocols", "Attractants") %>%
  dplyr::group_by(Collection.date.range, Locations, Latitudes, Longitudes, Collection.protocols, Attractants) %>%
  dplyr::summarise(Specimens.collected = mean(Specimens.collected)) %>%
  dplyr::arrange(Collection.date.range, Locations, Latitudes,)

# Display the types of traps at each location
for(i in 1:length(unique(trap_event$Locations))){
  print(unique(trap_event$Locations)[i])
  print(table(trap_event$Collection.protocols[which(trap_event$Locations == unique(trap_event$Locations)[i])]))
  print("**********************************************************************")
}

# For each location restrict data to just 1 trap type
for(i in 1:length(unique(trap_event$Locations))){
  
  # Make a descending table of trap type frequency
  tbl = sort(table(trap_event$Collection.protocols[trap_event$Locations == unique(trap_event$Locations)[i]]), decreasing = T)
  
  # Find the most common trap by taking the first value
  most_trap = names(tbl)[1]
  
  # Find indexes in fddata where both location is correct but trap is not the most common trap
  remove_these = which(fddata$Locations == unique(trap_event$Locations)[i] & fddata$Collection.protocols != most_trap)
  
  # In fddata, remove rows that do not belong to to the most common trap type at this location
  if(length(remove_these) > 0){
    fddata = fddata[-remove_these,] }
}

# Check frequency of trap events again
trap_event = 
  fddata %>%
  dplyr::select("Collection.date.range", "Specimens.collected", "Latitudes", "Locations", "Longitudes", "Collection.protocols", "Attractants") %>%
  dplyr::group_by(Collection.date.range, Locations, Latitudes, Longitudes, Collection.protocols, Attractants) %>%
  dplyr::summarise(Specimens.collected = mean(Specimens.collected)) %>%
  dplyr::arrange(Collection.date.range, Locations, Latitudes,)

# Display the types of traps at each location
for(i in 1:length(unique(trap_event$Locations))){
  print(unique(trap_event$Locations)[i])
  print(table(trap_event$Collection.protocols[which(trap_event$Locations == unique(trap_event$Locations)[i])]))
  print("**********************************************************************")
}



# Save cleaned fddata file
write.csv(fddata, "../Data/fdcounts_cleaned.csv")

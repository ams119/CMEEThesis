#!/bin/env Rscript
# Author: Anne Marie Saunders
# Script: fix_dates.R
# Desc: fix annoying date entries
# Arguments: ~/Documents/Hefty_Data/mosquitoAbun/fdcounts.csv
# Date: 04/17/20

library(stringr)

fddata = read.csv("~/Documents/Hefty_Data/dates_weird.csv", stringsAsFactors = F)
fddata = fddata[which(fddata$Locations != "Riverside"),]

class(fddata$Collection.date.range)

# Look at each location and see how many trap types are used at each

table(fddata$Collection.protocols[which(fddata$Locations == "Story")])

table(fddata$Collection.protocols[which(fddata$Locations == "Polk (Iowa (United States))")])

table(fddata$Collection.protocols[which(fddata$Locations == "Woodbury")])


# find the rows that are formatted correctly
indexes = grep("^\\d\\d\\d\\d-\\d\\d-\\d\\d$", fddata$Collection.date.range)

probs = fddata[-indexes,]
uprobs = unique(probs$Collection.date.range)
uprobs

# In some datasets, all issues are being caused by dates in the form:
# dddd-dd-dd TO dddd-dd-dd
#Check What's Going On With These:
TOprobs = grep("^\\d\\d\\d\\d-\\d\\d-\\d\\d TO \\d\\d\\d\\d-\\d\\d-\\d\\d$", probs$Collection.date.range)
commaprobs = grep("^\\d\\d\\d\\d-\\d\\d-\\d\\d,\\d\\d\\d\\d-\\d\\d-\\d\\d$", probs$Collection.date.range)
three_TOprobs = grep("^\\d\\d\\d\\d-\\d\\d-\\d\\d,\\d\\d\\d\\d-\\d\\d-\\d\\d TO \\d\\d\\d\\d-\\d\\d-\\d\\d$", probs$Collection.date.range)
three_commaprobs = grep("^\\d\\d\\d\\d-\\d\\d-\\d\\d,\\d\\d\\d\\d-\\d\\d-\\d\\d,\\d\\d\\d\\d-\\d\\d-\\d\\d$", probs$Collection.date.range)

nrow(probs) - length(TOprobs) - length(commaprobs) - length(three_TOprobs) - length(three_commaprobs)

four_commaprobs = grep("^\\d\\d\\d\\d-\\d\\d-\\d\\d,\\d\\d\\d\\d-\\d\\d-\\d\\d,\\d\\d\\d\\d-\\d\\d-\\d\\d,\\d\\d\\d\\d-\\d\\d-\\d\\d$", probs$Collection.date.range)

nrow(probs) - length(TOprobs) - length(commaprobs) - 
  length(three_TOprobs) - length(three_commaprobs) - length(four_commaprobs)

# Check trap type of each format
table(probs$Collection.protocols[TOprobs])

table(probs$Collection.protocols[commaprobs])

table(probs$Collection.protocols[three_TOprobs])

table(probs$Collection.protocols[three_commaprobs])

table(probs$Collection.protocols[four_commaprobs])


## I want to check how many are single day intervals
# first extract from and to dates from the TO probs
dates = strsplit(probs$Collection.date.range[TOprobs], split = " TO ")
from = sapply(dates, "[[" , 1)
to = sapply(dates, "[[" , 2)
dates = data.frame(from, to)

# now find the differences between from and to columns
dates$diff = round(difftime(as.POSIXct(dates$to), as.POSIXct(dates$from), units = "days"))
dates = data.frame(dates, probs[TOprobs,])

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

#Check out the really extreme example: interval of 60 days
sixty = allspecs[which(allspecs$diff == 60),]
# New jersey trap catch in Riverside, CA seemingly left out during wintertime







#### Fixing
# Just take the first date
fixed = gsub("\\s.*", "", fddata$Collection.date.range)

# Check that every date is accounted for 
indexes = grep("^\\d\\d\\d\\d-\\d\\d-\\d\\d$", fixed)

# every date now matches the pattern, so sub in this column and save
fddata$Collection.date.range = fixed
write.csv(fddata, "~/Documents/Hefty_Data/mosquitoAbun/cacounts2.csv")

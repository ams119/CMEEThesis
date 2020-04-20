#!/bin/env Rscript
# Author: Anne Marie Saunders
# Script: fix_dates.R
# Desc: fix annoying date entries
# Arguments: ~/Documents/Hefty_Data/mosquitoAbun/fdcounts.csv
# Date: 04/17/20

fddata = read.csv("~/Documents/Hefty_Data/mosquitoAbun/fdcounts.csv", stringsAsFactors = F)

class(fddata$Collection.date.range)
indexes = grep("^\\d\\d\\d\\d-\\d\\d-\\d\\d$", fddata$Collection.date.range)

probs = fddata[-indexes,]
uprobs = unique(probs$Collection.date.range)

# In fddata, all issues are being caused by dates in the form:
# dddd-dd-dd TO dddd-dd-dd

# Just take the first date
fixed = gsub("\\s.*", "", fddata$Collection.date.range)

# Check that every date is accounted for 
indexes = grep("^\\d\\d\\d\\d-\\d\\d-\\d\\d$", fixed)

# every date now matches the pattern, so sub in this column and save
fddata$Collection.date.range = fixed
write.csv(fddata, "~/Documents/Hefty_Data/mosquitoAbun/fdcounts2.csv")

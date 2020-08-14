#!/bin/env Rscript
# Author: Anne Marie Saunders
# Script: aggregation.R
# Desc: Transforming climate/abun data from daily scale to weekly/bimonthly/monthly scale. Can be used on its own or in conjuction with a batch running script
# Arguments: 
# Date: 04/23/20

library(tidyverse)

# Commented out reading in data because this is run with aggregation_batch.R as a wrapper defining the mapped variable
#mapped = read.csv("~/Documents/Masters_Thesis/Data/Extracted_Data/Saint_Johns_clim_TS.csv", header = T, stringsAsFactors = F)

# Sort the data
mapped = dplyr::arrange(mapped, date, Latitudes, Longitudes)

# Extract the location name, without special characters
locale = as.character(na.omit(mapped$Locations)[1])
locale = sub(" \\(.*", "", locale) # remove special characters
locale = sub("\\s", "_", locale) # Convert any remaining spaces to underscore

# Convert date from character to date format
mapped$date = as.Date(mapped$date)

# NA in Year column indicates this is interpolated meteorological data
# Use Year column to indicate whether this row is 'real' or interpolated
mapped$real = !is.na(mapped$Year)

# Create a new data frame which aggregates all species to a single count per trap
allspecs = 
  mapped %>%
  dplyr::select("date", "Specimens.collected", "Latitudes", "Longitudes", "Year", "precip", "max_temp", "real") %>%
  dplyr::group_by(Latitudes, Longitudes, date, Year, precip, max_temp) %>%
  dplyr::summarise(Specimens.collected = sum(Specimens.collected, na.rm = T), real = mean(real, na.rm = T)) %>%
  dplyr::arrange(date, Latitudes, Longitudes)


# Create a data frame with each species as column instead of in rows. 
### Add species specific columns to trap counts ###

# Convert all species name spaces to periods:
mapped$Species = gsub(" ", ".", mapped$Species)

# Also convert any weird symbols to periods:
mapped$Species = gsub("\\W", ".", mapped$Species)

# Create an alphebetical list of unique species names
species = sort(unique(mapped$Species[which(!is.na(mapped$Species))]))

## Remove species that are actually genus or family groups ##
# First find indexes of these groups #
non_species = c("genus", "Culicidae", "Culicinae")
non_species = grep(paste(non_species, collapse = "|"), species)

# Now only keep actual species in species list
if(length(non_species) > 0){species = species[-non_species]}

# Create a vector to store species indices to remove from species list
species2rm = vector()

# give each species its own column
for(j in 1:length(species)){
  
  # Get all the rows with counts of those species
  spec_counts = mapped[mapped$Species == species[j],]
  
  # Summarise the mean samples of this species from each day
  spec_counts = 
    spec_counts %>% 
    group_by(Latitudes, Longitudes, date) %>% 
    dplyr::select("date", "Latitudes", "Longitudes", "Specimens.collected") %>% 
    summarise(test = mean(Specimens.collected, na.rm = T))
  
  # As long as there is at least 1 non-NA, non-zero recording, assign species name to time series and join
  if(sum(spec_counts$test, na.rm = T) != 0){
    
    # Change column names so that count column identifies the species
    names(spec_counts)[names(spec_counts) == 'test'] = species[j]
  
    # join to the daily data frame by date only if there is at least 1 non-zero count of the data
    allspecs = left_join(allspecs, spec_counts, by = c("date", "Latitudes", "Longitudes"))}
  else{
    species2rm = c(species2rm, j)}
  
}

# Remove zero count species
if(length(species2rm) > 0){
  species = species[-species2rm]}

########### Merge species morphological groups ################

# find columns of species within each morphological group
# culex pipiens
cp_spec = c("Culex.salinarius", "Culex.quinquefasciatus", "Culex.pipiens", "Culex.restuans", "Culex.australicus")
cp_morphs = grep(paste(cp_spec, collapse = "|"), colnames(allspecs))

#Aedes atlanticus-tormentor
aat_spec = c("Aedes.atlanticus", "Aedes.tormentor")
aat_morphs = grep(paste(aat_spec, collapse = "|"), colnames(allspecs))

# Anopheles messeae/daciae 
amd_spec = c("Anopheles.messeae", "Anopheles.daciae")
amd_morphs = grep(paste(amd_spec, collapse = "|"), colnames(allspecs))


# If these species are present, create a morphological group column that is the mean of these groups and add name to species list
if(length(cp_morphs > 0)){
  allspecs$C.pipiens.morphological.group = rowMeans(allspecs[,cp_morphs], na.rm = T)
  species = c(species, "C.pipiens.morphological.group")
}


if(length(aat_morphs > 0)){
  allspecs$A.atlanticus.tormentor.morphological.group = rowMeans(allspecs[,aat_morphs], na.rm = T)
  species = c(species, "A.atlanticus.tormentor.morphological.group")
}

if(length(amd_morphs > 0)){
  allspecs$A.messeae.daciae.morphological.group = rowMeans(allspecs[,amd_morphs], na.rm = T)  
  species = c(species, "A.messeae.daciae.morphological.group")
}

# If any morphs were present, remove the species that have been averaged to a morphological group
if(length(amd_morphs) + length(aat_morphs) + length(cp_morphs) > 0 ){
  # Add the name of the species to the morphological groups list
  morphological_aggregations$Species[i] = str_c(colnames(allspecs)[c(amd_morphs, aat_morphs, cp_morphs)], collapse = ", ")
  
  # Now remove the columns from the dataframe
  allspecs = allspecs[, -c(amd_morphs, aat_morphs, cp_morphs)]

  # Only keep species that still have a column in the dataframe in the species list
  species = species[which(species %in% colnames(allspecs))]
}


###################
# Aggregate multiple traps per day to just 1 daily value with mean counts, temp, and precip
###################

# Create Year for every row
allspecs$Year = as.numeric(format(as.Date(allspecs$date, format="%Y-%U-%u"),"%Y"))

# Summarize the non species specific columns
daily = 
  allspecs %>%
  group_by(date) %>%
  dplyr::select("date", "Year", "Specimens.collected", "precip", "max_temp", "real") %>%
  summarise(Year = mean(Year), precip = mean(precip), 
              Specimens.collected = mean(Specimens.collected, na.rm = T), temp_mean = mean(max_temp), 
              obs = sum(real, na.rm = T)) %>%
  arrange(date)
  
# Create a column that marks whether there was (1) or was not (NaN) precip on each day
daily$precip_days = as.numeric(daily$precip/daily$precip)

# Aggregate the species columns
daily_species = 
  allspecs %>%
  group_by(date) %>%
  dplyr::select(c(date, 9:ncol(allspecs))) %>%
  summarize_all(mean, na.rm = T) 
  
daily = left_join(daily, daily_species, by = "date")

#####################
# Weekly Aggregation
#####################

# Create week numbering
#sepspecs$date_w = format(as.Date(sepspecs$date), "%Y-%U")
daily$date_w = format(as.Date(daily$date), "%Y-%U")

## Aggregate by week ##

# Mean and add together the climate columns and the total abundance column
weekly <-
  daily %>% 
  group_by(date_w) %>%
  dplyr::select("date_w", "Year", "temp_mean", "precip", "Specimens.collected", "precip_days", "obs") %>%
  summarise(Year = mean(Year), precip_mean = mean(precip), precip_sd = sd(precip), 
            Specimens.collected = mean(Specimens.collected, na.rm = T), temp_sd = sd(temp_mean, na.rm = T),
            temp_mean = mean(temp_mean), precip_days = sum(precip_days, na.rm = T), obs = sum(obs))
  
# Add the means of each species to the weekly frame
weekly_species = 
  daily %>%
  group_by(date_w) %>%
  dplyr::select(date_w, 8:(ncol(daily)-1)) %>% # select all the species columns and the date column
  summarise_all(mean, na.rm = T)
  
weekly = left_join(weekly, weekly_species, by = "date_w")
    
# Find the coefficient of variation of temp and precip
weekly$precip_cv = weekly$precip_sd/weekly$precip_mean
weekly$temp_cv = weekly$temp_sd/weekly$temp_mean

# Set abun of no count weeks to NA
#weekly$Specimens.collected = replace(weekly$Specimens.collected, weekly$obs == 0, NA)

# Convert character date column to date format - must create an arbitrary "day of week"
weekly$date_dw = paste0(weekly$date_w, "-1")
weekly$date_dw = as.Date(weekly$date_dw, format = "%Y-%U-%u")

# Visualize
# plot_w = ggplot(weekly, aes(x=date_dw, y = `Specimens.collected`/1000)) +
#   geom_line(col = "darkgreen") + xlab("") + 
#   scale_x_date(date_labels = "%Y") +
#   ggtitle(paste0(locale, " Weekly")) +
#   theme_bw() + labs(y = "Mosquitoes Collected (Thousands)",
#                     x = "Time")

# Save csv of weekly aggregated data
#filename = paste()
#write.csv(weekly, filename, row.names = F)

##################################
# Create biweekly aggregation
##################################

# Create a column to store bimonth number:
daily$bw_num = rep(NA, nrow(daily))

# Create month date column in daily data
daily$month = as.numeric(format(as.Date(daily$date), "%m"))

# Create day column in daily data
daily$day = as.numeric(format(as.Date(daily$date), "%d"))

# Assign bw_num based on dates- Months with 31 will have extra day (15 days + 1) randomly assigned
# February will be dealt with independently

# pick 0 or 1 randomly to use to assign extra day in long months
random = sample(c(0,1), size = 1)

# If the month has 31 days:
# Find the first half of the month, perhaps plus an extra day
months_firsthalf = which(daily$month %in% c(1, 3, 5, 7, 8, 10, 12) &
                      daily$day %in% 1:(15 + random))
  
# Assign the biweek number as month number x 2, -1 (example- early jan would be month (1) x 2 = 2, - 1 = 1)
daily$bw_num[months_firsthalf] = daily$month[months_firsthalf] * 2 - 1

# Find the second half of the month, perhaps minus an extra day
months_secondhalf = which(daily$month %in% c(1, 3, 5, 7, 8, 10, 12) &
                                daily$day %in% (16 + random):31)

# Assign the biweek number as month number times 2
daily$bw_num[months_secondhalf] = daily$month[months_secondhalf] * 2

# Do the same but for 30-day months: find the first half of the month:
months_firsthalf = which(daily$month %in% c(4, 6, 9, 11) &
                           daily$day %in% 1:15)

# Assign the biweek number as month number times 2 minus 1
daily$bw_num[months_firsthalf] = daily$month[months_firsthalf] * 2 - 1

# Find the second half of the month
months_secondhalf = which(daily$month %in% c(4, 6, 9, 11) &
                           daily$day %in% 16:30)

# Assign the biweek number as month number times 2
daily$bw_num[months_secondhalf] = daily$month[months_secondhalf] * 2
  
# Now finally deal with february: If it's a leap year just add that to the second half
months_firsthalf = which(daily$month == 2 &
                           daily$day %in% 1:14)

# Assign the biweek number as month number times 2 minus 1
daily$bw_num[months_firsthalf] = daily$month[months_firsthalf] * 2 - 1

# Find the second half of the month
months_secondhalf = which(daily$month == 2 &
                            daily$day %in% 15:29)

# Assign the biweek number as month number times 2
daily$bw_num[months_secondhalf] = daily$month[months_secondhalf] * 2

# Create a column to populate with bi-week number
#weekly$bw_num = rep(NA, nrow(weekly))

# Find rows with NA dates- hopefully using date_w instead of dat_dw will fix this
# trash = which(is.na(weekly$date_dw))
# 
# if(length(trash) > 0) # Examine to ensure it's irrelevant
# { 
#   weekly[trash,]
# 
#   # Get rid of this row
#   weekly = weekly[-trash,]
# }  

# Obtain vector of unique years for later biweek plotting
yrs = unique(weekly$Year)

## Assign a bi-week number, restarting at 1 with every year
# for(j in 1:length(yrs)){
#   sub = which(weekly$Year == yrs[j],)
#   nums = 1:length(sub)
#   bi_week = ((nums - 1) %/% 2) + 1
#   weekly$bw_num[sub] = bi_week
# }

# Assign bi-week number to sepspecs by joining through week date
# biweekly =
#   weekly %>% 
#   dplyr::select(date_w, bw_num, years) %>%
#   left_join(sepspecs, by = c("date_w"))

# Aggregate by bi-week
biweekly =
  daily %>% 
  group_by(bw_num, Year) %>%
  dplyr::select("bw_num", "Year", "temp_mean", "precip", "Specimens.collected", "precip_days", "obs") %>%
  summarise(precip_sd = sd(precip), precip_mean = mean(precip), 
            temp_sd = sd(temp_mean), temp_mean = mean(temp_mean), precip_days = sum(precip_days, na.rm = T),
            obs = sum(obs, na.rm = T), Specimens.collected = mean(Specimens.collected, na.rm = T), ) %>%
  arrange(Year, bw_num)

# Aggregate counts to week and join
biweek_species = 
  daily %>%
  group_by(bw_num, Year) %>%
  dplyr::select(c(bw_num, Year, 8:(ncol(daily)-4))) %>%
  summarise_all(mean, na.rm = T)

biweekly = left_join(biweekly, biweek_species, by = c("bw_num", "Year"))  

biweekly = 
  biweekly %>% 
  arrange(Year, bw_num)
  
# Assign row numbers
biweekly$ids = seq(1:nrow(biweekly))

# Set abun of no count weeks to NA
#biweekly$Specimens.collected = replace(biweekly$Specimens.collected, biweekly$obs == 0, NA)

# Find the coefficient of variation of temp and precip
biweekly$precip_cv = biweekly$precip_sd/biweekly$precip_mean
biweekly$temp_cv = biweekly$temp_sd/biweekly$temp_mean

# Visualize

# Create manual tick marks and axis labels
# brks = seq(0, 27 * length(yrs), by = 27)
# next_year = as.character(as.numeric(yrs[length(yrs)]) + 1)
# 
# plot_bw = ggplot(biweekly, aes(x=ids, y = `Specimens.collected`/1000)) +
#   geom_line(col = "blue") + xlab("") + 
#   scale_x_continuous("Time", breaks = brks, labels = c(yrs, next_year)) +
#   ggtitle(paste0(locale, " Biweekly")) +
#   theme_bw() + labs(y = "Mosquitoes Collected (thousands)")

# Save csv of weekly aggregated data
#filename = paste()
#write.csv(weekly, filename, row.names = F)

######################
# Monthly Aggregation
######################

# Create month numbering
allspecs$date_m <- format(as.Date(allspecs$date), "%Y-%m")
daily$date_m = format(as.Date(daily$date), "%Y-%m")

# Aggregate by month
monthly <-
  daily %>% 
  group_by(date_m) %>%
  dplyr::select("date_m", "Year", "temp_mean", "precip", "Specimens.collected", "precip_days", "obs") %>%
  summarise(Year = mean(Year), precip_mean = mean(precip), precip_sd = sd(precip), temp_sd = sd(temp_mean, na.rm = T),
            temp_mean = mean(temp_mean), precip_days = sum(precip_days, na.rm = T), obs = sum(obs), 
            Specimens.collected = mean(Specimens.collected, na.rm = T))

# Aggregate counts to week and join
monthly_species = 
  daily %>%
  group_by(date_m) %>%
  dplyr::select(date_m, 8:(ncol(daily)-5)) %>%
  summarise_all(mean, na.rm = T)

monthly = left_join(monthly, monthly_species, by = "date_m")

# Find the coefficient of variation of temp and precip
monthly$precip_cv = monthly$precip_sd/monthly$precip_mean
monthly$temp_cv = monthly$temp_sd/monthly$temp_mean

# Convert character date column to date format. Must arbitrarily add day of the month to make 
# date formatting work
monthly$date_dm = paste0(monthly$date_m, "-01")
monthly$date_dm = as.Date(monthly$date_dm, format = "%Y-%m-%d")

# Visualize

# plot_m = ggplot(monthly, aes(x=date_dm, y = `Specimens.collected`/1000)) +
#   geom_line(col = "purple") + xlab("") + 
#   scale_x_date(date_labels = "%Y") +
#   ggtitle(paste0(locale, " Monthly")) +
#   theme_bw() + labs(y = "Mosquitoes Collected (thousands)",
#                    x = "Time")

# Save csv of monthly aggregated data
#filename = paste()
#write.csv(weekly, filename, row.names = F)


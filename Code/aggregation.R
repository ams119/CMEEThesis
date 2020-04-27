#!/bin/env Rscript
# Author: Anne Marie Saunders, adapted from Matthew J Watts 
# Script: aggregation.R
# Desc: Transforming climate/abun data from daily scale to weekly/bimonthly/monthly scale
# Arguments: 
# Date: 04/23/20

library(tidyverse)

#mapped = read.csv("~/Documents/Hefty_Data/Extracted_Data/Manatee_clim_TS.csv", header = T, stringsAsFactors = F)
locale = as.character(na.omit(mapped$Locations)[1])

# Convert date from character to date format
mapped$date = as.Date(mapped$date)

ggplot(mapped, aes(x=date, y=`Specimens.collected`)) +
  geom_line(col = "blue") + xlab("") + 
  scale_x_date(date_labels = "%Y") +
  ggtitle("Daily") + 
  theme_bw()

# Convert NA values to 0- this is really only valid for vect abund NAs, not NAs of interpolated temp data (76,695 entries)
# add a zero to each NA
mapped$Specimens.collected[is.na(mapped$Specimens.collected)] = 0
# True zero is where Year or Location does not equal NA

# Create a new data frame where different species counts are just 1 row
allspecs = 
  mapped %>%
  dplyr::select("date", "Specimens.collected", "Latitudes", "Longitudes", "Year", "precip", "max_temp") %>%
  group_by(Latitudes, Longitudes, date, Year, precip, max_temp) %>%
  summarise(Specimens.collected = sum(Specimens.collected)) %>%
  arrange(date)

# Create a column that differentiates trap count dates from interpolated climate entries
allspecs$real = replace(allspecs$Year, !is.na(allspecs$Year), 1)

###################
# Aggregate multiple traps per day to just 1 daily value to get precip days and # obs
###################

daily = 
  allspecs %>%
  group_by(date) %>%
  dplyr::select("date", "Specimens.collected", "precip", "max_temp", "real") %>%
  summarise(precip = sum(precip), 
              Specimens.collected = sum(Specimens.collected), temp_mean = mean(max_temp), 
              obs = sum(real, na.rm = T)) %>%
  arrange(date)
  
# Create a column that marks whether there was (1) or was not (NaN) precip on each day
daily$precip_days = daily$precip/daily$precip

# Keep only count columns
daily = data.frame(daily$date, daily$precip_days, daily$obs)
colnames(daily) = c("date", "precip_days", "obs")

#####################
# Weekly Aggregation
#####################

# Create week numbering
allspecs$date_w = format(as.Date(allspecs$date), "%Y-%U")
daily$date_w = format(as.Date(daily$date), "%Y-%U")

# Aggregate by week
weekly <-
  allspecs %>% 
  group_by(date_w) %>%
  dplyr::select("date_w", "max_temp", "precip", "Specimens.collected") %>%
  summarise(precip_mean = mean(precip), precip_sd = sd(precip), 
            Specimens.collected = sum(Specimens.collected), temp_mean = mean(max_temp), 
            temp_sd = sd(max_temp))

# Aggregate counts to week and join
counts = 
  daily %>%
  group_by(date_w) %>%
  dplyr::select("date_w", "precip_days", "obs") %>%
  summarise(precip_days= sum(precip_days, na.rm = T), obs = sum(obs, na.rm = T))

weekly = left_join(weekly, counts, by = "date_w")

# Find the coefficient of variation of temp and precip
weekly$precip_cv = weekly$precip_sd/weekly$precip_mean
weekly$temp_cv = weekly$temp_sd/weekly$temp_mean

# Set abun of no count weeks to NA
weekly$Specimens.collected = replace(weekly$Specimens.collected, weekly$obs == 0, NA)

# Convert character date column to date format - must create an arbitrary "day of week"
weekly$date_dw = paste0(weekly$date_w, "-1")
weekly$date_dw = as.Date(weekly$date_dw, format = "%Y-%U-%u")

# Visualize
plot_w = ggplot(weekly, aes(x=date_dw, y = `Specimens.collected`/1000)) +
  geom_line(col = "darkgreen") + xlab("") + 
  scale_x_date(date_labels = "%Y") +
  ggtitle(paste0(locale, " Weekly")) +
  theme_bw() + labs(y = "Mosquitoes Collected (Thousands)",
                    x = "Time")

# ggplot(weekly, aes(x=date_dw, y=`Specimens.collected`)) +
#   geom_line(col = "blue") + xlab("") + 
#   scale_x_date(date_labels = "%Y") +
#   ggtitle("Weekly") + 
#   theme_bw()

# Save csv of weekly aggregated data
#filename = paste()
#write.csv(weekly, filename, row.names = F)

##################################
# Create biweekly aggregation
##################################

# Create a column to populate with bi-week number
weekly$bw_num = rep(NA, nrow(weekly))

# Find rows with NA dates
trash = which(is.na(weekly$date_dw))

if(length(trash) > 0) # Examine to ensure it's irrelevant
{ 
  weekly[trash,]

  # Get rid of this row
  weekly = weekly[-trash,]
}  
# obtain a column of the years of data
weekly$years = format(as.Date(weekly$date_dw, format="%Y-%U-%u"),"%Y")

# Obtain vector of unique years
yrs = unique(weekly$years)

## Assign a bi-week number, restarting at 1 with every year
for(j in 1:length(yrs)){
  sub = which(weekly$years == yrs[j],)
  nums = 1:length(sub)
  bi_week = ((nums - 1) %/% 2) + 1
  weekly$bw_num[sub] = bi_week
}

# Assign bi-week number to allspecs by joining through week date
biweekly =
  weekly %>% 
  dplyr::select(date_w, bw_num, years) %>%
  left_join(allspecs, by = c("date_w"))

# Aggregate by bi-week
biweekly =
  biweekly %>% 
  group_by(bw_num, years) %>%
  dplyr::select("bw_num", "years", "max_temp", "precip", "Specimens.collected") %>%
  summarise(precip_mean = mean(precip), precip_sd = sd(precip),
            Specimens.collected = sum(Specimens.collected), temp_mean = mean(max_temp), 
            temp_sd = sd(max_temp)) %>%
  arrange(years, bw_num)

# Aggregate counts to week and join
biweek_counts = 
  weekly %>%
  group_by(bw_num, years) %>%
  dplyr::select("bw_num", "precip_days", "obs", "years") %>%
  summarise(precip_days= sum(precip_days, na.rm = T), obs = sum(obs, na.rm = T))
  #left_join(biweekly, by = c("bw_num", "years"))

biweekly = merge(biweekly, biweek_counts, by = c("bw_num", "years"))  

biweekly = 
  biweekly %>% 
  arrange(years, bw_num)
  
# Assign row numbers
biweekly$ids = seq(1:nrow(biweekly))

# Set abun of no count weeks to NA
biweekly$Specimens.collected = replace(biweekly$Specimens.collected, biweekly$obs == 0, NA)

# Find the coefficient of variation of temp and precip
biweekly$precip_cv = biweekly$precip_sd/biweekly$precip_mean
biweekly$temp_cv = biweekly$temp_sd/biweekly$temp_mean

# Visualize
#ggplot(biweekly, aes(x=ids, y=`Specimens.collected`)) +
  # geom_line(col = "blue") + xlab("") + 
  # #scale_x_date(date_labels = "%Y") +
  # ggtitle("Bi-weekly") + 
  # theme_bw()

plot_bw = ggplot(biweekly, aes(x=ids, y = `Specimens.collected`/1000)) +
  geom_line(col = "blue") + xlab("") + 
  #scale_x_date(date_labels = "%Y") +
  ggtitle(paste0(locale, " Biweekly")) +
  theme_bw() + labs(y = "Mosquitoes Collected (thousands)",
                    x = "Time")

plot_bw
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
  allspecs %>% 
  group_by(date_m) %>%
  dplyr::select("date_m", "max_temp", "precip", "Specimens.collected") %>%
  summarise(precip_mean = mean(precip), precip_sd = sd(precip),
            Specimens.collected = sum(Specimens.collected), temp_mean = mean(max_temp), 
            temp_sd = sd(max_temp))

# Aggregate counts to week and join
monthly = 
  daily %>%
  group_by(date_m) %>%
  dplyr::select("date_m", "precip_days", "obs") %>%
  summarise(precip_days= sum(precip_days, na.rm = T), obs = sum(obs, na.rm = T)) %>%
  left_join(monthly, by = c("date_m")) %>%

# Find the coefficient of variation of temp and precip
monthly$precip_cv = monthly$precip_sd/monthly$precip_mean
monthly$temp_cv = monthly$temp_sd/monthly$temp_mean

# Set abun of no count weeks to NA
monthly$Specimens.collected = replace(monthly$Specimens.collected, monthly$obs == 0, NA)

# Convert character date column to date format. Must arbitrarily add day of the month to make 
# date formatting work
monthly$date_dm = paste0(monthly$date_m, "-01")
monthly$date_dm = as.Date(monthly$date_dm, format = "%Y-%m-%d")

# Visualize

plot_m = ggplot(monthly, aes(x=date_dm, y = `Specimens.collected`/1000)) +
  geom_line(col = "purple") + xlab("") + 
  scale_x_date(date_labels = "%Y") +
  ggtitle(paste0(locale, " Monthly")) +
  theme_bw() + labs(y = "Mosquitoes Collected (thousands)",
                   x = "Time")
plot_m
# https://stackoverflow.com/questions/17148679/construct-a-manual-legend-for-a-complicated-plot
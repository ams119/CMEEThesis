#!/bin/env Rscript
# Author: Anne Marie Saunders
# Script: graphics.R
# Desc: Creating graphics for lab proposal presentation
# Arguments: 
# Date: 04/30/20

library(maps)
library(tmap)
library(tmaptools)
library(sf)
library(ggplot2)

setwd("~/Documents/CMEEThesis/Proposal_Presentation/")

counties = st_as_sf(maps::map("county", "florida", fill = TRUE, plot = FALSE))

# extract the county names
ctynames = sub("florida,", "", counties$ID)

# convert desires counties to colors
indx = which(ctynames %in% c("collier", "hernando", "lee", "manatee", "orange", "st johns", "walton"))
ctynames[indx] = "steelblue4"
ctynames[-indx] = "grey92"

counties = cbind(counties, ctynames)

pdf("floridamap.pdf", height = 4, width = 4)
ggplot(data = counties) + geom_sf(color = "grey36", fill = counties$ctynames) + theme_bw() + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), panel.border = element_blank())
dev.off()

library(stringr)

wnv = read.csv("CDC_WNV.csv", header = T, stringsAsFactors = F)
str(wnv)

# Convert deaths into desirable format
#wnv$Deaths = str_extract(wnv$Deaths, "^.\\d*")
#wnv$Deaths = as.numeric(str_extract(wnv$Deaths, "\\d+"))

pdf("wnv_cases.pdf", width = 5, height = 4)
par(cex.sub = 0.8)
plot(wnv$cases~wnv$year, type = "l", col = "steelblue4", lwd = 2, xlim = c(1998, 2020),
     main = "West Nile Virus Cases in the USA",
     ylab = "Cases",
     xlab = "Year",)
title(sub = "From www.cdc.gov", adj = 1)
dev.off()

## create abundance plots for presentation
mapped = read.csv("~/Documents/Hefty_Data/Extracted_Data/Lee_clim_TS.csv", header = T, stringsAsFactors = F)
source("aggregation_allspecs.R")

pdf("weekly_agg.pdf", height = 4, width = 5)
ggplot(weekly, aes(x=date_dw, y = `Specimens.collected`)) +
  geom_line(col = "steelblue4") + xlab("") +
  scale_x_date(date_labels = "%Y") +
  ggtitle(paste0("Weekly Specimens Collected: Lee County, Florida")) +
  theme_bw() + labs(y = "Mosquitoes Collected (Thousands)",
                    x = "Time")
dev.off()

pdf("biweekly_agg.pdf", height = 4, width = 5)
ggplot(biweekly, aes(x=ids, y = `Specimens.collected`)) +
  geom_line(col = "steelblue4") + xlab("") +
  scale_x_continuous("Time", breaks = brks, labels = c(yrs, next_year)) +
  ggtitle(paste0( "Biweekly Specimens Collected: Lee County, Florida")) +
  theme_bw() + labs(y = "Mosquitoes Collected (thousands)")
dev.off()

pdf("monthly_agg.pdf", height = 4, width = 5)
ggplot(monthly, aes(x=date_dm, y = `Specimens.collected`)) +
  geom_line(col = "steelblue4") + xlab("") +
  scale_x_date(date_labels = "%Y") +
  ggtitle(paste0("Monthly Specimens Collected: Lee County, Florida")) +
  theme_bw() + labs(y = "Mosquitoes Collected (thousands)",
                    x = "Time")
dev.off()


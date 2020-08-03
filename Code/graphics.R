#!/bin/env Rscript
# Author: Anne Marie Saunders
# Script: graphics.R
# Desc: Create graphics for inclusion in writeup
# Arguments: 
# Date: 07/31/2020

## Import functions and libraries
source("gam_functions.R")
library('gratia')
library("ggplot2")
library("wesanderson")
library("tidyverse")
library("gridExtra")
#library("extrafont")


#### Create disease plot for introduction ####
diseases = read.csv("../Data/local_arbovirus_infections.csv", header = T, stringsAsFactors = F)

pdf("../Images/arboviralcases.pdf", height = 4, width = 6)
#loadfonts(device = "postscript")
#par(family = "LM Roman 10")
ggplot(diseases[which(diseases$Disease != "Zika"),], aes(x = Year, y = Count, color = Disease)) + 
  geom_line(lwd = 1, alpha = 0.5) + theme_bw() + geom_point() +
  ggtitle("Human Arboviral Cases in Florida, 2014-2019") + 
  scale_color_manual(values=wes_palette(n=5, name="Darjeeling1")) + ylab("Cases") + 
  theme(legend.position = c(0.2, 0.75), legend.title = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dev.off()

#### Create sample time series plots of temperature, precipitation, and abundance
data = read.csv("../Data/Extracted_Data/Aggregated/Manatee_weekly.csv", header = T, stringsAsFactors = F)

# Keep only biggest VBD threat mosquitoes
threats = c("Aedes.aegypti", "Aedes.albopictus", "Culex.nigripalpus", "Anopheles.quadrimaculatus")
data = data[,colnames(data) %in% c(threats, "temp_mean", "precip_days", "date_dw")]

# Gather this into long shape
data = data %>% gather(Species, Value, -c(date_dw, precip_days, temp_mean))

# Convert date to date format
data$date_dw = paste0(weekly$date_w, "-1")
data$date_dw = as.Date(data$date_dw, format = "%Y-%m-%d")

# Plot
title <- expression(paste(italic("Aedes albopictus"), " Abundance in Manatee County"))
aedes_abun_plot = ggplot(data[data$Species == "Aedes.albopictus",], aes(x=date_dw, y = Value)) +
  geom_line(color = "black") + xlab("") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  ggtitle(title) +
  theme_bw() + labs(y = "Abundance (average count per trap)", x = "Time", cex = 0.9) + 
  theme(legend.position = c(0.2, 0.75), 
          legend.title = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
          axis.title.x = element_text(size = 11), axis.title.y = element_text(size = 11),
          plot.title = element_text(size = 12))
pdf("../Images/aedes_abun_ts.pdf", height = 4, width = 4.5)
aedes_abun_plot
dev.off()

title <- expression(paste(italic("Culex nigripalpus"), " Abundance in Manatee County"))
culex_abun_plot = ggplot(data[data$Species == "Culex.nigripalpus",], aes(x=date_dw, y = Value)) +
  geom_line(color = "black") + xlab("") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  ggtitle(title) +
  theme_bw() + labs(y = "Abundance (average count per trap)", x = "Time", cex = 0.9) + 
  theme(legend.position = c(0.2, 0.75), 
        legend.title = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 11), axis.title.y = element_text(size = 11),
        plot.title = element_text(size = 12))
pdf("../Images/culex_abun_ts.pdf", height = 4, width = 4.5)
culex_abun_plot
dev.off()

temp_plot = ggplot(data[data$Species == "Aedes.albopictus",], aes(x=date_dw, y = temp_mean)) +
  geom_line(color = "black") + xlab("") + scale_x_date(date_labels = "%Y", date_breaks = "1 year") + 
  ggtitle("Average Maximum Temperature in Manatee County") +
  theme_bw() + labs(y = "Temperature (°C)", x = "Time", cex = 0.9) + 
  theme(legend.position = c(0.2, 0.75), 
        legend.title = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 11), axis.title.y = element_text(size = 11),
        plot.title = element_text(size = 12))
pdf("../Images/temp_ts.pdf", height = 4, width = 4.5)
temp_plot
dev.off()


precip_plot = ggplot(data[data$Species == "Aedes.albopictus",], aes(x=date_dw, y = precip_days)) +
  geom_line(color = "black") + xlab("") + scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  ggtitle("Precipitation in Lee County") + scale_y_continuous(breaks = 0:7) +
  theme_bw() + labs(y = "Days of Rainfall per Week", x = "Time", cex = 0.9) + 
  theme(legend.position = c(0.2, 0.75), 
        legend.title = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 11), axis.title.y = element_text(size = 11),
        plot.title = element_text(size = 12))
pdf("../Images/precip_ts.pdf", height = 4, width = 4.5)
precip_plot
dev.off()

# pdf("../Images/ts_plots.pdf", height = 6, width = 7)
# grid.arrange(abun_plot, temp_plot, precip_plot, ncol = 3)
# dev.off()

#### Create 2d Partial Dependency Plot of temperature and precipitation
# Create function to fit GAM and plot
plot_multi = function(scale, locale, pick_spec, filename){
  # Load output data
  output = read.csv(file = paste0("../Results/GAM_", scale, ".csv"), header = T, stringsAsFactors = F)
  
  # Load dataset with time series data
  ts_data = read.csv(file = paste0("../Data/Extracted_Data/Aggregated/", locale, "_", scale, ".csv"), header = T, stringsAsFactors = F)
  
  # Extract a vector of all species at this location. Column numbers change based on temporal scale.
  if(scale == 'weekly'){
    species = colnames(ts_data[10:(dim(ts_data)[2]-4)])
  }
  
  if(scale == 'biweekly' | scale == 'monthly'){
    species = colnames(ts_data[10:(dim(ts_data)[2]-3)])
  }
  
  # Create vectors identifying the lags at this temporal scale
  lags = make_laglists(scale = scale)
  
  # Create a data frame of lagged meteorological values
  lag_table = make_lag_table(temp = ts_data$temp_mean, precip = ts_data$precip_days, lags = lags)
  
  # Pick a species
  i = which(species == pick_spec)
  
  # Prepare x and y variables. We'll use the best fit lags of temp and precip for each species 
  vars = prep_variables(temp = lag_table[[output$Best_Temp[i]]], precip = lag_table[[output$Best_Precip[i]]], abundance = ts_data[[species[i]]])
  
  # Max number of basis splines is = to the number of unique data points (discrete days of rainfall) 
  # Thus max k (number of knots) is equal to nr of unique values + 1. 
  # Default will be 10, but datasets with fewer unique values 9 than this will be adjusted accordingly
  precip_k = 10
  if(length(unique(vars$precip)) < 10){precip_k = length(unique(vars$precip))}
  
  # Fit non-autoregressive multivariate model where smooth terms can be penalized out
  multi_gam = try(gam(abundance ~ s(temp, bs = 'cr', k = 10) + s(precip, k = precip_k, bs = 'cr'), data = vars, family = Gamma(link = "log"), method = "REML", select = T), silent = T)
  
  png(filename, height = 6, width = 6, units = "in", res = 800)
  vis.gam(multi_gam, n.grid = 50, theta = 45, phi = 32, ticktype = "detailed", #plot.type = "contour",
          color = "heat", main = sub("\\.", " ", species[i]), 
          zlab = "log(Abundance)", xlab =  "Maximum Temperature (°C)", ylab = "Days of Rainfall",
          cex.lab = 1, cex.axis = 0.8, xlim = range(vars$temp))
  dev.off()
  
  png("../Images/multi_plot2.png", height = 6, width = 12, units = "in", res = 800)
  plot(multi_gam, rug = TRUE, shift = coef(multi_gam)[1], page = 1, 
       ylim = c(0,1), seWithMean = T, 
       shade = T, shade.col = "lightblue", main = "Plot details to be changed")
  dev.off()
  
  print(summary(multi_gam))
}

plot_multi(scale = 'weekly', locale = 'Lee', 
           pick_spec = "Anopheles.quadrimaculatus", filename = "../Images/multi_plotAQ.png")

plot_multi(scale = 'monthly', locale = 'Lee', 
           pick_spec = "Aedes.albopictus", filename = "../Images/multi_plotAA.png")

#### Comparing AR Plot ####

data = read.csv("../Results/GAM_monthly.csv", header = T, stringsAsFactors = F)

data$DevDiff = data$MultiAR_DevianceExplained - data$Multi_DevianceExplained

pdf("../Images/devcomp.pdf", height = 3, width = 5)
ggplot(data, aes(x = DevDiff, color = Location)) + geom_density(alpha = 0.3) + theme_bw() +
  ggtitle("Deviance Explained in AR versus non AR Models") + xlab("Improvement in DE in Autoregressive Model") + xlim(0,100)
dev.off()

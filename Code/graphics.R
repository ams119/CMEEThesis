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
library("mgcViz")
#library("extrafont")


#### Create disease plot for introduction ####
diseases = read.csv("../Data/local_arbovirus_infections.csv", header = T, stringsAsFactors = F)

pdf("../Images/arboviralcases.pdf", height = 4, width = 6)
#loadfonts(device = "postscript")
#par(family = "LM Roman 10")
ggplot(diseases[which(diseases$Disease != "Zika"),], aes(x = Year, y = Count, color = Disease)) + 
  geom_line(lwd = 1, alpha = 0.5) + theme_bw() + geom_point() +
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
title <- expression(paste(italic("Aedes albopictus"), " Abundance"))
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

title <- expression(paste(italic("Culex nigripalpus"), " Abundance"))
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
  ggtitle("Average Maximum Temperature") +
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
  ggtitle("Precipitation") + scale_y_continuous(breaks = 0:7) +
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
fit_multi = function(scale, locale, pick_spec, filename){
  # Load output data
  output = read.csv(file = paste0("../Results/GAM2_", scale, ".csv"), header = T, stringsAsFactors = F)
  
  # Load dataset with time series data
  ts_data = read.csv(file = paste0("../Data/Extracted_Data/Aggregated/", locale, "_", scale, ".csv"), header = T, stringsAsFactors = F)
  
  # Extract a vector of all species at this location. Column numbers change based on temporal scale.
  # if(scale == 'weekly'){
  #   species = colnames(ts_data[10:(dim(ts_data)[2]-4)])
  # }
  # 
  # if(scale == 'biweekly' | scale == 'monthly'){
  #   species = colnames(ts_data[10:(dim(ts_data)[2]-3)])
  # }
  
  species = colnames(ts_data[10:(dim(ts_data)[2]-3)])
  
  # Create vectors identifying the lags at this temporal scale
  lags = make_laglists(scale = scale)
  
  # Create a data frame of lagged meteorological values
  lag_table = make_lag_table(temp = ts_data$temp_mean, precip = ts_data$precip_days, lags = lags)
  
  # Pick a species
  i = which(species == pick_spec)
  
  # Find row index in output
  output_index = which(output$Species == pick_spec & output$Location == locale)
  
  # Prepare x and y variables. We'll use the best fit lags of temp and precip for each species 
  vars = prep_variables(temp = lag_table[[output$Best_Temp[output_index]]], 
                        precip = lag_table[[output$Best_Precip[output_index]]], 
                        abundance = ts_data[[species[i]]])
  
  # Max number of basis splines is = to the number of unique data points (discrete days of rainfall) 
  # Thus max k (number of knots) is equal to nr of unique values + 1. 
  # Default will be 10, but datasets with fewer unique values 9 than this will be adjusted accordingly
  precip_k = 10
  if(length(unique(vars$precip)) < 10){precip_k = length(unique(vars$precip))}
  
  # Fit non-autoregressive multivariate model where smooth terms can be penalized out
  multi_gam = try(gam(abundance ~ s(temp, bs = 'cr', k = 10) + s(precip, k = precip_k, bs = 'cr'), data = vars, family = Gamma(link = "log"), method = "REML", select = T), silent = T)
  
  print(summary(multi_gam))
  
  return(multi_gam)
}



AQ_gam = fit_multi(scale = 'monthly', locale = 'Lee', 
           pick_spec = "Anopheles.quadrimaculatus")

pdf("../Images/multi_plotAQ.pdf", height = 4, width = 8)
par(mfrow = c(1,2), mar = c(4,4,3,1))
plot(AQ_gam, rug = FALSE, #shift = coef(multi_gam)[1],
     seWithMean = T, select = c(1), ylim = c(-1.5,1.5),
     shade = T, shade.col = "orange", 
     ylab = "Partial Dependency of Abundance", xlab = "Maximum Temperature (°C)", 
     panel.first = grid(col = "cornsilk3"))
title("Temperature", adj = 0)

plot(AQ_gam, rug = FALSE, #shift = coef(multi_gam)[1],
     ylim = c(-1.5,1.5), seWithMean = T, select = c(2),
     shade = T, shade.col = "lightblue", #main = "Precipitation", 
     ylab = "", xlab = "Days of Rainfall",
     panel.first = grid(col = "cornsilk3"), 
     yaxt = "n")
axis(side = 2, at= -1.5:1.5)
title("Precipitation", adj = 0)
dev.off()

Aalbo = fit_multi(scale = 'monthly', locale = 'Saint_Johns', 
           pick_spec = "Aedes.infirmatus")

pdf("../Images/multi_plotAQ.pdf", height = 4, width = 8)
par(mfrow = c(1,2), mar = c(4,4,3,1))
plot(Aalbo, rug = FALSE, #shift = coef(multi_gam)[1],
     seWithMean = T, select = c(1),# ylim = c(-0.5,0.5),
     shade = T, shade.col = "orange", 
     ylab = "Partial Dependency of Abundance", xlab = "Maximum Temperature (°C)", 
     panel.first = grid(col = "cornsilk3"))
title("Temperature", adj = 0)

plot(Aalbo, rug = FALSE, #shift = coef(multi_gam)[1],
     ylim = c(-1.5,1.5), seWithMean = T, select = c(2),
     shade = T, shade.col = "lightblue", #main = "Precipitation", 
     ylab = "", xlab = "Days of Rainfall",
     panel.first = grid(col = "cornsilk3"), 
     yaxt = "n")
axis(side = 2, at= -1.5:1.5)
title("Precipitation", adj = 0)
dev.off()


#### Comparing AR Plot ####

monthly = read.csv("../Results/GAM_monthly.csv", header = T, stringsAsFactors = F)

# Remove rows where either multi model did not converge
remove = which(is.na(monthly$MultiAR_DevianceExplained) | is.na(monthly$MultiAR_DevianceExplained))
monthly = monthly[-remove,]

# Also remove monthly datasets with too much zero inflation
remove = which(monthly$z_inflation_pct > 90)
monthly = monthly[-remove,]

monthly$DevDiff = monthly$MultiAR_DevianceExplained - monthly$Multi_DevianceExplained

pdf("../Images/devcomp.pdf", height = 3, width = 3.2)
# ggplot(monthly, aes(x = DevDiff, color = Location)) + geom_density(alpha = 1) + theme_bw() + 
#   xlab("Difference in Deviance (%)") + xlim(0,100) + ylab("Density") 
ggplot(monthly, aes(x = DevDiff, fill = Location)) + geom_histogram(position = "stack", breaks = seq(-5,100,5)) + theme_bw() + 
  xlab("Difference in Deviance (%)") + ylab("Number of Datasets")  + 
  scale_x_continuous(breaks= seq(-5 ,100,by=5), 
                     labels = c("", 0, rep("",3), 20, rep("",3), 40, rep("",3), 60,
                                rep("",3), 80, rep("",3), 100), 
                     limits = c(-5,100), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0,25,5)) +
  theme(legend.position = c(0.7, 0.65), legend.title = element_text(size = 8),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size =10), axis.title.y = element_text(size = 10),
        legend.text = element_text(size = 7)) + 
  scale_fill_manual(values=wes_palette(n=5, name="Darjeeling1"))
dev.off()

median(monthly$DevDiff)
mean(monthly$DevDiff)

# Find out how many models successfully converged on both multivariate models
success = monthly %>% filter(!is.na(Multi_AIC) & !is.na(MultiAR_AIC))
cat("\n", nrow(success), " out of ", nrow(monthly), " monthly datasets successfully converged in both AR and non-AR multivariate models")

# Find out how many datasets were significantly worse, the same, or better with the AR term incorporated
counts = success %>% mutate(AIC_diff = Multi_AIC - MultiAR_AIC) %>% count(AIC_diff > 2, AIC_diff < -2) 

counts = counts %>% add_column(Result = c("Equal", "Worse", "Better"))

pdf("../Images/AICcomp.pdf", height = 3, width = 3)
ggplot(counts, aes(x = Result, y = n)) + geom_bar(stat = "identity", fill = "grey50") + 
  theme_bw() + ylab("Number of Datasets") + xlab("Fit of Autoregressive Model") +
  geom_text(aes(label = sprintf("%.0f", n), y= n),  vjust = -1)+
  guides(fill=FALSE) + 
  scale_y_continuous(breaks = seq(0,100,25), limits = c(0,100)) +
  theme(panel.grid.major.x = element_blank(), 
        axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), text = element_text(size = 10))
dev.off()


#!/bin/env Rscript
# Author: Anne Marie Saunders
# Script: gantt.R
# Desc: Creates a gantt chart for inclusion in my project proposal. Uses the guidelines from https://www.molecularecologist.com/2019/01/simple-gantt-charts-in-r-with-ggplot2-and-the-tidyverse/ 
# Arguments: None
# Date: 18/10/19

# Load packages
library(ggplot2)
library(tidyverse)

# Read in data
gantt = read.csv("../Data/gantt.csv", h = T, stringsAsFactors = F)

# These vectors will control display order in the chart
acts = unique(gantt$Activity)
els = unique(gantt$Project.element)

# Reshape data frame and convert columns to factor for ordering
g.gantt <- gather(gantt, "state", "date", 4:5) %>%
  mutate(date = as.Date(date, "%Y.%m.%d"),
         Activity = factor(Activity, acts[length(acts):1]),
         Project.element=factor(Project.element, els))

head(g.gantt)

# Plot a chart
g = ggplot(g.gantt, aes(date, Activity, color = Project.element, group = Item)) + 
  geom_line(size = 5) + labs(x = NULL, y = NULL, title="Project Timeline")

g = g + theme_bw() + theme(legend.position = "bottom", legend.title = element_blank()) 
g = g + guides(colour = guide_legend(nrow = 1))

pdf("../Results/gantt.pdf", height = 4, width = 8)
g
dev.off()



#!/bin/bash
#Author: Anne Marie Saunders
#Script: run_project.sh
#Desc: Run all dissertation project scripts
#Date: 08/16/2020

# Subset full dataset to Florida data
Rscript data_subset.R

# Clean abundance data
Rscript cleaning.R

# Extract climate data
Rscript climate_extraction_batch.R 

# Aggregate data spatially and temporally
Rscript aggregation_batch.R

# Fit GAMs and perform model selection and cross validation
Rscript gam_batch.R


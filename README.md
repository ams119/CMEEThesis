![Image](Images/icllogo.jpg) 
 # Meteorological Drivers of Mosquito Abundance
This repository contains all the scripts necessary in building my 2020 thesis project. This thesis was undertaken to fulfill the requirements of my MSc Computational Methods in Ecology and Evolution course at Imperial College London. 

My aim is that all data manipulation, analysis, and compilation of all relevant presentations and reports is fully reproducible. This goal will be met with various success along the project's development. 


****
## Author 
Anne Marie Saunders<br/>anne.saunders19@imperial.ac.uk 
****
## Date
April 2020 - August 2020
****
## License
GNU General Public License v3.0
****
## Directory Map
All files and directories listed should be present in this repository prior to running. The files in the Code directory should be executed in the listed order, except for those files listed as nested within a higher script. These are called from the higher level file.

Maximum Daily Temperature and Daily Precipitation data is provided by NOAA/OAR/ESRL PSD, Boulder, Colorado, USA from their website. 

Temperature data should be downloaded as netCDF files from ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_temp/
and stored in Data/tmax/

Precipitation  data should be downloaded as netCDF files from ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_precip/
and stored in Data/precip/

### Files and Directories:
 * Code
<br/>	
	* data_subset.R

	* fix_dates.R

	* climate_extraction_batch.R

        * climate_extraction.R

	* aggregation_batch.R

        * aggregation.R

	* gam_batch.py

        * gam_functions.py

* Data 

    * vectdyn_fulldata.csv

    * gantt.csv

    * /tmax/

    * /precip/

    * /Extracted_Data/

* Results

    * /GAM_Plots/

    * /AggByLocation/

* Proposal

* Proposal_Presentation

* Images



* Acknowledgments
* References
****
## Code Files

### **blackbirds.py** 

 Desc: A regex challenge to find and print Kingdom, Phylum, and species names


 Arguments: ../Data/blackbirds.txt
### **DrawFW.py** 

 Desc: Learn how to create food web network of sample interactions


 Arguments: None
### **fmr.R** 

 Desc: Creates a plot of species mass and metabolic rate


 Arguments: ../Data/NagyEtAl1999.csv 
### **LV1.py** 

 Desc: Learn to use scipy stats packages and integrate packages


 Arguments: None
### **LV2.py** 

 Desc: Learn to use scipy stats packages and integrate packages


 Arguments: None
### **LV3.py** 

 Desc: Transform into discrete time model


 Arguments: None
### **LV4.py** 

 Desc: Add fluxuation to resource model


 Arguments: None
### **LV5.py** 

 Desc: Add fluxuations to both predator and prey models


 Arguments: None
### **Nets.R** 

 Desc: A script for making food web networks in R


 Arguments: ../Data/QMEE_Net_Mat_edges.csv, ../Data/QMEE_Net_Mat_nodes.csv
### **profileme2.py** 

 Desc: Fix the slowness of code written in profileme.py


 Arguments: None
### **profileme.py** 

 Desc: Learn how to find out what part of our code is causing speed bottlenecks


 Arguments: None
### **regexs.py** 

 Desc: Learn regex (regular expression) commands useful for text mining applications


 Arguments: None
### **run_fmr_R.py** 

 Desc: Runs an Rscript that creates a plot from a python script


 Arguments: fmr.R 
### **run_LV.py** 

 Desc: Learn to use scipy stats packages and integrate packages


 Arguments: None
### **TestR.py** 

 Desc: Practice running R script from python using subprocess


 Arguments: TestR.R
### **TestR.R** 

 Desc: A plant for testing TestR.py which runs R scripts using subprocess


 Arguments: None 
### **timeitme.py** 

 Desc: Use timeit module to figure out the fastest way to do a particular piece of a command or loop.


 Arguments: None
### **using_os.py** 

 Desc: using os to make commands applicable to any os platform


 Arguments: using_os.py
****
## Acknowledgements
Many thanks for the ongoing wisdom and  contribution from Samraat Pawar, Lauren Cator, Ruiyun Li, Matthew Watts, Sam Rund, and Deraj Wilson-Aggarwal. 
****
## References
Image from https://unichoices.co.uk/university/imperial-college-london/

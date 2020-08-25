 # Meteorological Drivers of Mosquito Abundance
This repository contains all the scripts necessary in building my 2020 thesis project. This thesis was undertaken for partial fulfilment of the requirements for my MSc Computational Methods in Ecology and Evolution course at Imperial College London. 

My aim is that all data manipulation and analysis is fully reproducible. This goal will likely be met with various success along the project's development. 

All code was written and tested with R 3.6.3


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
## Dependencies

The following packages and versions were used in this repository:

* tidyverse (1.3.0): for data manipulation <br/><br/>
* ggplot2 (3.3.2): for data visualisation <br/><br/>
* sf (0.9.1); sp (1.4.1); rgdal (1.4.8); maps (3.3.0); raster (3.0.12); tmap (3.0); tmaptools (3.0): for manipulation of spatial data <br/><br/>
* RNetCDF (2.1.1); for manipulating CDF files of meteorological data <br/><br/>
* ff (2.2.14); ffbase (0.12.8): for working with very large (7.2 GB) abundance dataset <br/><br/>
* mgcv (1.8.31): used for GAM fitting<br/><br/>

****
## Directory Map

Maximum Daily Temperature and Daily Precipitation data is provided by NOAA/OAR/ESRL PSD, Boulder, Colorado, USA from their website. 

Temperature data should be downloaded as netCDF files from [ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_temp/
and stored in a directory called Data/tmax/](ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_temp/
and stored in a directory called Data/tmax/)

Precipitation  data should be downloaded as netCDF files from [ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_precip/
and stored in a directory called Data/precip/](ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_precip/
and stored in a directory called Data/precip/)

Abundance data was sourced from the Population Biology database, which is part of the VectorBase data platform [www.vectorbase.org](www.vectorbase.org). The dataset used in this project can be accessed [here](https://drive.google.com/file/d/13LzaOvaNRyyUYqjBty03HmXi187E0jWN/view?usp=sharing) and should be stored in Data/

### Directories:
* **Code**: all code files

* **Data**: Should contain gantt.csv and have precip, temp, and abundance datasets stored here as detailed above. Will be populated with pre-processed data when code is run

* **Results**; Will be populated with analysis csvs and data visualisations when code is run. Contains jupyter notebook Results.ipynb that presents an analysis of the results of my GAM procedures.

****
## Code Files

Code should be run in the listed order or by running the shell script run_project.sh.

\*scripts marked by an asterisk are not run independently but are dependencies of other scripts

* data_subset.R: subsets Florida data of interest from vectdyn_fulldata.csv

* cleaning.R: performs various data quality checks and cleans the data.

* *climate_extraction.R: Extracts meteorological data from raster files and maps to abundance datasets

* climate_extraction_batch.R: runs climate_extraction.R iteratively for each location. 

* \*aggregation.R: aggregates time series meteorollogical and abundance data to weekly, bimonthly, and monthly time scales

* aggregation_batch.R: runs aggregation.R iteratively for each location.

* \*gam_functions.R: defines functions needed for GAM fitting

* gam_batch.R: runs gam_functions.R iteratively for each location and temporal scale

****
## Acknowledgements
Many thanks for the ongoing wisdom and  contribution from Samraat Pawar, Lauren Cator, Ruiyun Li, Matthew Watts, Sam Rund, and Deraj Wilson-Aggarwal. 



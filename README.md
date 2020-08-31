 # Predicting Vector Population Dynamics
This repository contains all the scripts necessary in building my 2020 thesis project. This thesis was undertaken for partial fulfilment of the requirements for my MSc Computational Methods in Ecology and Evolution course at Imperial College London. 

My aim is that all data manipulation and analysis is fully reproducible. This goal will likely be met with various success along the project's development. 

All code was written and tested with R 3.6.3 on Ubuntu 18.04.5.   

My project was undertaken to assess the predictive power and applicability of statistical models of mosquito abundances. These questions are important for the development of early warning systems that could predict disease risk and lessen the impacts of vector-borne disease. The final project report is available for download [here](https://drive.google.com/file/d/1SYFSzFekrR6Uy7e8hOwLjt19DgD2_Izy/view?usp=sharing)


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

Temperature data should be downloaded as netCDF files from ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_temp/
and stored in a directory called Data/tmax/

Precipitation  data should be downloaded as netCDF files from ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_precip/
and stored in a directory called Data/precip/

Abundance data was sourced from the Population Biology database, which is part of the VectorBase data platform [(www.vectorbase.org)](www.vectorbase.org). The dataset used in this project can be accessed [here](https://drive.google.com/file/d/13LzaOvaNRyyUYqjBty03HmXi187E0jWN/view?usp=sharing) and should be stored in Data/

### Directories:
* **Code**: all code files

* **Data**: Should contain gantt.csv and have precip, temp, and abundance datasets stored here as detailed above. Will be populated with pre-processed data when code is run

* **Results**; Will be populated with analysis csvs and data visualisations when code is run. Contains jupyter notebook Results.ipynb that presents an analysis of the results of my GAM procedures.

****
## Code Files

Code should be run in the listed order or by running the BASH script run_project.sh.

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
Many thanks to Dr. Samraat Pawar, Dr. Lauren Cator, Dr. Ruiyun Li, Matthew Watts, Dr. Sam Rund, and Deraj Wilson-Aggarwal for their ongoing wisdom and contribution during this project. To my supervisors, Samraat, Lauren, and Ruiyun, who offered in-depth probing as my project took shape and and enthusiastically supported me through challenging times. To Matthew Watts, who kindly shared data and coding advice. To Sam Rund, who acted as a patient resource for my many sampling, species identification, and data quality questions. To Deraj Wilson-Aggarwal for his encouragement and advice that were so needed during some of the "growth periods" of my project. It's been a privilege to work with you all.

I would also like to acknowledge the efforts of the data collectors, curators, and managers whose on-the-ground work collecting, identifying, and reporting 7GB worth of mosquitoes from trap sites made the data used for this project possible. 

I owe a debt of gratitude to my friends and family who supported me logistically, morally, and academically during this dissertation as we all adapted to life during the COVID-19 crisis. Dónal Burns, Ruth Keane, Henrique Galante, Dave Saunders, Dana Clements, Rob Daly, and the entire Daly family all went out of their way to contribute their time and support to my success and well-being. Finally, thank you to my parents, who have had my back through thick and thin and supported my dreams no matter where in the world they've taken me.




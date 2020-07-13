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
## Dependencies

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
        
    * data_subset.R

    * fix_dates.R

    * climate_extraction_batch.R
        
        * climate_extraction.R

    * aggregation_batch.R

        * aggregation.R

    * gam_batch.R

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

****
## Acknowledgements
Many thanks for the ongoing wisdom and  contribution from Samraat Pawar, Lauren Cator, Ruiyun Li, Matthew Watts, Sam Rund, and Deraj Wilson-Aggarwal. 
****
## References
Image from https://unichoices.co.uk/university/imperial-college-london/

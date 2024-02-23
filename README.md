# Searching and reporting in Campbell Collaboration systematic reviews: An assessment of current methods

We assessed the search reporting and conduct of Campbell Collaboration systematic reviews since 2017. This repository provides the R code, datasets and outputs associated with this project. The protocol for this project can be found at [https://doi.org/10.1002/cl2.1208](https://doi.org/10.1002/cl2.1208),.

## Contents

- [Project Structure](#project-structure)
- [Scripts](#scripts)
- [Dataset](#dataset)
- [Data Outputs](#data-outputs)
- [Plots](#plots)

## Project Structure

The repository contains five R script files and the original cleaned and recoded dataset. The Data Outputs directory includes intermediate datasets that are needed to generate supplementary tables and to assess reference management software usage from an unstructure data field. The plots directory contains all of the plots and tables included in the manuscript that were generated in R.

## Scripts

1. **cc_methods_part1_sources.R:** This script provides the code used to generate the plots and statistics related to source selection and reporting. 
2. **cc_methods_part2_search.R:** This script provides the code used to generate the plots and statistics related to search reporting and conduct.
3. **cc_methods_part3_supp.R:** This script provides the code used to generate the plots and statistics related to supplementary search methods.
4. **cc_methods_part4_is.R:** This script provides the code used to generate the plots and statistics related to information specialist involvement. 
5. **cc_methods_part5_other.R:** This script provides the code used to generate the plots and statistics related to the other considerations section including information management and timeliness of the search.

## Data

1. **CC-methods-data-extraction-final-20240222-clean-recode.csv**    
This is the dataset. It contains the extracted data from all included studies in the assessment (n=85). For more details about variable names and the complete data extraction tool, see the final manuscript. This file is used with the scripts above to generate the plots and tables.
2. **Data Extraction Form Items and Variable Names.xlsx**
This file contains the items included in the data extraction and the associated variable names that appear in the dataset.


## Data outputs

1. **ccmethods_isinvolve_conduct_table.csv:** This .csv file is used to generate the Supplmentary table indicating the role of information specialist involvment in search conduct.
2. **ccmethods_isinvolve_report_table.csv:** This .csv file is used to generate the Supplmentary table indicating the role of information specialist involvment in search reporting.
3. **refman_software.csv:** This .csv file contains the refman_software_text column from the main dataset for a subset of the data that reported something about reference management software for information management. This is used to manually identify specific software used in the review.
4. **refman_software_clean.csv:** This .csv file contains the manually extracted information about specific reference management software used for information management in the subset of reviews that reported this.


## Plots

This directory contains all plots and tables reported in the manuscript generated with R. File names contain the Figure number associated with the final manuscript.


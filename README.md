# Lil_Aye_Deez_2k19
GE585 Spring 2019: CDC Aedes mosquito forecast competition

### PULLING & VISUALIZING DATA

-- Packages required:
tidyverse, daymetr, zoo

1. Run *1_Climate_data_import.R* to pull in and plot county-level temperature, precipitation, and humidity data (years 2013-2017).

### CONTACT INFORMATION

Taylor Perez
ttperez@bu.edu
401-834-0343

Juliette Bateman
jbateman@bu.edu
732-966-7586

John Foster
fosterj@bu.edu
651-558-7362

Casey Kelly
caseyk@bu.edu
908-547-7209

Carina Terry
cterry@bu.edu
817-821-7115

### General Workflow

Monthly abundance data from the CDC is downladed from the `subset_aedes_data` function in the County_subset.R script. Out of the 95 counties the CDC provided data, we chose the 10 counties that have at least two years of data (2016 and 2017) that aren't all zero or just presence/absence. The function is used in 1_Climate_data_import.R, and daily weather is downloaded from daymet for each county.

Within `1_Climate_data_import.R`, daily precipitation, min and max temperature, and vapor pressure are downloaded. We then aggregate the daily observations to monthly mean, and calculate mean monthly relative humidity and total precipitaiton for each month to the data. 

Currently we have two functions for fitting the training data, `Random_Walk_Fit.R` and `GLM_Fit.R`. The first is just a random walk, while the latter fits any combination of monthly weather covariates as an additive dynamic model. Both fits model the data with a Poisson distribution as we are dealing with counth data, and the process is modelled with a normal distribution. Process error is given an uninformative gamma prior to keep the zero bound on precision and for conjugacy. 
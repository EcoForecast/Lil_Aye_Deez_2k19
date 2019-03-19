# Lil_Aye_Deez_2k19
GE585 Spring 2019: CDC Aedes mosquito forecast competition

### PULLING & VISUALIZING DATA

-- Packages required:
tidyverse, daymetr

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

# General Workflow

Monthly abundance data from the CDC is downladed from the `subset_aedes_data` function in the County_subset.R script. Out of the 95 counties the CDC provided data, we chose the 11 counties that have at least two years of data (2016 and 2017) that aren't all zero or just presence/absence. The function is used in 1_Climate_data_import.R, and daily weather is downloaded from daymet for each county.


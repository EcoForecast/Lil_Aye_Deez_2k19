# Model's three components:the data model, the process model, and the priors. 

# The data model relates the number of mosquitos collected,observed data, at any time point (monthly) to co-variates precipitation, temperature, and relative humidity.

# Our process model will be a dynamic linear model which will allow us to explore the dynamics of our system.

# The priors will use a gamma distribution (no negative) and we should determine based on what we think are reasonable means and precisions for each covariate.

devtools::install_github("EcoForecast/ecoforecastR")
source("County_subset.R")
source("1_Climate_data_import.R")

data.training <- aedes.data$data.training
county.training <- data.training$state_county
aegypti.hindcast.out <- vector() #empty vector to store output which will be params, mcmc, predict, model, data
for(i in 1:length(unique(data.training$state_county))){
   data.plot <- subset(data.training, state_county == county.training[i]) %>%
     unite("year_month", year, month, sep = "_")
   ecoforecastR::fit_dlm(model=list(obs="~data.plot$num_aegypti_collected",fixed="~clim.dat.monthly[i] + clim.dat.monthly[i]$tmin + clim.dat.monthly[i]$tmax + clim.dat.monthly[i]$vp"),clim.dat.monthly)
}


albopictus.hindcast.out <- vector() #empty vector to store output which will be params, mcmc, predict, model, data
 for(i in 1:length(unique(data.training$state_county))){
   data.plot <- subset(data.training, state_county == county.training[i]) %>%
     unite("year_month", year, month, sep = "_")
   ecoforecastR::fit_dlm(model=list(obs="data.plot$num_albopictus_collected",fixed="~clim.dat.monthly[i] + clim.dat.monthly[i]$tmin + clim.dat.monthly[i]$tmax + clim.dat.monthly[i]$vp"),clim.dat.monthly)
 }
 
 
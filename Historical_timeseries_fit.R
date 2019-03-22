# Model's three components:the data model, the process model, and the priors. 

# The data model relates the number of mosquitos collected,observed data, at any time point (monthly) to co-variates precipitation, temperature, and relative humidity.

# Our process model will be a dynamic linear model which will allow us to explore the dynamics of our system.

# The priors will use a gamma distribution (no negative) and we should determine based on what we think are reasonable means and precisions for each covariate.

devtools::install_github("EcoForecast/ecoforecastR")
source("County_subset.R")
source("1_Climdate_data_import.R")


aegypti.hindcast.out <- vector() #empty vector to store output which will be params, mcmc, predict, model, data
for(i in 1:length(unique(data.training$state_county))){
   data.plot <- subset(data.training, state_county == county.training[i]) %>%
     unite("year_month", year, month, sep = "_")
   ecoforecastR::fit_dlm(model=list(obs="data.plot$num_aegypti_collected",fixed="~clim.dat.monthly[i] + clim.dat.monthly[i]$tmin + clim.dat.monthly[i]$tmax + clim.dat.monthly[i]$vp"),clim.dat.monthly)
}


albopictus.hindcast.out <- vector() #empty vector to store output which will be params, mcmc, predict, model, data
 for(i in 1:length(unique(data.training$state_county))){
   data.plot <- subset(data.training, state_county == county.training[i]) %>%
     unite("year_month", year, month, sep = "_")
   ecoforecastR::fit_dlm(model=list(obs="data.plot$num_albopictus_collected",fixed="~clim.dat.monthly[i] + clim.dat.monthly[i]$tmin + clim.dat.monthly[i]$tmax + clim.dat.monthly[i]$vp"),clim.dat.monthly)
 }
 
 
# another method we could use is the following (based off lab 7) #############################

# converting training data in matrix
data.merged <- matrix(data.training, ncol=13, byrow=FALSE) 

#merging mosquito and covariate data
for(i in 1:length(clim.dat.monthly)){
  data.merged <- merge(data.training, clim.dat.monthly[[i]], by = "row.names", all = TRUE)
}
colnames(data.merged) <- c("state_county", "statefp", "countyfp", "year", "month", "trap_type", "num_collection_events", "num_sites", "num_trap_nights", "num_aegypti_collected", "num_albopictus_collected","num_collections_aegypti",
                           "tmin", "tmax", "vp", "date")


# in this model x will be the number of aedes collected

aedes.model = "
model{
### Loop over all individuals
for(i in 1:ni){

#### Data Model: aedes collected
for(t in 1:nt){
ae.col[i,t] ~ dnorm(x[i,t],tau_ae.col)
}

#### Data Model: temp
for(t in 2:nt){
temp[i,t] <- x[i,t]-x[i,t-1]
vp[i,t] ~ dnorm(temp[i,t],tau_temp)
}

#### Data Model: vp (humidity)
for(t in 3:nt){
hum[i,t] <- x[i,t]-x[i,t-1]
vp[i,t] ~ dnorm(hum[i,t],tau_hum)
}

#### Process Model
for(t in 2:nt){
ae.pop[i,t] <- x[i,t-1] + temp.bar + hum.bar #ae.pop is expected number of mosquitos collected in next month, temp.bar is mean temp, hum.bar is mean humidity
x[i,t]~dnorm(ae.pop[i,t],tau_add)
}

#initial conditions
x[i,1] ~ dnorm(x_ic,tau_ic)

#individual/random effects
ind.temp[i] ~ (0, tau_ind.temp)
ind.hum[i] ~ (o, tau)ind.hum)

}  ## end loop over individuals

  ## year effects
  for (t in 1:nt){
year[t] ~ dnorm(0, tau_yr)
  }

#### Priors: I put in place-holder numerical values
tau_ae.col ~ dgamma(a_ae.col,r_ae.col)
tau_temp ~ dgamma(a_temp,r_temp)
tau_hum ~ dgamma(a_hum,r_hum)
tau_add ~ dgamma(a_add, r_add)
tau_ind1 ~ dgamma(1, 0, .1)
tau_ind2 ~ dgamma(1, 0, .1)
temp.bar ~ dnorm(0.5,0.5) 
hum.bar ~ dnorm(0.5, 0.5)
}"
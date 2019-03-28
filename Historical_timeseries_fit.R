# Model's three components:the data model, the process model, and the priors
# The data model relates the number of mosquitos collected, in each county at any time point (monthly) to co-variates precipitation, temperature, and relative humidity.

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
 
 
# another method we could use is the following (based off lab 7) with RE on #############################


aedes.model = "
model{

### Loop over all individuals
for(i in 1:ni){

#### Data Model: aedes collected
for(t in 1:nz){
y[i,t] ~ dnorm(ae.col[i,z],tau_ae.col)
}

#### Data Model: temperature
for(t in 2:nz){
t[i,t] ~ dnorm(temp[i,z],tau_temp)
}

#### Data Model: humidity
for(t in 3:nz){
h[i,t] ~ dnorm(hum[i,z],tau_hum)
}

#### Data Model: precipitation
for(t in 4:nz){
p[i,t] ~ dnorm(prcp[i,z],tau_hum)
}

#### Process Model
for(t in 2:nz){
ae.new[i,t] <- t[i,z-1] + h[i,z-1] + p[i,z-1] + ind.temp[i] + ind.hum[i] + ind.prcp[i]
ae.col[i]~dnorm(ae.new[i],tau_add)  #ae.new is expected number of mosquitos collected in next month
}

#initial conditions
ae.col[i,1] ~ dnorm(ae.col_ic,tau_ic)

#individual/random effects
ind.temp[i] ~ (0, tau_ind.temp)
ind.hum[i] ~ (0, tau_ind.hum)
ind.prcp[i] ~ (0, tau_ind.prcp)

}  ## end loop over individuals

  ##monthly effects
  for (z in 1:nz){
month[z] ~ dnorm(0, tau_mo)
  }

#### Priors: I put in place-holder numerical values
tau_ae.col ~ dgamma(a_ae.col,r_ae.col) #Initial ae.collected
tau_temp ~ dgamma(a_temp,r_temp)       #observation error: Temperature
tau_hum ~ dgamma(a_hum,r_hum)          #observation error: humidity
tau_prcp ~ dgamma(a_prcp, r_prcp)      #observation error: precipitaiton
tau_add ~ dgamma(a_add, r_add)         #process error
tau_ind.temp ~ dgamma(1, 0, .1)        #prior on individual RE from temp
tau_ind.hum ~ dgamma(1, 0,.1)          #prior on individual RE form humidity
tau_ind.prcp ~ dgamma(1, 0,.1)         #prior on individual RE prcp
tau_month ~ dgamma(1, 0, .1)           #priod on monthly RE
}"


## JAGS initial conditions
nchain = 3
init <- list()
for(i in 1:nchain){
  y.samp = sample(,length(data.training$num_aegypti_collected),replace=TRUE)
  init[[i]] <- list(x = "data-set combining mosquito and covariate data per county" ,tau_add=runif(1,1,5)/var(diff(y.samp),na.rm=TRUE),
                    tau_ae.col=,tau_temp=,tau_hum=, tau_prcp=, tau_ind.temp=, tau_ind.hum=, tau_ind.prcp=, tau_month=,ind=rep(0,'data'),month=rep(0,'data'))
}

## compile JAGS model
j.model   <- jags.model (file = textConnection(aedes.model),
                         data = data$training,
                         inits = init,
                         n.chains = 3)


## run MCMC
jags.out   <- coda.samples (model = j.model,
                            variable.names = c("ae.col","tau_add","tau_temp","tau_hum","tau_prcp",
                                               "tau_ind.temp","tau_ind.hum", "tau_ind.prcp","tau_month","ind","month"),
                            n.iter = n.iter)


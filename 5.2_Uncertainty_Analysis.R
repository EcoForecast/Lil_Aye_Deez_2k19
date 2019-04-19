### This script loads in MCMC fit outputs and assesses uncertainties

library(ecoforecastR)
library(rjags)
library(coda)

#setwd("../Aedes2019Forecast/")

source("0_County_subset.R")
source("1.0_Climate_data_import.R")
source("2.1_Run_Fit.R")
source("2.2_Basic_Diagnostics_Function.R")
source("4.1_GLM_Fit.R")
source("5.1_Create_Met_Ensemble.R")

path = "./GLM_Out/"

aedes.data <- subset_aedes_data()
data.fit <- aedes.data$data.training
counties <- aedes.data$subset.counties
clim.dat.monthly <- get_daymet()

######################################

# load in aegypti model fits for each county & counts, store in list
aegypti <- list()
aegypti.files <- list.files(path = path, pattern="^aegypti") # load in files with "aegypti" at the beginning
for(i in aegypti.files){
  aegypti[[i]] <- mget(load(file.path("GLM_Out", i))) # get objects in each RData file, store
}

# load in albopictus model fits for each county & counts, store in list
albopictus <- list()
albopictus.files <- list.files(path = path, pattern="^albopictus") # load in files with "aegypti" at the beginning
for(i in albopictus.files){
  albopictus[[i]] <- mget(load(file.path("GLM_Out", i))) # get objects in each RData file, store
}

######################################

# plot by site with aggregated counts and confidence intervals
## site (s = 1-11)
## time length of fit (typically 1-end of fit)
## x limit (default x = 1-end of fit)

## NOTE the following MUST be included before calling plot.run() (see lines 59-65 and 152-161)
sp = aegypti #or albopictus
state <- as.matrix(sp[[s]]$out$predict)
ci.state <- apply(state,2,quantile,c(0.025,0.5,0.975))
time <- 1:ncol(ci.state)
summary(sp)

plot.run <- function(s,time,x=c(1,length(time))){ 
  plot(time, ci.state[2,],main=counties[s],ylab="Count", pch="",xlim=x)
  ciEnvelope(time,ci.state[1,],ci.state[3,],col="plum2")
  points(time,sp[[s]]$y,pch=16,type="b")
}

par(mfrow=c(2,2))

# aegypti plots
sp <- aegypti
for(s in 1:length(sp)){
  state <- as.matrix(sp[[s]]$out$predict)
  ci.state <- apply(state,2,quantile,c(0.025,0.5,0.975)) 
  time <- 1:ncol(ci.state)
  plot.run(s,time)
}

# albopictus plots
sp <- albopictus
for(s in 1:length(sp)){
  state <- as.matrix(sp[[s]]$out$predict)
  ci.state <- apply(state,2,quantile,c(0.025,0.5,0.975)) 
  time <- 1:ncol(ci.state)
  plot.run(s,time)
}

######################################

##` @param IC    Initial Conditions
##` @param mu    counts
##` @param beta  beta coefficients on met data
##` @param ens   ensemble of drivers 
##` @param Q     Process error (default = 0 for deterministic runs)
##` @param n     Size of Monte Carlo ensemble
##` @param NT    Length of forecast (months)

forecast <- function(IC,beta,ens,tau=0,n,NT){ 
  N <- matrix(NA,n,NT) # store results
  Nprev <- IC # initialize with initial conditions 
  for(t in 1:NT){
    mu <- Nprev + as.vector(ens[t,] %*% beta) # calculate mu using previous step + met data * beta coefficients
    negative.check <- rnorm(n,mu,tau) # get new prediction
    N[,t] <- max(negative.check, 0)
    Nprev <- N[,t]    
  }
  return(N)
}

###################################### GET MET DATA & DEFINE INPUTS FUNCTION

met <- create_met_ensemble() # get driver ensemble members

# calculate mean of all inputs

# get the input data for the forecast function based on 
## species (sp = aegypti or albopictus)
## site (s = 1-11)
## months into the future (NT = 1-12)
calc.inputs <- function(sp,s,NT){ 
  met.ens <- met[[s]]
  # only work with 1:NT months
  for(i in 1:length(met.ens)){
    met.ens[[i]] <- met.ens[[i]][seq(1:NT),]
  }
  
  # subset met ensemble to variables used in historical fit
  met.ensemble <- list(prcp = met.ens$prcp,
                  tmax = met.ens$tmax,
                  RH = met.ens$RH)
  
  # rows = months, cols = met variables
  met.means <- matrix(NA,NT,length(met.ensemble))
  for(i in 1:length(met.ensemble)){
    met.means[,i] <- apply(met.ensemble[[i]],1,mean) # drivers
  }
  met.prcp<- matrix(met.ens$prcp, ncol=1, byrow=FALSE)
  met.tmax<- matrix(met.ens$tmax, ncol=1, byrow=FALSE)
  met.RH<- matrix(met.ens$RH, ncol=1, byrow=FALSE)
  # add column of ones for intercept
  
  parameters<- as.matrix(sp[[s]]$out$params, ncol=5, byrow=FALSE) # 5 cols, 20001 rows
  met.values<- cbind(rep(1, 2001), rep(met.prcp, 2001), 
                     rep(met.tmax, 2001), rep(met.RH, 2001)) # 4 cols, 60 x 2001 rows 
  met.values<- met.values[1:100004,] #length of vars$parameters aka the length of beta and nrow in ens to be the same
  
  
  met.means <- cbind(rep(1, NT), met.means) # 4 cols, 6 rows
  params <- as.matrix(sp[[s]]$out$params) 
  param.mean <- apply(params,2,mean) # betas & tau
  IC <- as.matrix(sp[[s]]$out$predict) # initial conditions
  

  inputs <- list(met.means,param.mean,IC, parameters, met.values)
  names(inputs) <- c("met.means","param.mean","IC", "parameters", "met.values")
  return(inputs)
}

###################################### DETERMINISTIC FORECAST FOR AEGYPTI, FLORIDA LEE, 6 MONTHS


vars <- calc.inputs(aegypti,6,6)

N.det <- forecast(IC=mean(vars$IC[,ncol(vars$IC)]), # mean of initial conditions @ last time step of fit
                   beta=vars$param.mean[-5], # mean of fitted beta values (remove tau from matrix)
                   ens=vars$met.means, # matrix of mean met data variables from 5.1
                   tau=0,  # process error off (note tau must be a SD, not a precision)
                   n=1, # 1 run to generate deterministic prediction
                   NT=6) # forecast 6 months into the future

par(mfrow=c(1,1))

# define inputs necessary
sp <- aegypti # species
NT <- 6 # number of months
state <- as.matrix(sp[[6]]$out$predict) # get predictions
ci.state <- apply(state,2,quantile,c(0.025,0.5,0.975)) # confidence intervals for predictions
time <- 1:ncol(ci.state) # time length of fit 
time2 <- seq(ncol(ci.state)+1,ncol(ci.state)+NT) # length of forecast (beginning @ next time step after length of fit ends)
time3 = seq(1:125)


# plot
plot.run(6,time, x=c(80,ncol(ci.state)+NT)) # plot for Florida Lee for length of fit, set xlim to show deterministic forecast

lines(time2,N.det,col="red",lwd=2,type="l") # plot deterministic forecast


######################################

### Initial Condition Uncertainty 
N.cols<- c("black", "red", "green", "blue", "orange") #woohoo colors
trans<- 0.8 # setting transparency

vars <- calc.inputs(aegypti,6,6) #same sample site and forecast length as above

Nmc <- 500 # number of samples to run
draw <- sample.int(nrow(vars$parameters), Nmc, replace = TRUE)

N.I <- forecast(IC=vars$IC[draw,ncol(vars$IC)], # sample initial conditions @ last time step of fit
                  beta=vars$param.mean[-5], # mean of fitted beta values (remove tau from matrix)
                  ens=vars$met.means, # matrix of mean met data variables from 5.1
                  tau=0,  # process error off (note tau must be a SD, not a precision)
                  n=Nmc, # 1 run to generate deterministic prediction
                  NT=6) # forecast 6 months into the future

N.I.ci = apply(N.I,2,quantile, c(0.025, 0.5, 0.975))

plot.run(6,time,x=c(100,ncol(ci.state)+NT)) # plot for Florida Lee for length of fit
ecoforecastR::ciEnvelope(time2,N.I.ci[1,], N.I.ci[3,], col="red")
lines(time2, N.I.ci[2,], lwd=2, type="b")

######### Parameter Uncertainty-- changing beta from mean parametervalues to samples

vars <- calc.inputs(aegypti,6,6) #same sample site and forecast length as above

N.IP <- forecast(IC=vars$IC[draw,ncol(vars$IC)], # sample initial conditions @ last time step of fit
                beta=vars$param.mean[-5], # mean of fitted beta values (remove tau from matrix)
                ens=vars$met.means, # matrix of  mean met data variables from 5.1
                tau=0,  # process error off (note tau must be a SD, not a precision)
                n=Nmc, # Nmc runs
                NT=6) # forecast 6 months into the future

N.IP.ci = apply(N.IP,2,quantile, c(0.025, 0.5, 0.975))

plot.run(6,time,x=c(100,ncol(ci.state)+NT)) # plot for Florida Lee for length of fit
ecoforecastR::ciEnvelope(time,N.IP.ci[1,], N.IP.ci[3,], col="blue")
lines(time2, N.IP.ci[2,], lwd=2, type="b")



######### Driver Uncertainty-- changing ens from mean met values to met samples

vars<- calc.inputs(aegypti,6,6)

N.IPD <- forecast(IC=vars$IC[,ncol(vars$IC)], # sample initial conditions @ last time step of fit
                 beta=t(as.matrix(vars$parameters[,1:4])), # sample of fitted beta values (remove tau from matrix)
                 ens=vars$met.values, # matrix of  mean met data variables from 5.1
                 tau=0,  # process error off (note tau must be a SD, not a precision)
                 n=1, # 1 run to generate deterministic prediction
                 NT=6) # forecast 6 months into the future

N.IPD.ci = apply(N.IPD,2,quantile, c(0.025, 0.5, 0.975))

plot.run(6,time,x=c(100,ncol(ci.state)+NT))# plot for Florida Lee for length of fit
ecoforecastR::ciEnvelope(time,N.IPD.ci[1,], N.IPD.ci[3,], col="red")
lines(time2, N.IPD.ci[2,], lwd=2, type="l")

######### Process uncertainty tau = 1/sqrt(vars$param.mean[5])

vars<- calc.inputs(aegypti,6,6)

N.IPDE <- forecast(IC=vars$IC[,ncol(vars$IC)], # sample initial conditions @ last time step of fit
                  beta=t(as.matrix(vars$parameters[,1:4])), # sample of fitted beta values (remove tau from matrix)
                  ens=vars$met.values, # matrix of  mean met data variables from 5.1
                  tau=1/sqrt(vars$parameters[,5]),  # process error off (note tau must be a SD, not a precision)
                  n=1, # 1 run to generate deterministic prediction
                  NT=6) # forecast 6 months into the future

N.IPDE.ci = apply(N.IPDE, 2, quantile, c(0.025, 0.5, 0.975))
plot.run(6,time,x=c(100,ncol(ci.state)+NT))# plot for Florida Lee for length of fit
ecoforecastR::ciEnvelope(time3,N.IPDE.ci[1,], N.IPDE.ci[3,], col="red")
lines(time2, N.IPDE.ci[2,], lwd=2, type="l")


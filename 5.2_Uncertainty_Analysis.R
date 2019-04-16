### This script loads in MCMC fit outputs and assesses uncertainties

library(ecoforecastR)

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

plot.run <- function(sp,s){ # plot by species & location (numeric indicator from list)
  state <- as.matrix(sp[[s]]$out$predict)
  ci.state <- apply(state,2,quantile,c(0.025,0.5,0.975)) 
  time <- 1:ncol(ci.state)
  plot(time, ci.state[2,],main=counties[s],ylab="Count", pch="")
  ciEnvelope(time,ci.state[1,],ci.state[3,],col="plum2")
  points(time,sp[[s]]$y,pch=16,type="b")
}

par(mfrow=c(2,2))

# aegypti plots
sp <- aegypti
for(i in 1:length(sp)){
  plot.run(sp,i)
}

# albopictus plots
sp <- albopictus
for(i in 1:length(sp)){
  plot.run(sp,i)
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
    mu <- Nprev + ens[i,] %*% beta # calculate mu using previous step + met data * beta coefficients
    negative.check <- rnorm(n,mu,tau) # get new prediction
    N[,t] <- max(negative.check, 0)
    Nprev <- N[,t]    
  }
  return(N)
}

######################################

met <- create_met_ensemble() # get driver ensemble members

# calculate mean of all inputs
#calc.inputs <- function(sp,s,NT){
### getting a subscript out of bounds error on storing the parameter means
### also need to figure out a way to only grab certain met variables (ie the ones used in the GLM fit: prcp, tmax, RH)
sp=aegypti
s = 6
NT = 6 # define months
  met.ens <- met[[s]]
  # only work with 1:NT months
  for(i in 1:length(met.ens)){
    met.ens[[i]] <- met.ens[[i]][seq(1:NT),]
  }
  
  # subset met ensemble to variables used in historical fit
  met.ens <- list(prcp = met.ens$prcp,
                  tmax = met.ens$tmax,
                  RH = met.ens$RH)
  
  # rows = months, cols = met variables
  met.means <- matrix(NA,NT,length(met.ens))
  for(i in 1:length(met.ens)){
    met.means[,i] <- apply(met.ens[[i]],1,mean) # drivers
  }
  
  # add column of ones for intercept
  met.means <- cbind(rep(1, NT), met.means)
  
  params <- as.matrix(sp[[s]]$out$params) 
  param.mean <- apply(params,2,mean) # betas & tau
  IC <- as.matrix(sp[[s]]$out$predict) # initial conditions

  inputs <- list(met.means,param.mean,IC)
  names(inputs) <- c("met.mat","param.mean","IC")
  return(inputs)
#}

vars <- calc.inputs(aegypti,6,6)
vars <- inputs

N.det <- forecast(IC=mean(IC[,ncol(IC)]),
                   beta=vars$param.mean[-5],
                   ens=met.means,
                   tau=1/sqrt(vars$param.mean[5]),  ## process error off
                   n=1,
                   NT=6)

# plot
plot.run(sp, s)
time <- ## should be length of time at the certain site we're looking at
time2 <- time + NT # something like this
lines(time2,N.det,col="purple",lwd=3)
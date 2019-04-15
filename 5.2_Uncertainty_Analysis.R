### This script loads in MCMC fit outputs and assesses uncertainties

library(ecoforecastR)

setwd("~/Lil_Aye_Deez_2k19/")

source("0_County_subset.R")
source("1.0_Climate_data_import.R")
source("2.1_Run_Fit.R")
source("2.2_Basic_Diagnostics_Function.R")
source("4.1_GLM_Fit.R")
source("5.1_Create_Met_Ensemble.R")

setwd("~/Lil_Aye_Deez_2k19/GLM_Out/")

aedes.data <- subset_aedes_data()
data.fit <- aedes.data$data.training
counties <- aedes.data$subset.counties
clim.dat.monthly <- get_daymet()

######################################

# load in aegypti model fits for each county & counts, store in list
aegypti <- list()
aegypti.files <- list.files(pattern="^aegypti") # load in files with "aegypti" at the beginning
for(i in aegypti.files){
  aegypti[[i]] <- mget(load(i)) # get objects in each RData file, store
}

# load in albopictus model fits for each county & counts, store in list
albopictus <- list()
albopictus.files <- list.files(pattern="^albopictus") # load in files with "aegypti" at the beginning
for(i in albopictus.files){
  albopictus[[i]] <- mget(load(i)) # get objects in each RData file, store
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
  mu <-  + Nprev + ens[i,] %*% beta # calculate mu using previous step + met data * beta coefficients
  N[,t] <- rnorm(n,mu,tau) # get new prediction
  Nprev <- N[,t]    
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
NT = 6
  met.ens <- met[[s]]
  for(i in 1:length(met.ens)){
    met.ens[[i]] <- t(met.ens[[i]][seq(1:NT),])
  }
  met.means <- matrix(NA,NT,length(met.ens))
  for(i in 1:length(met.ens)){
    met.means[,i] <- t(apply(met.ens[[i]],2,mean)) # drivers
  }
  params <- as.matrix(sp[[s]]$out$params) 
  param.mean <- apply(params,2,mean) # betas & tau
  IC <- as.matrix(sp[[s]]$out$predict) # initial conditions

  inputs <- list(met.means,param.mean,IC)
  names(inputs) <- c("met.mat","param.mean","IC")
  return(inputs)
#}

vars <- calc.inputs(aegypti,6,6)

N.det <- forecast(IC=mean(IC[,ncol(IC)]),
                   beta=t(vars$param.mean[-6,]),
                   ens=met.means,
                   tau=0,  ## process error off
                   n=1,
                   NT=6)

# plot
plot.run()
time <- ## should be length of time at the certain site we're looking at
time2 <- time + NT # something like this
lines(time2,N.det,col="purple",lwd=3)
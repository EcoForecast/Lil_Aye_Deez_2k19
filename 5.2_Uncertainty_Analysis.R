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
##` @param Q     Process error (default = 0 for deterministic runs)
##` @param n     Size of Monte Carlo ensemble
##` @param NT    Length of forecase

forecast <- function(IC,mu,beta,ens,Q=0,n,NT){
  N <- matrix(NA,n,NT) # store results
  Nprev <- IC # initial conditions 
  for(i in 1:NT){
    mu <- x[i-1] + ens[6,i] %*% beta 
    x <- dnorm(mu[i], tau_proc)
    N[,t] <- rlnorm(n,mu,Q)
    Nprev <- N[,t]    
  }
  return(N)
}

ens <- create_met_ensemble() # get driver ensemble members

# ## calculate mean of all inputs
# ppt.mean <- matrix(apply(ensemble[6],2,mean),1,NT) ## driver
# ## parameters
# params <- as.matrix(out$params)
# param.mean <- apply(params,2,mean)
# ## initial conditions
# IC <- as.matrix(out$predict)
# 
# N.det <- forecastN(IC=mean(IC[,"N[6,30]"]),
#                    r=param.mean["r_global"],
#                    Kg=param.mean["K_global"],
#                    alpha=param.mean["alpha_site[6]"],
#                    beta=param.mean["beta"],
#                    ppt=ppt.mean,
#                    Q=0,  ## process error off
#                    n=1)
# 
# ## Plot run
# plot.run()
# lines(time2,N.det,col="purple",lwd=3)
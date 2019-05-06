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

path = "./RandomWalk_Out/"

aedes.data <- subset_aedes_data()
data.fit <- aedes.data$data.training
counties <- aedes.data$subset.counties
clim.dat.monthly <- get_daymet()

######################################

# load in aegypti model fits for each county & counts, store in list
aegypti <- list()
aegypti.files <- list.files(path = path, pattern="^aegypti") # load in files with "aegypti" at the beginning
for(i in aegypti.files){
  aegypti[[i]] <- mget(load(file.path("RandomWalk_Out", i))) # get objects in each RData file, store
}

# load in albopictus model fits for each county & counts, store in list
albopictus <- list()
albopictus.files <- list.files(path = path, pattern="^albopictus") # load in files with "aegypti" at the beginning
for(i in albopictus.files){
  albopictus[[i]] <- mget(load(file.path("RandomWalk_Out", i))) # get objects in each RData file, store
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

plot.run <- function(s,time,x=c(1,length(time)), ...){ 
  plot(time, ci.state[2,],main=gsub('_', ' ',counties[s]), xlab = "Year - Month", xaxt = 'n', pch="",xlim=x,...)
  ciEnvelope(time,ci.state[1,],ci.state[3,],col="lightblue")
  points(time,sp[[s]]$y,pch=16,type="p",col='black')
  # axis(1, at = at, labels = year.month[at])
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

forecast <- function(IC,tau=0,n,NT){ 
  N <- matrix(NA,n,NT) # store results
  Nprev <- IC # initialize with initial conditions 
  for(t in 1:NT){
    mu <- Nprev
    negative.check <- rnorm(n,mu,tau) # get new prediction
    N[,t] <- (abs(negative.check) + negative.check) / 2 # check for negative predictions
    Nprev <- N[,t]    
  }
  return(N)
}

###################################### GET MET DATA & DEFINE INPUTS FUNCTION

met <- create_met_ensemble(n.ens = 100) # get driver ensemble members

# calculate mean of all inputs

# get the input data for the forecast function based on
## species (sp = aegypti or albopictus)
## site (s = 1-11)

# months into the future (NT = 1-12)
calc.inputs <- function(sp,s,NT){
  met.ens <- met[[s]]
  # only work with 1:NT months
  for(i in 1:length(met.ens)){
    met.ens[[i]] <- met.ens[[i]][seq(1:NT),]
  }

  # # subset met ensemble to variables used in historical fit
  met.ensemble <- list(prcp = met.ens$prcp,
                       tmax = met.ens$tmax,
                       RH = met.ens$RH)

  # rows = months, cols = met variables
  met.means <- matrix(NA,NT,length(met.ensemble))
  for(i in 1:length(met.ensemble)){
    met.means[,i] <- apply(met.ensemble[[i]],1,mean) # drivers
  }
  colnames(met.means) <- c("prcp", "tmax", "RH")

  met.prcp <- matrix(met.ens$prcp, ncol=1, byrow=FALSE)
  met.tmax <- matrix(met.ens$tmax, ncol=1, byrow=FALSE)
  met.RH <- matrix(met.ens$RH, ncol=1, byrow=FALSE)

  parameters<- as.matrix(sp[[s]]$params)#, ncol=1, byrow=FALSE) # 5 cols, 20001 rows
  met.values<- cbind(rep(1, 2001), rep(met.prcp, 2001),
                     rep(met.tmax, 2001), rep(met.RH, 2001)) # 4 cols, 60 x 2001 rows
  met.values<- met.values[1:100004,] #length of vars$parameters aka the length of beta and nrow in ens to be the same


  met.means <- cbind(rep(1, NT), met.means) # 4 cols, 6 rows


  params <- as.matrix(sp[[s]]$out$params)
  param.mean <- apply(params,2,mean) # betas & tau
  IC <- as.matrix(sp[[s]]$out$predict) # initial conditions


  # inputs <- list(met.means,param.mean,IC, parameters, met.values)
  inputs <- list(met.means,param.mean,IC, params, met.ensemble)
  names(inputs) <- c("met.means","param.mean","IC", "parameters", "met.values")
  return(inputs)
}

##### DETERMINISTIC FORECAST FOR AEGYPTI, FLORIDA Osceola, 6 MONTHS #####
s <- 8
NT <- 12
vars <- calc.inputs(aegypti,s,NT)

N.det <- forecast(IC=mean(vars$IC[,ncol(vars$IC)]), # mean of initial conditions @ last time step of fit
                  # beta=t(as.matrix(vars$param.mean[-5])), # mean of fitted beta values (remove tau from matrix)
                  # prcp=as.matrix(vars$met.means[,"prcp"]), # matrix of mean precip from 5.1
                  # tmax=as.matrix(vars$met.means[,"tmax"]), # matrix of mean tmax from 5.1
                  # rh=as.matrix(vars$met.means[,"RH"]), # matrix of mean rh from 5.1
                  tau=0,  # process error off (note tau must be a SD, not a precision)
                  n=1, # 1 run to generate deterministic prediction
                  NT=NT) # forecast NT months into the future

par(mfrow=c(1,1))

# define inputs necessary
sp <- aegypti # species
state <- as.matrix(sp[[s]]$out$predict) # get predictions
ci.state <- apply(state,2,quantile,c(0.025,0.5,0.975)) # confidence intervals for predictions
time <- 1:ncol(ci.state) # time length of fit 
time2 <- seq(ncol(ci.state)+1,ncol(ci.state)+NT) # length of forecast (beginning @ next time step after length of fit ends)
time3 = seq(1:time2[length(time2)])

# plot
x.start <- ncol(ci.state)-15
plot.run(s,time, x=c(x.start,ncol(ci.state)+NT)) # plot for Florida Lee for length of fit, set xlim to show deterministic forecast
lines(time2,N.det,col="red",lwd=2,type="l") # plot deterministic forecast


######################################

### Initial Condition Uncertainty 
Nmc <- 500 # number of samples to run
draw <- sample.int(nrow(vars$parameters), Nmc, replace = TRUE)

N.I <- forecast(IC=vars$IC[draw,ncol(vars$IC)], # sample initial conditions @ last time step of fit
                # beta=t(as.matrix(vars$param.mean[-5])), # mean of fitted beta values (remove tau from matrix)
                # prcp=as.matrix(vars$met.means[,"prcp"]), # matrix of mean precip from 5.1
                # tmax=as.matrix(vars$met.means[,"tmax"]), # matrix of mean tmax from 5.1
                # rh=as.matrix(vars$met.means[,"RH"]), # matrix of mean rh from 5.1
                tau=0,  # process error off (note tau must be a SD, not a precision)
                n=Nmc, # Nmc samples for propogation
                NT=NT) # forecast 6 months into the future

N.I.ci = apply(N.I,2,quantile, c(0.025, 0.5, 0.975))

plot.run(s,time,x=c(x.start,ncol(ci.state)+NT)) # plot for Florida Lee for length of fit
ecoforecastR::ciEnvelope(time2,N.I.ci[1,], N.I.ci[3,], col="red")
lines(time2, N.I.ci[2,], lwd=2, type="b")

######### Parameter Uncertainty-- changing beta from mean parametervalues to samples

N.IP <- forecast(IC=vars$IC[draw,ncol(vars$IC)], # sample initial conditions @ last time step of fit
                 # beta=vars$parameters[draw, -5], # sample of fitted beta values (remove tau from matrix)
                 # prcp=as.matrix(vars$met.means[,"prcp"]), # matrix of mean precip from 5.1
                 # tmax=as.matrix(vars$met.means[,"tmax"]), # matrix of mean tmax from 5.1
                 # rh=as.matrix(vars$met.means[,"RH"]), # matrix of mean rh from 5.1
                 # tau=0,  # process error off (note tau must be a SD, not a precision)
                 n=Nmc, # Nmc runs
                 NT=NT) # forecast NT months into the future

N.IP.ci = apply(N.IP,2,quantile, c(0.025, 0.5, 0.975))

plot.run(s,time,x=c(x.start,ncol(ci.state)+NT)) # plot for Florida Lee for length of fit
ecoforecastR::ciEnvelope(time2,N.IP.ci[1,], N.IP.ci[3,], col="lightblue")
ecoforecastR::ciEnvelope(time2,N.I.ci[1,], N.I.ci[3,], col="red")
lines(time2, N.IP.ci[2,], lwd=2, type="b")

######### Driver Uncertainty-- changing ens from mean met values to met samples

# random sample of met ensemble 
 draw.met <- sample.int(ncol(vars$met.values$prcp), Nmc, replace = TRUE)

N.IPD <- forecast(IC=vars$IC[draw,ncol(vars$IC)], # sample initial conditions @ last time step of fit
                  # beta=vars$parameters[draw, -5], # sample of fitted beta values (remove tau from matrix)
                  # prcp=as.matrix(vars$met.values$prcp[,draw.met]), # matrix of ensemble precip from 5.1
                  # tmax=as.matrix(vars$met.values$tmax[,draw.met]), # matrix of mean tmax from 5.1
                  # rh=as.matrix(vars$met.values$RH[,draw.met]), # matrix of mean rh from 5.1
                  tau=0,  # process error off (note tau must be a SD, not a precision)
                  n=Nmc, 
                  NT=NT) # forecast 6 months into the future

N.IPD.ci = apply(N.IPD,2,quantile, c(0.025, 0.5, 0.975))

plot.run(s,time,x=c(x.start,ncol(ci.state)+NT))# plot for Florida Lee for length of fit
ecoforecastR::ciEnvelope(time2,N.IPD.ci[1,], N.IPD.ci[3,], col="lightsalmon")
ecoforecastR::ciEnvelope(time2,N.IP.ci[1,], N.IP.ci[3,], col="indianred2")
ecoforecastR::ciEnvelope(time2,N.I.ci[1,], N.I.ci[3,], col="violetred4")
lines(time2, N.IP.ci[2,], lwd=2, type="l")
legend('topleft',
       legend = c('Forecast','Inital Conditions','Parameter','Driver','Process'),
       col = c('black', 'violetred4', 'indianred2','lightsalmon','peachpuff1'),
       lwd = c(2,4,4,4,4),
       cex = 0.75,
       inset = 0.02)

######### Process uncertainty tau = 1/sqrt(vars$param.mean[5])

N.IPDE <- forecast(IC=vars$IC[draw,ncol(vars$IC)], # sample initial conditions @ last time step of fit
                   # beta=vars$parameters[draw, -5], # sample of fitted beta values (remove tau from matrix)
                   # prcp=as.matrix(vars$met.values$prcp[,draw.met]), # matrix of ensemble precip from 5.1
                   # tmax=as.matrix(vars$met.values$tmax[,draw.met]), # matrix of mean tmax from 5.1
                   # rh=as.matrix(vars$met.values$RH[,draw.met]), # matrix of mean rh from 5.1
                   tau=1/sqrt(vars$parameters[draw,5]),  # process error off (note tau must be a SD, not a precision)
                   n=Nmc,
                   NT=NT) # forecast 6 months into the future

N.IPDE.ci = apply(N.IPDE, 2, quantile, c(0.025, 0.5, 0.975))

plot.run(s,time,x=c(x.start,ncol(ci.state)+NT),ylim=c(0,500))# plot for Florida Lee for length of fit
ecoforecastR::ciEnvelope(time2,N.IPDE.ci[1,], N.IPDE.ci[3,], col="peachpuff1")
ecoforecastR::ciEnvelope(time2,N.IPD.ci[1,], N.IPD.ci[3,], col="lightsalmon")
ecoforecastR::ciEnvelope(time2,N.IP.ci[1,], N.IP.ci[3,], col="indianred2")
ecoforecastR::ciEnvelope(time2,N.I.ci[1,], N.I.ci[3,], col="violetred4")
lines(time2, N.IP.ci[2,], lwd=2, type="l")
legend('topleft',
       legend = c('Forecast','Inital Conditions','Parameter','Driver','Process'),
       col = c('black', 'violetred4', 'indianred2','lightsalmon','peachpuff1'),
       lwd = c(2,4,4,4,4),
       cex = 0.75,
       inset = 0.02)

############################## FOR ALL SITES

par(mfrow=c(2,2))

sp <- aegypti
nmc = 500
sites = c(4,10) ##CHANGED THIS TO ONLY RUN CA ORANGE AND FL HILLSBORO, CHANGE BACK TO 1:11 FOR ALL
Nt = 12
aegypti.forecast.mean <- aegypti.forecast.var <- list()
for(S in sites){
  
  var <- calc.inputs(sp, S, Nt)
  
  st <- as.matrix(sp[[S]]$out$predict) # get predictions
  ci.state <- apply(st,2,quantile,c(0.025,0.5,0.975)) # confidence intervals for predictions
  time <- 1:ncol(ci.state) # time length of fit 
  time2 <- seq(ncol(ci.state)+1,ncol(ci.state)+Nt) # length of forecast (beginning @ next time step after length of fit ends)
  time3 = seq(1:time2[length(time2)])
  x.start = ncol(ci.state)-15
  
  draw <- sample.int(nrow(var$parameters), nmc, replace = TRUE)
  draw.met <- sample.int(ncol(var$met.values$prcp), nmc, replace = TRUE)
  
  N.det <- forecast(IC=mean(var$IC[,ncol(var$IC)]), # mean of initial conditions @ last time step of fit
                    # beta=t(as.matrix(var$param.mean[-5])), # mean of fitted beta values (remove tau from matrix)
                    # prcp=as.matrix(var$met.means[,"prcp"]), # matrix of mean precip from 5.1
                    # tmax=as.matrix(var$met.means[,"tmax"]), # matrix of mean tmax from 5.1
                    # rh=as.matrix(var$met.means[,"RH"]), # matrix of mean rh from 5.1
                    tau=0,  # process error off (note tau must be a SD, not a precision)
                    n=1, # 1 run to generate deterministic prediction
                    NT=Nt) # forecast NT months into the future
  N.I <- forecast(IC=var$IC[draw,ncol(var$IC)], # sample initial conditions @ last time step of fit
                  # beta=t(as.matrix(var$param.mean[-5])), # mean of fitted beta values (remove tau from matrix)
                  # prcp=as.matrix(var$met.means[,"prcp"]), # matrix of mean precip from 5.1
                  # tmax=as.matrix(var$met.means[,"tmax"]), # matrix of mean tmax from 5.1
                  # rh=as.matrix(var$met.means[,"RH"]), # matrix of mean rh from 5.1
                  tau=0,  # process error off (note tau must be a SD, not a precision)
                  n=nmc, # Nmc samples for propogation
                  NT=Nt) # forecast 6 months into the future
  N.IP <- forecast(IC=var$IC[draw,ncol(var$IC)], # sample initial conditions @ last time step of fit
                   # beta=var$parameters[draw, -5], # sample of fitted beta values (remove tau from matrix)
                   # prcp=as.matrix(var$met.means[,"prcp"]), # matrix of mean precip from 5.1
                   # tmax=as.matrix(var$met.means[,"tmax"]), # matrix of mean tmax from 5.1
                   # rh=as.matrix(var$met.means[,"RH"]), # matrix of mean rh from 5.1
                   tau=0,  # process error off (note tau must be a SD, not a precision)
                   n=nmc, # Nmc runs
                   NT=Nt) # forecast NT months into the future
  N.IPD <- forecast(IC=var$IC[draw,ncol(var$IC)], # sample initial conditions @ last time step of fit
                    # beta=var$parameters[draw, -5], # sample of fitted beta values (remove tau from matrix)
                    # prcp=as.matrix(var$met.values$prcp[,draw.met]), # matrix of ensemble precip from 5.1
                    # tmax=as.matrix(var$met.values$tmax[,draw.met]), # matrix of mean tmax from 5.1
                    # rh=as.matrix(var$met.values$RH[,draw.met]), # matrix of mean rh from 5.1
                    tau=0,  # process error off (note tau must be a SD, not a precision)
                    n=nmc, # 1 run to generate deterministic prediction
                    NT=Nt) # forecast 6 months into the future
  N.IPDE <- forecast(IC=var$IC[draw,ncol(var$IC)], # sample initial conditions @ last time step of fit
                     # beta=var$parameters[draw, -5], # sample of fitted beta values (remove tau from matrix)
                     # prcp=as.matrix(var$met.values$prcp[,draw.met]), # matrix of ensemble precip from 5.1
                     # tmax=as.matrix(var$met.values$tmax[,draw.met]), # matrix of mean tmax from 5.1
                     # rh=as.matrix(var$met.values$RH[,draw.met]), # matrix of mean rh from 5.1
                     tau=1/sqrt(var$parameters[draw,5]),  # process error off (note tau must be a SD, not a precision)
                     n=nmc,
                     NT=Nt)
  
  N.I.ci = apply(N.I,2,quantile, c(0.025, 0.5, 0.975)) 
  N.IP.ci = apply(N.IP,2,quantile, c(0.025, 0.5, 0.975))
  N.IPD.ci = apply(N.IPD,2,quantile, c(0.025, 0.5, 0.975))
  N.IPDE.ci = apply(N.IPDE, 2, quantile, c(0.025, 0.5, 0.975))
  aegypti.forecast.mean[[S]] <- apply(N.IPDE, 2, mean)
  aegypti.forecast.var[[S]] <- apply(N.IPDE, 2, var)
  
  # historical observation dates
  county <- counties[S]
  historical.fit <- aedes.data$data.training %>%
    filter(state_county == county) %>% 
    unite("year_month", year, month, sep = "-")
  dates.training <- unique(historical.fit$year_month)
  
  # 2018 observations
  fit.2018 <- aedes.data$data.validation %>%
    filter(state_county == county) %>% 
    unite("year_month", year, month, sep = "-")
  dates.2018 <- unique(fit.2018$year_month)
  
  # indexing vector, only puts 7 dates on the axis
  all.dates <- c(dates.training, dates.2018)
  
  plot.run(S,time,x=c(x.start,ncol(ci.state)+Nt),ylim=c(0,max(N.IPDE.ci)), xaxt = "n", ylab = "Individual Aedes aegypti")
  ecoforecastR::ciEnvelope(time2,N.IPDE.ci[1,], N.IPDE.ci[3,], col="peachpuff1")
  ecoforecastR::ciEnvelope(time2,N.IPD.ci[1,], N.IPD.ci[3,], col="lightsalmon")
  ecoforecastR::ciEnvelope(time2,N.IP.ci[1,], N.IP.ci[3,], col="indianred2")
  ecoforecastR::ciEnvelope(time2,N.I.ci[1,], N.I.ci[3,], col="violetred4")
  lines(time2, N.IP.ci[2,], lwd=2, type="l")
  legend('topleft',
         legend = c('Inital Conditions','Process'),
         col = c( 'violetred4', 'peachpuff1'),
         lwd = c(4,4),
         cex = 0.75)
  axis(1, at = at, labels = all.dates[at])
}

sp <- albopictus
nmc = 500
sites = c(4,10) ##CHANGED THIS TO ONLY RUN CA ORANGE AND FL HILLSBORO, CHANGE BACK TO 1:11 FOR ALL
Nt = 12
albopictus.forecast.mean <- albopictus.forecast.var <- list()
for(S in sites){
  
  var <- calc.inputs(sp, S, Nt)
  
  st <- as.matrix(sp[[S]]$out$predict) # get predictions
  ci.state <- apply(st,2,quantile,c(0.025,0.5,0.975)) # confidence intervals for predictions
  time <- 1:ncol(ci.state) # time length of fit 
  time2 <- seq(ncol(ci.state)+1,ncol(ci.state)+Nt) # length of forecast (beginning @ next time step after length of fit ends)
  time3 = seq(1:time2[length(time2)])
  x.start = ncol(ci.state)-15
  
  draw <- sample.int(nrow(var$parameters), nmc, replace = TRUE)
  draw.met <- sample.int(ncol(var$met.values$prcp), nmc, replace = TRUE)
  
  N.det <- forecast(IC=mean(var$IC[,ncol(var$IC)]), # mean of initial conditions @ last time step of fit
                    # beta=t(as.matrix(var$param.mean[-5])), # mean of fitted beta values (remove tau from matrix)
                    # prcp=as.matrix(var$met.means[,"prcp"]), # matrix of mean precip from 5.1
                    # tmax=as.matrix(var$met.means[,"tmax"]), # matrix of mean tmax from 5.1
                    # rh=as.matrix(var$met.means[,"RH"]), # matrix of mean rh from 5.1
                    tau=0,  # process error off (note tau must be a SD, not a precision)
                    n=1, # 1 run to generate deterministic prediction
                    NT=Nt) # forecast NT months into the future
  N.I <- forecast(IC=var$IC[draw,ncol(var$IC)], # sample initial conditions @ last time step of fit
                  # beta=t(as.matrix(var$param.mean[-5])), # mean of fitted beta values (remove tau from matrix)
                  # prcp=as.matrix(var$met.means[,"prcp"]), # matrix of mean precip from 5.1
                  # tmax=as.matrix(var$met.means[,"tmax"]), # matrix of mean tmax from 5.1
                  # rh=as.matrix(var$met.means[,"RH"]), # matrix of mean rh from 5.1
                  tau=0,  # process error off (note tau must be a SD, not a precision)
                  n=nmc, # Nmc samples for propogation
                  NT=Nt) # forecast 6 months into the future
  N.IP <- forecast(IC=var$IC[draw,ncol(var$IC)], # sample initial conditions @ last time step of fit
                   # beta=var$parameters[draw, -5], # sample of fitted beta values (remove tau from matrix)
                   # prcp=as.matrix(var$met.means[,"prcp"]), # matrix of mean precip from 5.1
                   # tmax=as.matrix(var$met.means[,"tmax"]), # matrix of mean tmax from 5.1
                   # rh=as.matrix(var$met.means[,"RH"]), # matrix of mean rh from 5.1
                   tau=0,  # process error off (note tau must be a SD, not a precision)
                   n=nmc, # Nmc runs
                   NT=Nt) # forecast NT months into the future
  N.IPD <- forecast(IC=var$IC[draw,ncol(var$IC)], # sample initial conditions @ last time step of fit
                    # beta=var$parameters[draw, -5], # sample of fitted beta values (remove tau from matrix)
                    # prcp=as.matrix(var$met.values$prcp[,draw.met]), # matrix of ensemble precip from 5.1
                    # tmax=as.matrix(var$met.values$tmax[,draw.met]), # matrix of mean tmax from 5.1
                    # rh=as.matrix(var$met.values$RH[,draw.met]), # matrix of mean rh from 5.1
                    tau=0,  # process error off (note tau must be a SD, not a precision)
                    n=nmc, # 1 run to generate deterministic prediction
                    NT=Nt) # forecast 6 months into the future
  N.IPDE <- forecast(IC=var$IC[draw,ncol(var$IC)], # sample initial conditions @ last time step of fit
                     # beta=var$parameters[draw, -5], # sample of fitted beta values (remove tau from matrix)
                     # prcp=as.matrix(var$met.values$prcp[,draw.met]), # matrix of ensemble precip from 5.1
                     # tmax=as.matrix(var$met.values$tmax[,draw.met]), # matrix of mean tmax from 5.1
                     # rh=as.matrix(var$met.values$RH[,draw.met]), # matrix of mean rh from 5.1
                     tau=1/sqrt(var$parameters[draw,5]),  # process error off (note tau must be a SD, not a precision)
                     n=nmc,
                     NT=Nt)
  
  N.I.ci = apply(N.I,2,quantile, c(0.025, 0.5, 0.975)) 
  N.IP.ci = apply(N.IP,2,quantile, c(0.025, 0.5, 0.975))
  N.IPD.ci = apply(N.IPD,2,quantile, c(0.025, 0.5, 0.975))
  N.IPDE.ci = apply(N.IPDE, 2, quantile, c(0.025, 0.5, 0.975))
  albopictus.forecast.mean[[S]] <- apply(N.IPDE, 2, mean)
  albopictus.forecast.var[[S]] <- apply(N.IPDE, 2, var)
  
  # historical observation dates
  county <- counties[S]
  historical.fit <- aedes.data$data.training %>%
    filter(state_county == county) %>% 
    unite("year_month", year, month, sep = "-")
  dates.training <- unique(historical.fit$year_month)
  
  # 2018 observations
  fit.2018 <- aedes.data$data.validation %>%
    filter(state_county == county) %>% 
    unite("year_month", year, month, sep = "-")
  dates.2018 <- unique(fit.2018$year_month)
  
  # indexing vector, only puts 7 dates on the axis
  all.dates <- c(dates.training, dates.2018)
  at <- seq(1, length(all.dates), length.out = 14)
  
  plot.run(S,time,x=c(x.start,ncol(ci.state)+Nt),ylim=c(0,max(N.IPDE.ci)), ylab = "Individual Aedes albopictus")
  ecoforecastR::ciEnvelope(time2,N.IPDE.ci[1,], N.IPDE.ci[3,], col="honeydew2")
  ecoforecastR::ciEnvelope(time2,N.IPD.ci[1,], N.IPD.ci[3,], col="honeydew3")
  ecoforecastR::ciEnvelope(time2,N.IP.ci[1,], N.IP.ci[3,], col="honeydew4")
  ecoforecastR::ciEnvelope(time2,N.I.ci[1,], N.I.ci[3,], col="gray30")
  lines(time2, N.IP.ci[2,], lwd=2, type="l")
  legend('topleft',
         legend = c('Inital Conditions','Process'),
         col = c( 'gray30', 'honeydew2'),
         lwd = c(4,4),
         cex = 0.75)
  axis(1, at = at, labels = all.dates[at])
}

names(aegypti.forecast.mean) <- counties
names(aegypti.forecast.var) <- counties
names(albopictus.forecast.mean) <- counties
names(albopictus.forecast.var) <- counties

save(aegypti.forecast.mean,
     aegypti.forecast.var,
     albopictus.forecast.mean,
     albopictus.forecast.var,
     file = "forecast.mean.var.RData")

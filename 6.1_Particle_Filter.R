library(ecoforecastR)

## load functions/scripts
source("0_County_subset.R")
source("1.0_Climate_data_import.R")
source("5.1_Create_Met_Ensemble.R")

## select county (Florida_Hillsborough or California_Orange)
# county <- "Florida_Hillsborough"
county <- "California_Orange"
county <- counties[1]

## select species (albopictus or aegypti)
# spp <- "albopictus"
spp <- "aegypti"

# 2018 observations
# aggregate counts for each month, as they are separated by trap type
data.2018 <- subset(aedes.data$data.validation, state_county==county)
Y.albo <- Y.aegypti <- vector()
for(t in 1:12){
  month <- data.2018 %>% 
    filter(month == t)
  if(nrow(month) > 0){
    Y.albo[t] <- sum(month$num_albopictus_collected)
    Y.aegypti[t] <- sum(month$num_aegypti_collected)
  } else {
    Y.albo[t] <- NA
    Y.aegypti[t] <- NA
  } 
}

## get historical observations and model fit data
path <- "./GLM_Out/"

if(spp == "aegypti"){
  file <- list.files(path = path, pattern=paste("^aegypti_",county,sep="")) 
  historical.fit <- mget(load(file.path("GLM_Out", file))) # get objects in each RData file, store
  # 2018 observations
  obs.2018 <- Y.aegypti
} else {
  file <- list.files(path = path, pattern=paste("^albopictus_",county,sep="")) 
  historical.fit <- mget(load(file.path("GLM_Out", file))) # get objects in each RData file, store
  # 2018 observations
  obs.2018 <- Y.albo
}

## matrix of 2018 observations
obs.forecast <- matrix(obs.2018,12,12,byrow = TRUE)
for(i in 1:11){
  obs.forecast[i,(i+1):12] <- NA 
}
na.pad <- matrix(NA,12,3)
obs.forecast <- cbind(obs.forecast, na.pad)

## set pf settings
ne <- 20000   # number ensemble members
nt <- 12:1    # time vector

## grab params
params <- as.matrix(historical.fit$out$params)
draw <- sample.int(nrow(params), ne, replace = TRUE)
params <- params[draw,]
params.orig <- params

## load 2018 weather, grab county of interest
clim.dat.2018 <- get_daymet(2018, 2018)
inputs <- clim.dat.2018[[county]][,c("prcp","tmax","RH")]

## define forecast model
mozzie.glm <- function(N, params, inputs){
  tau <- 1/sqrt(params[,5]) # process error - standard dev
  ne <- length(N) # number ensembles
  
  # process model
  mu <- N + params[,1] + params[,2]*inputs$prcp + params[,3]*inputs$tmax + params[,4]*inputs$RH
  N <- rnorm(ne,mu,tau) # get new prediction
  N[N<0] <- 0 # check for negative predictions
  return(N)
}

ensemble.out <- pf.out <- list()
historical.obs <- historical.fit$y
last.obs <- length(historical.obs)

## function

# loop over months: iterative forecast
for(m in 1:12){
  if(m==1){
    N.last <- historical.obs[last.obs]
  } else {
    N.last <- obs.2018[m-1]
  }
  # so that there is some variation in initial condition
  if(N.last==0){N.last <- 0.001}
  N <- rpois(ne, N.last)
  
  ## foreward ensemble simulation
  output <- matrix(0,ne,nt[m])
  for(t in m:(m+nt[m]-1)){
    output[,t-m+1] <- mozzie.glm(N, params, inputs[t,])
    N <- output[,t-m+1]
  }

  ci.Ensemble <- apply(output, 2, quantile, c(0.025,0.5,0.975))

  mozzie.like <- matrix(NA, ne, nt[m])
  for(i in 1:ne){
    mozzie.like[i,] <- dpois(round(output[i,]), obs.forecast[m,m:(m+nt[m]-1)], log = TRUE)
    mozzie.like[i,is.na(mozzie.like[i,])] = 0       ## missing data as weight 1; log(1)=0
    mozzie.like[i,] = exp(cumsum(mozzie.like[i,]))  ## convert to cumulative likelihood
  }
  
  # hist(mozzie.like[,ncol(mozzie.like)],main="Final Ensemble Weights")
  
  nobs = ncol(mozzie.like)                     ## number of observations
  mozzie.pf = matrix(NA,3,nobs)
  wbar = apply(mozzie.like,2,mean)             ## mean weight at each time point
  for(i in 1:nobs){
    mozzie.pf[,i] = wtd.quantile(output[,i],mozzie.like[,i]/wbar[i],c(0.025,0.5,0.975))  ## calculate weighted median and CI
  }
  
  ensemble.out[[m]] <- ci.Ensemble
  pf.out[[m]] <- mozzie.pf
}

time.all <- 1:12
main <- paste(county, "- Ae.", spp, sep = " ")
for(i in 1:12){
  plot(time.all,pch="",ylim=c(0,max(obs.2018,na.rm = T)),xlab="Month",ylab="Individuals",main=main)
  ciEnvelope(i:12,c(ensemble.out[[i]][1,]),c(ensemble.out[[i]][3,]),col=col.alpha("lightGrey",0.6))
  ciEnvelope(i:12,c(pf.out[[i]][1,]),c(pf.out[[i]][3,]),col=col.alpha("lightBlue",0.6))
  points(1:i,obs.2018[1:i],pch=19,col=2)
  points((i+1):12,obs.2018[(i+1):12],pch=19)
  lines(i:12,pf.out[[i]][2,])
  legend("topleft",c("Ensemble Forecast","PF Forecast","Median PF","Assimilated Observation","Future Observations"),
         lty = c(1,1,1,NA,NA),
         lwd = c(7,7,1,NA,NA),
         pch = c(NA,NA,NA,19,19),
         col = c("lightGrey","lightBlue",1,2,1))
}


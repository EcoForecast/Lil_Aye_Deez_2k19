library(ecoforecastR)
library(rjags)
library(coda)

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
  aegypti[[i]] <- mget(load(file.path(path, i))) # get objects in each RData file, store
}

# load in albopictus model fits for each county & counts, store in list
albopictus <- list()
albopictus.files <- list.files(path = path, pattern="^albopictus") # load in files with "aegypti" at the beginning
for(i in albopictus.files){
  albopictus[[i]] <- mget(load(file.path(path, i))) # get objects in each RData file, store
}

# root mean squared error
rmse <- function(pred, obs){
  n <- length(pred)
  rmse <- sqrt(mean((pred - obs)^2))
  return(rmse)
}

## R-squared calculation relative to 1:1 line
r_square_one2one <- function(pred, obs){
  obs.mean <- mean(obs)
  sum.sq.error <- sum((pred - obs)^2)
  diff.obs.mu <- sum((obs - obs.mean)^2)
  r.square <- 1 - (sum.sq.error/diff.obs.mu)
  return(r.square)
}


par(mfrow=c(2,2))

##albopictus plots and stats

for(c in 1:length(counties)){
  ##get average of parameters
  avg.param <- apply(as.matrix(albopictus[[c]]$out$params), 2, mean)
  
  ##vector to store predictions
  mu <- rep(0, length(albopictus[[c]]$y))
  mu[1] <- albopictus[[c]]$y[1]
  
  met.data <- clim.dat.monthly
  met.driver <- c("prcp", "tmax", "RH")
  
  # get met data for county and variables of interest
  met.sub <- as.matrix(met.data[[c]][,met.driver])
  
  # add column of 1's for intercept
  MET <- cbind(rep(1, nrow(met.sub)), met.sub)
  
  ##get predicted values using average parameters
  for(i in 2:length(albopictus[[c]]$y)){
    mu[i] = albopictus[[c]]$y[i-1] + MET[i,]%*%avg.param[-5]
    }

  ##plot observed vs. expected and 1:1 line
  plot(mu,albopictus[[c]]$y, xlab = "Predicted Values", ylab = "Observed Values", main=gsub('_', ' ',counties[c]))
  abline(0,1, col = "darkorange1", lwd=2)
  
  ##print statistics
  print(counties[c])
  print(paste("RMSE:",rmse(mu, albopictus[[c]]$y),sep=" "))
  print(paste("R2:", r_square_one2one(mu, albopictus[[c]]$y),sep=" "))
}

##aegypti plots and stats

for(c in 1:length(counties)){
  ##get average of parameters
  avg.param <- apply(as.matrix(aegypti[[c]]$out$params), 2, mean)
  
  ##vector to store predictions
  mu <- rep(0, length(aegypti[[c]]$y))
  mu[1] <- aegypti[[c]]$y[1]
  
  met.data <- clim.dat.monthly
  met.driver <- c("prcp", "tmax", "RH")
  
  # get met data for county and variables of interest
  met.sub <- as.matrix(met.data[[c]][,met.driver])
  
  # add column of 1's for intercept
  MET <- cbind(rep(1, nrow(met.sub)), met.sub)
  
  ##get predicted values using average parameters
  for(i in 2:length(aegypti[[c]]$y)){
    mu[i] = aegypti[[c]]$y[i-1] + MET[i,]%*%avg.param[-5]
  }
  
  ##plot observed vs. expected and 1:1 line
  plot(mu,aegypti[[c]]$y, xlab = "Predicted Values", ylab = "Observed Values", main=gsub('_', ' ',counties[c]))
  abline(0,1, col = "lightslateblue", lwd=2)
  
  ##print statistics
  print(counties[c])
  print(paste("RMSE:",rmse(mu, aegypti[[c]]$y),sep=" "))
  print(paste("R2:", r_square_one2one(mu, aegypti[[c]]$y),sep=" "))
}

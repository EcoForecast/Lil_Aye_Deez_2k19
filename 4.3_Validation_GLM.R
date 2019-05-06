source("4.2_Historical_GLM.R")

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

##albopictus plots and stats

for(c in 1:length(counties)) {

##get average of parameters
avg.param = apply(albo.param.GLM[[c]], 2, mean)

avg.param = as.matrix(avg.param)

##vector to store predictions
mu = rep(0, length(albo.y[[c]]))
mu[1] = albo.y[[c]][1]

met.data = clim.dat.monthly
met.driver = c("prcp", "tmax", "RH")

# get met data for county and variables of interest
met.sub <- as.matrix(met.data[[c]][,met.driver])

# add column of 1's for intercept
MET <- cbind(rep(1, nrow(met.sub)), met.sub)


##get predicted values using average parameters
for(i in 2:length(albo.y[[c]])) {
  
  mu[i] = albo.y[[c]][i-1] + MET[i,]%*%avg.param[-5]
  
}

name = paste("Aedes albopictus", str_replace(counties[c], "_", " "), sep = "\n")

##plot observed vs. expected and 1:1 line
plot(albo.y[[c]],mu, xlab = "Observed Values", ylab = "Predicted Values", main = name, pch = 16)
abline(0,1, col = "darkred", lwd = 2)

##print statistics
print(counties[c])
print("RMSE")
print(rmse(mu, albo.y[[c]]))
print("R2")
print(r_square_one2one(mu, albo.y[[c]]))

}

##aegypti plots and stats

for(c in 1:length(counties)) {
  
  ##get average of parameters
  avg.param = apply(aegypti.param.GLM[[c]], 2, mean)
  
  avg.param = as.matrix(avg.param)
  
  ##vector to store predictions
  mu = rep(0, length(aegypti.y[[c]]))
  mu[1] = aegypti.y[[c]][1]
  
  met.data = clim.dat.monthly
  met.driver = c("prcp", "tmax", "RH")
  
  # get met data for county and variables of interest
  met.sub <- as.matrix(met.data[[c]][,met.driver])
  
  # add column of 1's for intercept
  MET <- cbind(rep(1, nrow(met.sub)), met.sub)
  
  
  ##get predicted values using average parameters
  for(i in 2:length(aegypti.y[[c]])) {
    
    mu[i] = aegypti.y[[c]][i-1] + MET[i,]%*%avg.param[-5]
    
  }
  
  name = paste("Aedes aegypti", str_replace(counties[c], "_", " "), sep = "\n")
  
  ##plot observed vs. expected and 1:1 line
  plot(aegypti.y[[c]],mu, xlab = "Observed Values", ylab = "Predicted Values", main = name, pch = 16)
  abline(0,1, col = "darkred", lwd = 2)
  
  ##print statistics
  print(counties[c])
  print("RMSE")
  print(rmse(mu, aegypti.y[[c]]))
  print("R2")
  print(r_square_one2one(mu, aegypti.y[[c]]))
  
}


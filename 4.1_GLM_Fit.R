
#' Function for the GLM model. Observations are modelled under a Poisson, while the mozzies are 
#' modelled with normal process error. Monthly weather drivers can be added as a vector, and any
#' combination of drivers can be used. Just make sure the names in the met.driver argument match 
#' colnames from daymet!
#' 
#' @param county.name Name of county of interest
#' @param spp species, one of "albo" or "aegypti"
#' @param aedes.data mozzie data
#' @param met.data monthly weather data
#' @param met.driver vector of met variables to use in model. Any of "prcp", "sumprcp", "tmin", "tmax", "RH"
#' @param n.iter number of iterations, default = 5000
#' @param inits initial conditions, default = NULL

GLM_Fit <- function(county.name, spp, aedes.data, met.data, met.driver, inits = NULL, ...){
  
  # get county of interest and create a "year-month" column
  county.sub <- data.fit %>% 
    filter(state_county == counties[i]) 
  for(i in 1:nrow(county.sub)){
    if(county.sub$month[i]==10){
      county.sub$new.month[i] <- 91 
    } else if(county.sub$month[i]==11){
      county.sub$new.month[i] <- 92 
    } else if(county.sub$month[i]==12){
      county.sub$new.month[i] <- 93 
    } else {
      county.sub$new.month[i] <- county.sub$month[i] 
    }
  }
  
  county.sub <- county.sub %>% 
    unite("year_month", year, month, sep = "-", remove = FALSE) %>% 
    unite("year_month_seq", year, new.month, sep = "-")
  
  # aggregate counts for each month, as they are separated by trap type
  y.albo <- aggregate(county.sub$num_albopictus_collected, by = list(county.sub$year_month_seq), FUN = sum)[,2]
  y.aegypti <- aggregate(county.sub$num_aegypti_collected, by = list(county.sub$year_month_seq), FUN = sum)[,2]
  
  # get met data for county and variables of interest
  met.sub <- as.matrix(met.data[[county.name]][,met.driver])

  # add column of 1's for intercept
  MET <- cbind(rep(1, nrow(met.sub)), met.sub)
  
  # use appropriate data (which species?)
  if(spp == "albo"){
    y <- y.albo
  } else {
    y <- y.aegypti
  }
  
  # beta means - for prior
  b0 <- rep(0, ncol(MET))
  
  # beta precisions - for prior
  Vb <- solve(diag(10000, ncol(MET)))
  
  # create data list for JAGS
  data <- list(y = y,
               n.month = length(y),
               MET = MET,
               b0 = b0,
               Vb = Vb)
  
  model <- "
  model{
    
    #### Data Model
    for(i in 1:n.month){
      y[i] ~ dpois(x[i])
    }
    
    #### Process Model
    for(i in 2:n.month){
      mu[i] <- x[i-1] + MET[i,] %*% beta 
      x[i] ~ dnorm(mu[i], tau_proc)
    }
    
    #### Priors
    x[1] ~ dpois(5)
    beta ~ dmnorm(b0, Vb)
    tau_proc ~ dgamma(0.01,0.01)
  
  }"
  
  j.model <- jags.model(file = textConnection(model),
                        data = data,
                        n.chains = 3,
                        inits = inits,
                        ...)
  
  return(j.model)
  
}

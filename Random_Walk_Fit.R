
#' Function for the random walk model. Observations are modelled under a Poisson, while the two species are 
#' modelled with multivariate normal process error as most counties are correlated
#' 
#' @param county.name Name of county of interest
#' @param data.set data
#' @param n.iter number of iterations, default = 5000
#' @param inits initial conditions, default = NULL

Random_Walk_Fit <- function(county.name, data.set, n.iter = 5000, inits = NULL){

  # get county of interest and create a "year-month" column
  county.sub <- data.set %>% 
    filter(state_county == county.name) %>% 
    unite("year_month", year, month, sep = "-")
  
  # aggregate counts for each month, as they are separated by trap type
  y.albo <- aggregate(county.sub$num_albopictus_collected, by = list(county.sub$year_month), FUN = sum)[,2]
  y.aegypti <- aggregate(county.sub$num_aegypti_collected, by = list(county.sub$year_month), FUN = sum)[,2]
  
  # combine and transpose
  y <- t(cbind(y.albo, y.aegypti))
  
  # create data list for JAGS
  data <- list(y = y,
               x = y,
               n.month = ncol(y),
               R = diag(1,2,2))
  
  model <- "
  model{
    
    #### Data Model
    for(i in 1:n.month){
      y[1,i] ~ dpois(x[1,i])
      y[2,i] ~ dpois(x[2,i])
    }
    
    #### Process Model
    for(i in 2:n.month){
      x[1:2,i] ~ dmnorm(x[1:2,i-1], SIGMA)
    }
    
    #### Priors
    x[1,1] ~ dpois(5)
    x[2,1] ~ dpois(5)
    SIGMA ~ dwish(R, 3)
  
  }"
  
  j.model <- jags.model(file = textConnection(model),
                        data = data,
                        inits = inits,
                        n.chains = 3)
  jags.out <- coda.samples(model = j.model,
                           variable.names = c("SIGMA", "x"),
                           n.iter = n.iter)
  
  return(jags.out)

}

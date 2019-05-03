source("1.0_Climate_data_import.R")

create_met_ensemble <- function(n.ens = 10){
  clim.dat.2018 <- get_daymet(2018, 2018)
  ens.2018 <- list()
  for(i in 1:length(clim.dat.2018)){
    county <- clim.dat.2018[[i]]
    county.name <- names(clim.dat.2018[i])
    date <- county$date # save dates
    county <- select(county, -date) # get rid of date column 
    
    # storage
    prcp <- sum.prcp <- tmin <- tmax <- vp <- RH <- matrix(NA, 12, n.ens)
    
    # run ensemble
    for(e in 1:n.ens){
      prcp[,e] <- jitter(county[,"prcp"], amount = sd(county[,"prcp"]))
      sum.prcp[,e] <- jitter(county[,"sum.prcp"], amount = sd(county[,"sum.prcp"]))
      tmin[,e] <- jitter(county[,"tmin"], amount = sd(county[,"tmin"]))
      tmax[,e] <- jitter(county[,"tmax"], amount = sd(county[,"tmax"]))
      vp[,e] <- jitter(county[,"vp"], amount = sd(county[,"vp"]))
      RH[,e] <- jitter(county[,"RH"], amount = sd(county[,"RH"]))
      
      # cant have negative precipitation or RH - set to 0 if negative
      prcp <- (abs(prcp) + prcp) / 2
      sum.prcp <- (abs(sum.prcp) + sum.prcp) / 2
      RH <- (abs(RH) + RH) / 2
    }
    ens.2018[[i]] <- list(prcp = prcp,
                                 sum.prcp = sum.prcp,
                                 tmin = tmin,
                                 tmax = tmax,
                                 vp = vp,
                                 RH = RH)
  }
  names(ens.2018) <- names(clim.dat.2018)
  return(ens.2018)
}

# par(mfrow = c(2,3))
# 
# for(c in 1:length(ens.2018)){
#   for(i in 1:6){
#     plot(1:12, ens.2018[[c]][[i]][,1], type = "l")
#     for(e in 2:10){
#       lines(1:12, ens.2018[[c]][[i]][,e])
#     }
#   }
# }

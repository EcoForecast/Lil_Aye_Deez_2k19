# This code fits albopictus and aegypti
# count data to a GLM in JAGS

library(ecoforecastR)

source("0_County_subset.R")
source("1.0_Climate_data_import.R")
source("2.1_Run_Fit.R")
source("2.2_Basic_Diagnostics_Function.R")
source("4.1_GLM_Fit.R")

aedes.data <- subset_aedes_data()
data.fit <- aedes.data$data.training
counties <- aedes.data$subset.counties
clim.dat.monthly <- get_daymet()

# loop for albopictus fits
for(i in 1:length(counties)){
  model <- GLM_Fit(county.name = counties[i],
                           spp = "albo",
                           aedes.data = data.fit,
                           met.data = clim.dat.monthly,
                           met.driver = c("prcp", "tmax", "RH")) 
  
  out <- Run_Fit(model, 
                      variable.names = c("tau_proc", "x", "beta"),
                      n.iter = 20000)

  cat("\n\nIterations Complete for", counties[i], "\n", i, "/", length(counties), "counties complete\n\n")
  
  # plot model diagnostics, remove burnin
  
  out <- diagnostics(out$params, out$predict, 5000)
  
  # convert to matrix
  state <- as.matrix(out$predict)
  params <- as.matrix(out$params)
  
  # remove spaces from counties[i]
  county.name <- gsub(" ", "", counties[i], fixed = TRUE)
  
  # confidence intervals
  ci.state <- apply(state, 2, quantile, c(0.025,0.5,0.975)) 
  
  time <- 1:ncol(ci.state)
  
  # get county of interest and create a "year-month" column
  county.sub <- data.fit %>% 
    filter(state_county == counties[i]) %>% 
    unite("year_month", year, month, sep = "-") 
  
  # aggregate counts for each month, as they are separated by trap type
  y <- aggregate(county.sub$num_albopictus_collected, by = list(county.sub$year_month), FUN = sum)[,2]
  
  # remove spaces from counties[i] & save 
  file.name <- paste("albopictus_", county.name, "_Precip_Tmax_RH.RData", sep = "")
  dir <- 'GLM_Out'
  save(out, y, file = file.path(dir, file.name)) # save thinned JAGS output and aggregated monthly counts for plotting
  
  plot(time, ci.state[2,], main = counties[i],pch="")
  ciEnvelope(time, ci.state[1,], ci.state[3,], col = "lightblue")
  points(time, y, pch = 16)

}

# loop for aegypti fits
for(i in 1:length(counties)){
  model <- GLM_Fit(county.name = counties[i],
                           spp = "aegypti",
                           aedes.data = data.fit,
                           met.data = clim.dat.monthly,
                           met.driver = c("prcp", "tmax", "RH")) 
  
  out <- Run_Fit(model, 
                      variable.names = c("tau_proc", "x", "beta"),
                      n.iter = 20000)

  cat("\n\nIterations Complete for", counties[i], "\n", i, "/", length(counties), "counties complete\n\n")
  
  # plot model diagnostics, remove burnin
  
  out <- diagnostics(out$params, out$predict, 5000)
  
  # convert to matrix
  state <- as.matrix(out$predict)
  params <- as.matrix(out$params)
  
  # get county of interest and create a "year-month" column
  county.sub <- data.fit %>% 
    filter(state_county == counties[i]) %>% 
    unite("year_month", year, month, sep = "-") 
  
  # aggregate counts for each month, as they are separated by trap type
  y <- aggregate(county.sub$num_aegypti_collected, by = list(county.sub$year_month), FUN = sum)[,2]
  
  # remove spaces from counties[i] & save 
  county.name <- gsub(" ", "", counties[i], fixed = TRUE)
  file.name <- paste("aegypti_", county.name, "_Precip_Tmax_RH.RData", sep = "")
  dir <- 'GLM_Out'
  save(out, y, file = file.path(dir, file.name)) # save thinned JAGS output and aggregated monthly counts for plotting
  
  # confidence intervals
  ci.state <- apply(state, 2, quantile, c(0.025,0.5,0.975)) 
  
  time <- 1:ncol(ci.state)
  
  plot(time, ci.state[2,], main = counties[i],pch="")
  ciEnvelope(time, ci.state[1,], ci.state[3,], col = "lightblue")
  points(time, y, pch = 16)
}

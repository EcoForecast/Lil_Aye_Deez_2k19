
library(ecoforecastR)
library(runjags)

source("County_subset.R")
source("Random_Walk_Fit.R")
source("Run_Forecast.R")

aedes.data <- subset_aedes_data()
data.fit <- aedes.data$data.training
counties <- aedes.data$subset.counties

# loop for albopictus fits
for(i in 1:length(counties)){
 
  model <- Random_Walk_Fit(county.name = counties[i],
                           spp = "albo",
                           data.set = data.fit,
                           n.adapt = 15000) 
  
  out <- Run_Forecast(model, 
                      variable.names = c("tau_proc", "x"),
                      n.iter = 20000)
  
  cat("\n\nIterations Complete for", counties[i], "\n", i, "/", length(counties), "counties complete\n\n")
  
  # convert to matrix
  state <- as.matrix(out$predict)
  
  # confidence intervals
  ci.state <- apply(state, 2, quantile, c(0.025,0.5,0.975)) 
  
  time <- 1:ncol(ci.state)
  
  # get county of interest and create a "year-month" column
  county.sub <- data.fit %>% 
    filter(state_county == counties[i]) %>% 
    unite("year_month", year, month, sep = "-") 
  
  # aggregate counts for each month, as they are separated by trap type
  y.albo <- aggregate(county.sub$num_albopictus_collected, by = list(county.sub$year_month), FUN = sum)[,2]
  
  plot(time, ci.state[2,], main = counties[i],pch="")
  ciEnvelope(time, ci.state[1,], ci.state[3,], col = "lightblue")
  points(time, y.albo, pch = 16)
}

# loop for aegypti fits
for(i in 1:length(counties)){
 
  model <- Random_Walk_Fit(county.name = counties[i],
                           spp = "aegypti",
                           data.set = data.fit,
                           n.adapt = 15000) 
  
  out <- Run_Forecast(model, 
                      variable.names = c("tau_proc", "x"),
                      n.iter = 20000)
  
  cat("\n\nIterations Complete for", counties[i], "\n", i, "/", length(counties), "counties complete\n\n")
  
  # convert to matrix
  state <- as.matrix(out$predict)
  
  # confidence intervals
  ci.state <- apply(state, 2, quantile, c(0.025,0.5,0.975)) 
  
  time <- 1:ncol(ci.state)
  
  # get county of interest and create a "year-month" column
  county.sub <- data.fit %>% 
    filter(state_county == counties[i]) %>% 
    unite("year_month", year, month, sep = "-") 
  
  # aggregate counts for each month, as they are separated by trap type
  y.aegypti <- aggregate(county.sub$num_aegypti_collected, by = list(county.sub$year_month), FUN = sum)[,2]
  
  plot(time, ci.state[2,], main = counties[i],pch="")
  ciEnvelope(time, ci.state[1,], ci.state[3,], col = "lightblue")
  points(time, y.aegypti, pch = 16)
}


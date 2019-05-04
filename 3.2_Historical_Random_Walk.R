
library(ecoforecastR)

source("0_County_subset.R")
source("2.1_Run_Fit.R")
source("2.2_Basic_Diagnostics_Function.R")
source("3.1_Random_Walk_Fit.R")

aedes.data <- subset_aedes_data()
data.fit <- aedes.data$data.training
counties <- aedes.data$subset.counties

# loop for albopictus fits
for(i in 1:length(counties)){
 
  model <- Random_Walk_Fit(county.name = counties[i],
                           spp = "albo",
                           data.set = data.fit,
                           n.adapt = 15000) 
  
  out <- Run_Fit(model, 
                      variable.names = c("tau_proc", "x"),
                      n.iter = 20000)
  
  cat("\n\nIterations Complete for", counties[i], "\n", i, "/", length(counties), "counties complete\n\n")
  
  # plot model diagnostics, remove burnin
  
  out <- diagnostics(out$params, out$predict, 5000)
  
  # convert to matrix
  state <- as.matrix(out$predict)
  params <- as.matrix(out$params)
  
  # remove spaces from counties[i]
  county.name <- gsub(" ", "", counties[i], fixed = TRUE)
  file.name <- paste("albopictus_", county.name, "_Precip_Tmax_RH.RData", sep = "")
  dir <- 'RandomWalk_Out'
  save(params, predict, file = file.path(dir, file.name))
  
  # confidence intervals
  ci.state <- apply(state, 2, quantile, c(0.025,0.5,0.975)) 
  
  time <- 1:ncol(ci.state)
  
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
  
  
  # vector of all dates in each time series 
  year.month = unique(county.sub$year_month)
  N.months = length(year.month)
  
  # indexing vector, only puts 7 dates on the axis
  at <- seq(1, N.months, length.out = 7)
  
  plot(time, ci.state[2,], main = counties[i],pch="", xaxt= "n", xlab = "Year - Month", ylab = "Individual Ades albopictus")
  ciEnvelope(time, ci.state[1,], ci.state[3,], col = "lightblue")
  points(time, y.albo, pch = 16)
  axis(1, at = at, labels = year.month[at]) 
}

# loop for aegypti fits
for(i in 1:length(counties)){
 
  model <- Random_Walk_Fit(county.name = counties[i],
                           spp = "aegypti",
                           data.set = data.fit,
                           n.adapt = 15000) 
  
  out <- Run_Fit(model, 
                      variable.names = c("tau_proc", "x"),
                      n.iter = 20000)
  
  cat("\n\nIterations Complete for", counties[i], "\n", i, "/", length(counties), "counties complete\n\n")
  
  # plot model diagnostics, remove burnin
  
  out <- diagnostics(out$params, out$predict, 5000)
  
  # convert to matrix
  state <- as.matrix(out$predict)
  params <- as.matrix(out$params)
  
  # remove spaces from counties[i]
  county.name <- gsub(" ", "", counties[i], fixed = TRUE)
  file.name <- paste("aegypti_", county.name, "_Precip_Tmax_RH.RData", sep = "")
  dir <- 'RandomWalk_Out'
  save(params, predict, file = file.path(dir, file.name))
  
  # confidence intervals
  ci.state <- apply(state, 2, quantile, c(0.025,0.5,0.975)) 
  
  time <- 1:ncol(ci.state)
  
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
  y.aegypti <- aggregate(county.sub$num_aegypti_collected, by = list(county.sub$year_month_seq), FUN = sum)[,2]
  
  # vector of all dates in each time series 
  year.month = unique(county.sub$year_month)
  N.months = length(year.month)
  
  # indexing vector, only puts 7 dates on the axis
  at <- seq(1, N.months, length.out = 7)
  
  plot(time, ci.state[2,], main = counties[i],pch="", xaxt = "n", xlab = "Year - Months", ylab = "Individual Ades aegypti")
  ciEnvelope(time, ci.state[1,], ci.state[3,], col = "lightblue")
  points(time, y.aegypti, pch = 16)
  axis(1, at = at, labels = year.month[at])
}


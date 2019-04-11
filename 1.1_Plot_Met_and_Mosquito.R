
# Get Met Data
source("1_Climate_data_import.R")


###### DAILY PLOTS ######

### plotting minimum and maximum temperatures for each county
for(i in 1:length(aedes.data$subset.counties)){               # plot minimum temperature
  plot(clim.dat[[i]]$date, clim.dat[[i]]$tmin..deg.c., 
       xlab = "Time", 
       ylab = "Minimum Temperature (Degrees C)", 
       main = aedes.data$subset.counties[i])
}
for(i in 1:length(aedes.data$subset.counties)){               # plot maximum temperature
  plot(clim.dat[[i]]$date, clim.dat[[i]]$tmax..deg.c., 
       xlab = "Time", 
       ylab = "Maximum Temperature (Degrees C)", 
       main = aedes.data$subset.counties[i])
}

### plotting precipitation for each county
for(i in 1:length(aedes.data$subset.counties)){               # plot total precipitation
  plot(clim.dat[[i]]$date, clim.dat[[i]]$prcp..mm.day., 
       xlab = "Time", 
       ylab = "Precipitation (mm/day)", 
       main = aedes.data$subset.counties[i])
}

### plot relative humidity
for(i in 1:length(aedes.data$subset.counties)){
  plot(clim.dat[[i]]$date, clim.dat[[i]]$RH,
       xlab = "Time", 
       ylab = "Relative Humidity (% Saturation)", 
       ylim = c(0,100),
       main = aedes.data$subset.counties[i])
}

###### MONTHLY PLOTS ######

### plotting minimum and maximum temperatures for each county
for(i in 1:length(aedes.data$subset.counties)){               # plot minimum temperature
  plot(clim.dat.monthly[[i]]$date, clim.dat.monthly[[i]]$tmin, 
       xlab = "Time", 
       ylab = "Avg Monthly Minimum Temperature (Degrees C)", 
       main = aedes.data$subset.counties[i])
}

for(i in 1:length(aedes.data$subset.counties)){               # plot maximum temperature
  plot(clim.dat.monthly[[i]]$date, clim.dat.monthly[[i]]$tmax, 
       xlab = "Time", 
       ylab = "Avg Monthly Maximum Temperature (Degrees C)", 
       main = aedes.data$subset.counties[i])
}

### plotting precipitation for each county
for(i in 1:length(aedes.data$subset.counties)){               # plot total precipitation
  plot(clim.dat.monthly[[i]]$date, clim.dat.monthly[[i]]$prcp, 
       xlab = "Time", 
       ylab = "Avg Monthly Precipitation (mm/day)", 
       main = aedes.data$subset.counties[i])
}

### plot relative humidity
for(i in 1:length(aedes.data$subset.counties)){
  plot(clim.dat.monthly[[i]]$date, clim.dat.monthly[[i]]$RH,
       xlab = "Time", 
       ylab = "Avg Monthly Relative Humidity (% Saturation)", 
       ylim = c(0,100),
       main = aedes.data$subset.counties[i])
}

### plot mosquito data
par(mfrow = c(2,2))
for(i in 1:length(aedes.data$subset.counties)){
  data.plot <- subset(aedes.data$data.training, 
                      aedes.data$data.training$state_county == aedes.data$subset.counties[i]) %>%
    unite("year_month", year, month, sep = "_")
  time <- 1:nrow(data.plot)
  plot(time[1:length(data.plot$num_aegypti_collected)], data.plot$num_aegypti_collected,
       type = "l",
       main = aedes.data$subset.counties[i],
       ylab = "Number Ae. aegypti collected")
  plot(time[1:length(data.plot$num_aegypti_collected)], data.plot$num_albopictus_collected,
       type = "l",
       main = aedes.data$subset.counties[i],
       ylab = "Number Ae. albopictus collected")
  plot(time[1:length(data.plot$num_aegypti_collected)], data.plot$num_collection_events,
       type = "l",
       ylab = "Number of collection events")
  plot(time[1:length(data.plot$num_aegypti_collected)], data.plot$num_trap_nights,
       type = "l",
       ylab = "Number of trap nights")
}


par(mfrow = c(2,2))
for(i in 1:length(unique(aedes.data$data.training$state_county))){
  data.plot <- subset(aedes.data$data.training, 
                      aedes.data$data.training$state_county == aedes.data$subset.counties[i]) %>%
    unite("year_month", year, month, sep = "_")
  main <- aedes.data$subset.counties[i]
  time <- 1:nrow(data.plot)
  plot(time, data.plot$num_aegypti_collected,
       type = "l",
       main = main,
       ylab = "Number Ae. aegypti collected")
  plot(time, data.plot$num_albopictus_collected,
       type = "l",
       main = main,
       ylab = "Number Ae. albopictus collected")
  plot(time, data.plot$num_collection_events,
       type = "l",
       ylab = "Number of collection events")
  plot(time, data.plot$num_trap_nights,
       type = "l",
       ylab = "Number of trap nights")
}

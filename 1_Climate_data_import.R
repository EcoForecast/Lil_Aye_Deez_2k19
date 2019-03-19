# This code reads in and plots
# county-level climate data

library(daymetr)

source("County_subset.R")

# grab aedes data from cdc
aedes.data <- subset_aedes_data()

### reference table of county names and locations
county.locations = matrix(NA, nrow = 11, ncol = 3)
county.locations[,1] = aedes.data$subset.counties
county.locations[,2] = c(34.052, 33.716, 34.959, 32.716, 27.990, 26.663, 25.552, 28.102, 28.323, 27.862, 29.972)
county.locations[,3] = c(-118.243, -117.831, -116.419, -117.161, -82.302, -81.954, -80.633, -81.076, -82.432, -81.691, -81.428)

### downloading the data for each county
clim.dat <- list()                                 # create an empty list to store climate variables
for(i in 1:length(aedes.data$subset.counties)){               # grab the data 
  clim.dat[[i]] <- daymetr::download_daymet(site=county.locations[i,1],
                                            lat=county.locations[i,2],
                                            lon=county.locations[i,3],
                                            start=2013,
                                            end=2017,
                                            internal=TRUE)$data
}
for(i in 1:length(aedes.data$subset.counties)){               # add dates 
  clim.dat[[i]]$date <- as.Date(paste(clim.dat[[i]]$year,clim.dat[[i]]$yday,sep = "-"),"%Y-%j")
}
names(clim.dat) <- aedes.data$subset.counties                 # name elements in list by county

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

##calculating relative humidity

## water vapor saturation pressure equation:
  ## pws = exp(20.386 - 5132/T)  (T in Kelvins) source: https://en.wikipedia.org/wiki/Vapour_pressure_of_water
##relative humidity equation:
  ## RH = pw/pws * 100   source: https://www.vaisala.com/sites/default/files/documents/Humidity_Conversion_Formulas_B210973EN-F.pdf

## function to calculate relative humidity

calc.humidity <- function(vapor.pres, temp, temp.units){
  
  ##convert to kelvin if temp is celsius or fahrenheit
  if (temp.units == "C"){
    temp = temp + 273.15
  } 
  if (temp.units == "F"){
    temp = (temp + 459.67) * 5 / 9
  }
  
  #calculate water vapor saturation pressure
  vapor.pres.sat = exp(20.386 - (5132/temp))
  
  #convert to Pa
  vapor.pres.sat = vapor.pres.sat * 133.32
  
  #calculate relative humidity 
  RH = (vapor.pres / vapor.pres.sat) * 100
  
  RH
  
}

##create a column for relative humidity 
for(i in 1:length(aedes.data$subset.counties)){
  RH = calc.humidity(clim.dat[[i]]$vp..Pa., clim.dat[[i]]$tmax..deg.c., "C")
  clim.dat[[i]] = cbind(clim.dat[[i]], RH)
}

## plot relative humidity
for(i in 1:length(aedes.data$subset.counties)){
  plot(clim.dat[[i]]$date, clim.dat[[i]]$RH,
       xlab = "Time", 
       ylab = "Relative Humidity (% Saturation)", 
       ylim = c(0,100),
       main = aedes.data$subset.counties[i])
}

##plot mosquito data

for(i in 1:length(aedes.data$data.training$state_county)){
   data.plot <- subset(aedes.data$data.training, aedes.data$data.training$state_county == aedes.data$subset.counties[i]) %>%
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
  

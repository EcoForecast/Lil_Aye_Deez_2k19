# This code reads in and plots
# county-level climate data

library(daymetr)

### reference table of county names and locations
county.locations = matrix(NA, nrow = 11, ncol = 3)
county.locations[,1] = subset.counties
county.locations[,2] = c(34.052, 33.716, 34.959, 32.716, 27.990, 26.663, 25.552, 28.102, 28.323, 27.862, 29.972)
county.locations[,3] = c(-118.243, -117.831, -116.419, -117.161, -82.302, -81.954, -80.633, -81.076, -82.432, -81.691, -81.428)

### downloading the data for each county
clim.dat <- list()                                 # create an empty list to store climate variables
for(i in 1:length(subset.counties)){               # grab the data 
  clim.dat[[i]] <- daymetr::download_daymet(site=county.locations[i,1],
                                            lat=county.locations[i,2],
                                            lon=county.locations[i,3],
                                            start=2013,
                                            end=2017,
                                            internal=TRUE)$data
}
for(i in 1:length(subset.counties)){               # add dates 
  clim.dat[[i]]$date <- as.Date(paste(clim.dat[[i]]$year,clim.dat[[i]]$yday,sep = "-"),"%Y-%j")
}
names(clim.dat) <- subset.counties                 # name elements in list by county

### plotting minimum and maximum temperatures for each county
for(i in 1:length(subset.counties)){               # plot minimum temperature
  plot(clim.dat[[i]]$date, clim.dat[[i]]$tmin..deg.c., 
       xlab = "Time", 
       ylab = "Minimum Temperature (Degrees C)", 
       main = subset.counties[i])
}
for(i in 1:length(subset.counties)){               # plot maximum temperature
  plot(clim.dat[[i]]$date, clim.dat[[i]]$tmax..deg.c., 
       xlab = "Time", 
       ylab = "Maximum Temperature (Degrees C)", 
       main = subset.counties[i])
}

### plotting precipitation for each county
for(i in 1:length(subset.counties)){               # plot total precipitation
  plot(clim.dat[[i]]$date, clim.dat[[i]]$prcp..mm.day., 
       xlab = "Time", 
       ylab = "Precipitation (mm/day)", 
       main = subset.counties[i])
}


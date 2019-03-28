# This code reads in and plots
# county-level climate data

library(daymetr)
library(zoo)

source("County_subset.R")

###### DATA IMPORT ######

### grab aedes data from cdc
aedes.data <- subset_aedes_data()

### reference table of county names and locations
county.locations = matrix(NA, nrow = length(aedes.data$subset.counties), ncol = 3)
county.locations[,1] = aedes.data$subset.counties
county.locations[,2] = c(34.052, 33.716, 34.959, 32.716, 27.990, 25.552, 28.102, 28.323, 27.862, 29.972)
county.locations[,3] = c(-118.243, -117.831, -116.419, -117.161, -82.302, -80.633, -81.076, -82.432, -81.691, -81.428)

### downloading the data for each county
clim.dat <- list()                                 # create an empty list to store climate variables
for(i in 1:length(aedes.data$subset.counties)){               # grab the data 
  SubCounty <- subset(aedes.data$data.training, state_county == county.locations[i,1])
  start.year <- min(SubCounty$year) 
  clim.dat[[i]] <- daymetr::download_daymet(site=county.locations[i,1],
                                            lat=county.locations[i,2],
                                            lon=county.locations[i,3],
                                            start=start.year,
                                            end=2017,
                                            internal=TRUE)$data
}
for(i in 1:length(aedes.data$subset.counties)){               # add dates 
  clim.dat[[i]]$date <- as.Date(paste(clim.dat[[i]]$year,clim.dat[[i]]$yday,sep = "-"),"%Y-%j")
}
names(clim.dat) <- aedes.data$subset.counties                 # name elements in list by county



### calculating relative humidity

## water vapor saturation pressure equation:
  ## pws = exp(20.386 - 5132/T)  (T in Kelvins) source: https://en.wikipedia.org/wiki/Vapour_pressure_of_water
##relative humidity equation:
  ## RH = pw/pws * 100   source: https://www.vaisala.com/sites/default/files/documents/Humidity_Conversion_Formulas_B210973EN-F.pdf

#### function to calculate relative humidity
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

### create a column for relative humidity 
for(i in 1:length(aedes.data$subset.counties)){
  RH = calc.humidity(clim.dat[[i]]$vp..Pa., clim.dat[[i]]$tmax..deg.c., "C")
  clim.dat[[i]] = cbind(clim.dat[[i]], RH)
}



###### MONTHLY DATA ######

#### daily to monthly function 
daily.to.monthly <- function(dat){
  #  Get months from dates 
  dat$Month <- format(dat$date, format = "%m")
  
  #  Get years from dates 
  dat$Year <- format(dat$date, format="%Y")
  
  #  Aggregate each variable on months and year and get the monthly means
  dat.monthly <- data.frame()
  dat.prcp <- aggregate(dat$prcp..mm.day. ~ Month + Year , dat , mean)
  dat.total.prcp <- aggregate(dat$prcp..mm.day. ~ Month + Year , dat , sum)
  dat.tmin <- aggregate(dat$tmin..deg.c. ~ Month + Year , dat ,  mean)
  dat.tmax <- aggregate(dat$tmax..deg.c. ~ Month + Year , dat ,  mean )
  dat.vp <- aggregate(dat$vp..Pa. ~ Month + Year , dat , mean )
  dat.monthly <- cbind(prcp = dat.prcp[,3],
                       sum.prcp = dat.total.prcp[,3],
                       tmin = dat.tmin[,3],
                       tmax = dat.tmax[,3],
                       vp = dat.vp[,3])
  return(as.data.frame(dat.monthly))
}

### make a new empty list to store the new monthly average values
clim.dat.monthly <- list()

#### add in parameter average monthly values using function
for(i in 1:length(clim.dat)){
  clim.dat.monthly[[i]] = daily.to.monthly(clim.dat[[i]])
}

# daily.to.monthly.dates <- function(dat){
#   #  Get months from dates 
#   dat$Month <- format(dat$date, format = "%m")
#   
#   #  Get years from dates 
#   dat$Year <- format(dat$date,format="%Y")
#   
#   #  Aggregate each variable on months and year and get the monthly means
#   # I had to redo this because I wanted to ensure the dates lined up correctly...
#   #If i included it in the previous loop, the classes were altered (factor instead of numeric)
#   dat.prcp <- aggregate( dat$prcp..mm.day. ~ Month + Year , data = dat , FUN = mean)
#   dat.date <- paste(dat.prcp$Year, dat.prcp$Month, sep = "-")
#   dat.date <- as.Date(as.yearmon(dat.date))
#   return(dat.date)
# }
# 
# ### calculate date for one county, since theyre all the same
# dates <- daily.to.monthly.dates(clim.dat$`California_Los Angeles`)
# 
# ### add dates to list
# for(i in 1:length(aedes.data$subset.counties)){
#   
#   clim.dat.monthly[[i]]$date <- dates
# }

### add name elements in list by county
names(clim.dat.monthly) <- aedes.data$subset.counties                 



### create a column for monthly relative humidity 
for(i in 1:length(aedes.data$subset.counties)){
  RH = calc.humidity(clim.dat.monthly[[i]]$vp, clim.dat.monthly[[i]]$tmax, "C")
  clim.dat.monthly[[i]] = cbind(clim.dat.monthly[[i]], RH)
}


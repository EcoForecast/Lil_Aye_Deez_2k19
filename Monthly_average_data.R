
###GETTING MONTHLY MEANS FOR VARIABLES IN ALL COUNTIES 

##CA, Los Angeles: 

#  Get months from dates 
California_LosAngeles.clim$Month <- months(California_LosAngeles.clim$date)

#  Get years from dates 
California_LosAngeles.clim$Year <- format(California_LosAngeles.clim$date,format="%y")

#  Aggregate each variable on months and year and get the monthly means

California_LosAngeles.clim.monthly <- aggregate( California_LosAngeles.clim$prcp..mm.day. ~ Month + Year , California_LosAngeles.clim , mean)
California_LosAngeles.clim.monthly$tmin <- aggregate(California_LosAngeles.clim$tmin..deg.c. ~ Month + Year , California_LosAngeles.clim , mean)
California_LosAngeles.clim.monthly$tmax <- aggregate(California_LosAngeles.clim$tmax..deg.c. ~ Month + Year , California_LosAngeles.clim , mean )
California_LosAngeles.clim.monthly$vp <- aggregate(California_LosAngeles.clim$vp..Pa. ~ Month + Year , California_LosAngeles.clim , mean )
California_LosAngeles.clim.monthly$date <- paste(California_LosAngeles.clim.monthly$Month, California_LosAngeles.clim.monthly$Year, sep = "-")



##CA, Orange County: 

#  Get months from dates 
California_Orange.clim$Month <- months(California_Orange.clim$date)

#  Get years from dates 
California_Orange.clim$Year <- format(California_Orange.clim$date,format="%y")

#  Aggregate each variable on months and year and get the monthly means

California_Orange.clim.monthly <- aggregate( California_Orange.clim$prcp..mm.day. ~ Month + Year , California_Orange.clim , mean)
California_Orange.clim.monthly$tmin <- aggregate(California_Orange.clim$tmin..deg.c. ~ Month + Year , California_Orange.clim, mean)
California_Orange.clim.monthly$tmax <- aggregate(California_Orange.clim$tmax..deg.c. ~ Month + Year , California_Orange.clim, mean )
California_Orange.clim.monthly$vp <- aggregate(California_Orange.clim$vp..Pa. ~ Month + Year , California_Orange.clim , mean )
California_Orange.clim.monthly$date <- paste(California_Orange.clim.monthly$Month, California_Orange.clim.monthly$Year, sep = "-")

##CA, SanBernardino: 

#  Get months from dates 
California_SanBernardino.clim$Month <- months(California_SanBernardino.clim$date)

#  Get years from dates 
California_SanBernardino.clim$Year <- format(California_SanBernardino.clim$date,format="%y")

#  Aggregate each variable on months and year and get the monthly means

California_SanBernardino.clim.monthly <- aggregate( California_SanBernardino.clim$prcp..mm.day. ~ Month + Year , California_SanBernardino.clim , mean)
California_SanBernardino.clim.monthly$tmin <- aggregate(California_SanBernardino.clim$tmin..deg.c. ~ Month + Year , California_SanBernardino.clim, mean)
California_SanBernardino.clim.monthly$tmax <- aggregate(California_SanBernardino.clim$tmax..deg.c. ~ Month + Year , California_SanBernardino.clim, mean )
California_SanBernardino.clim.monthly$vp <- aggregate(California_SanBernardino.clim$vp..Pa. ~ Month + Year , California_SanBernardino.clim , mean )
California_SanBernardino.clim.monthly$date <- paste(California_SanBernardino.clim.monthly$Month, California_SanBernardino.clim.monthly$Year, sep = "-")

##CA, SanDiego: 

#  Get months from dates 
California_SanDiego.clim$Month <- months(California_SanDiego.clim$date)

#  Get years from dates 
California_SanDiego.clim$Year <- format(California_SanDiego.clim$date,format="%y")

#  Aggregate each variable on months and year and get the monthly means

California_SanDiego.clim.monthly <- aggregate( California_SanDiego.clim$prcp..mm.day. ~ Month + Year , California_SanDiego.clim , mean)
California_SanDiego.clim.monthly$tmin <- aggregate(California_SanDiego.clim$tmin..deg.c. ~ Month + Year , California_SanDiego.clim, mean)
California_SanDiego.clim.monthly$tmax <- aggregate(California_SanDiego.clim$tmax..deg.c. ~ Month + Year , California_SanDiego.clim, mean )
California_SanDiego.clim.monthly$vp <- aggregate(California_SanDiego.clim$vp..Pa. ~ Month + Year , California_SanDiego.clim , mean )
California_SanDiego.clim.monthly$date <- paste(California_SanDiego.clim.monthly$Month, California_SanDiego.clim.monthly$Year, sep = "-")

##FL, Hillsborough: 

#  Get months from dates 
Florida_Hillsborough.clim$Month <- months(Florida_Hillsborough.clim$date)

#  Get years from dates 
Florida_Hillsborough.clim$Year <- format(Florida_Hillsborough.clim$date,format="%y")

#  Aggregate each variable on months and year and get the monthly means

Florida_Hillsborough.clim.monthly <- aggregate( Florida_Hillsborough.clim$prcp..mm.day. ~ Month + Year , Florida_Hillsborough.clim , mean)
Florida_Hillsborough.clim.monthly$tmin <- aggregate(Florida_Hillsborough.clim$tmin..deg.c. ~ Month + Year , Florida_Hillsborough.clim, mean)
Florida_Hillsborough.clim.monthly$tmax <- aggregate(Florida_Hillsborough.clim$tmax..deg.c. ~ Month + Year , Florida_Hillsborough.clim, mean )
Florida_Hillsborough.clim.monthly$vp <- aggregate(Florida_Hillsborough.clim$vp..Pa. ~ Month + Year , Florida_Hillsborough.clim , mean )
Florida_Hillsborough.clim.monthly$date <- paste(Florida_Hillsborough.clim.monthly$Month, Florida_Hillsborough.clim.monthly$Year, sep = "-")



##FL, Lee: 

#  Get months from dates 
Florida_Lee.clim$Month <- months(Florida_Lee.clim$date)

#  Get years from dates 
Florida_Lee.clim$Year <- format(Florida_Lee.clim$date,format="%y")

#  Aggregate each variable on months and year and get the monthly means

Florida_Lee.clim.monthly <- aggregate( Florida_Lee.clim$prcp..mm.day. ~ Month + Year , Florida_Lee.clim , mean)
Florida_Lee.clim.monthly$tmin <- aggregate(Florida_Lee.clim$tmin..deg.c. ~ Month + Year , Florida_Lee.clim, mean)
Florida_Lee.clim.monthly$tmax <- aggregate(Florida_Lee.clim$tmax..deg.c. ~ Month + Year , Florida_Lee.clim, mean )
Florida_Lee.clim.monthly$vp <- aggregate(Florida_Lee.clim$vp..Pa. ~ Month + Year , Florida_Lee.clim , mean )
Florida_Lee.clim.monthly$date <- paste(Florida_Lee.clim.monthly$Month, Florida_Lee.clim.monthly$Year, sep = "-")


##FL, MiamiDade: 

#  Get months from dates 
Florida_MiamiDade.clim$Month <- months(Florida_MiamiDade.clim$date)

#  Get years from dates 
Florida_MiamiDade.clim$Year <- format(Florida_MiamiDade.clim$date,format="%y")

#  Aggregate each variable on months and year and get the monthly means

Florida_MiamiDade.clim.monthly <- aggregate( Florida_MiamiDade.clim$prcp..mm.day. ~ Month + Year , Florida_MiamiDade.clim , mean)
Florida_MiamiDade.clim.monthly$tmin <- aggregate(Florida_MiamiDade.clim$tmin..deg.c. ~ Month + Year , Florida_MiamiDade.clim, mean)
Florida_MiamiDade.clim.monthly$tmax <- aggregate(Florida_MiamiDade.clim$tmax..deg.c. ~ Month + Year , Florida_MiamiDade.clim, mean )
Florida_MiamiDade.clim.monthly$vp <- aggregate(Florida_MiamiDade.clim$vp..Pa. ~ Month + Year , Florida_MiamiDade.clim , mean )
Florida_MiamiDade.clim.monthly$date <- paste(Florida_MiamiDade.clim.monthly$Month, Florida_MiamiDade.clim.monthly$Year, sep = "-")


##FL, Osceola: 

#  Get months from dates 
Florida_Osceola.clim$Month <- months(Florida_Osceola.clim$date)

#  Get years from dates 
Florida_Osceola.clim$Year <- format(Florida_Osceola.clim$date,format="%y")

#  Aggregate each variable on months and year and get the monthly means

Florida_Osceola.clim.monthly <- aggregate( Florida_Osceola.clim$prcp..mm.day. ~ Month + Year , Florida_Osceola.clim , mean)
Florida_Osceola.clim.monthly$tmin <- aggregate(Florida_Osceola.clim$tmin..deg.c. ~ Month + Year , Florida_Osceola.clim, mean)
Florida_Osceola.clim.monthly$tmax <- aggregate(Florida_Osceola.clim$tmax..deg.c. ~ Month + Year , Florida_Osceola.clim, mean )
Florida_Osceola.clim.monthly$vp <- aggregate(Florida_Osceola.clim$vp..Pa. ~ Month + Year , Florida_Osceola.clim , mean )
Florida_Osceola.clim.monthly$date <- paste(Florida_Osceola.clim.monthly$Month, Florida_Osceola.clim.monthly$Year, sep = "-")


##FL, Pasco: 

#  Get months from dates 
Florida_Pasco.clim$Month <- months(Florida_Pasco.clim$date)

#  Get years from dates 
Florida_Pasco.clim$Year <- format(Florida_Pasco.clim$date,format="%y")

#  Aggregate each variable on months and year and get the monthly means

Florida_Pasco.clim.monthly <- aggregate( Florida_Pasco.clim$prcp..mm.day. ~ Month + Year , Florida_Pasco.clim , mean)
Florida_Pasco.clim.monthly$tmin <- aggregate(Florida_Pasco.clim$tmin..deg.c. ~ Month + Year , Florida_Pasco.clim, mean)
Florida_Pasco.clim.monthly$tmax <- aggregate(Florida_Pasco.clim$tmax..deg.c. ~ Month + Year , Florida_Pasco.clim, mean )
Florida_Pasco.clim.monthly$vp <- aggregate(Florida_Pasco.clim$vp..Pa. ~ Month + Year , Florida_Pasco.clim , mean )
Florida_Pasco.clim.monthly$date <- paste(Florida_Pasco.clim.monthly$Month, Florida_Pasco.clim.monthly$Year, sep = "-")


##FL, Polk: 

#  Get months from dates 
Florida_Polk.clim$Month <- months(Florida_Polk.clim$date)

#  Get years from dates 
Florida_Polk.clim$Year <- format(Florida_Polk.clim$date,format="%y")

#  Aggregate each variable on months and year and get the monthly means

Florida_Polk.clim.monthly <- aggregate( Florida_Polk.clim$prcp..mm.day. ~ Month + Year , Florida_Polk.clim , mean)
Florida_Polk.clim.monthly$tmin <- aggregate(Florida_Polk.clim$tmin..deg.c. ~ Month + Year , Florida_Polk.clim, mean)
Florida_Polk.clim.monthly$tmax <- aggregate(Florida_Polk.clim$tmax..deg.c. ~ Month + Year , Florida_Polk.clim, mean )
Florida_Polk.clim.monthly$vp <- aggregate(Florida_Polk.clim$vp..Pa. ~ Month + Year , Florida_Polk.clim , mean )
Florida_Polk.clim.monthly$date <- paste(Florida_Polk.clim.monthly$Month, Florida_Polk.clim.monthly$Year, sep = "-")


##FL, St.Johns: 

#  Get months from dates 
Florida_St.Johns.clim$Month <- months(Florida_St.Johns.clim$date)

#  Get years from dates 
Florida_St.Johns.clim$Year <- format(Florida_St.Johns.clim$date,format="%y")

#  Aggregate each variable on months and year and get the monthly means

Florida_St.Johns.clim.monthly <- aggregate( Florida_St.Johns.clim$prcp..mm.day. ~ Month + Year , Florida_St.Johns.clim , mean)
Florida_St.Johns.clim.monthly$tmin <- aggregate(Florida_St.Johns.clim$tmin..deg.c. ~ Month + Year , Florida_St.Johns.clim, mean)
Florida_St.Johns.clim.monthly$tmax <- aggregate(Florida_St.Johns.clim$tmax..deg.c. ~ Month + Year , Florida_St.Johns.clim, mean )
Florida_St.Johns.clim.monthly$vp <- aggregate(Florida_St.Johns.clim$vp..Pa. ~ Month + Year , Florida_St.Johns.clim , mean )
Florida_St.Johns.clim.monthly$date <- paste(Florida_St.Johns.clim.monthly$Month, Florida_St.Johns.clim.monthly$Year, sep = "-")



#put the counties into a new list
clim.dat.monthly <- list()
clim.dat.monthly[[1]] = California_LosAngeles.clim.monthly
clim.dat.monthly[[2]] = California_Orange.clim.monthly
clim.dat.monthly[[3]] = California_SanBernardino.clim.monthly
clim.dat.monthly[[4]] = California_SanDiego.clim.monthly
clim.dat.monthly[[5]] = Florida_Hillsborough.clim.monthly
clim.dat.monthly[[6]] = Florida_Lee.clim.monthly
clim.dat.monthly[[7]] = Florida_MiamiDade.clim.monthly
clim.dat.monthly[[8]] = Florida_Osceola.clim.monthly
clim.dat.monthly[[9]] = Florida_Pasco.clim.monthly
clim.dat.monthly[[10]] = Florida_Polk.clim.monthly
clim.dat.monthly[[11]] = Florida_St.Johns.clim.monthly

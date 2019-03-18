
library(daymetr)

##table of county names and locations, for refernec
  county.locations = matrix(NA, nrow = 11, ncol = 3)
  county.locations[,1] = subset.counties
  county.locations[,2] = c(34.052, 33.716, 34.959, 32.716, 27.990, 26.663, 25.552, 28.102, 28.323, 27.862, 29.972)
  county.locations[,3] = c(-118.243, -117.831, -116.419, -117.161, -82.302, -81.954, -80.633, -81.076, -82.432, -81.691, -81.428)


##downloading the data for each county
  California_LosAngeles.clim <- daymetr::download_daymet(site = county.locations[1,1],
                                                        lat = 34.052,
                                                        lon = -118.243,
                                                        start = 2013,
                                                        end = 2017,
                                                        internal = TRUE)$data
  California_Orange.clim <- daymetr::download_daymet(site = county.locations[2,1],
                                                    lat = 33.716,
                                                    lon = -117.831,
                                                    start = 2013,
                                                    end = 2017,
                                                    internal = TRUE)$data
  California_SanBernardino.clim <- daymetr::download_daymet(site = county.locations[3,1],
                                                            lat = 34.959,
                                                            lon = -116.419,
                                                            start = 2013,
                                                            end = 2017,
                                                            internal = TRUE)$data
  California_SanDiego.clim <- daymetr::download_daymet(site = county.locations[4,1],
                                                      lat = 32.716,
                                                      lon = -117.161,
                                                      start = 2013,
                                                      end = 2017,
                                                      internal = TRUE)$data
 Florida_Hillsborough.clim <- daymetr::download_daymet(site = county.locations[5,1],
                                                      lat = 27.99,
                                                      lon = -82.302,
                                                      start = 2013,
                                                      end = 2017,
                                                      internal = TRUE)$data
Florida_Lee.clim <- daymetr::download_daymet(site = county.locations[6,1],
                                            lat = 26.663,
                                            lon = -81.954,
                                            start = 2013,
                                            end = 2017,
                                            internal = TRUE)$data
Florida_MiamiDade.clim <- daymetr::download_daymet(site = county.locations[7,1],
                                                  lat = 25.552,
                                                  lon = -80.633,
                                                  start = 2013,
                                                  end = 2017,
                                                  internal = TRUE)$data
Florida_Osceola.clim <- daymetr::download_daymet(site = county.locations[8,1],
                                                lat = 28.102,
                                                lon = -81.076,
                                                start = 2013,
                                                end = 2017,
                                                internal = TRUE)$data
Florida_Pasco.clim <- daymetr::download_daymet(site = county.locations[9,1],
                                              lat = 28.323,
                                              lon = -82.432,
                                              start = 2013,
                                              end = 2017,
                                              internal = TRUE)$data
Florida_Polk.clim <- daymetr::download_daymet(site = county.locations[10,1],
                                              lat = 27.862,
                                              lon = -81.691,
                                              start = 2013,
                                              end = 2017,
                                              internal = TRUE)$data
Florida_St.Johns.clim <- daymetr::download_daymet(site = county.locations[11,1],
                                                  lat = 29.972,
                                                  lon = -81.428,
                                                  start = 2013,
                                                  end = 2017,
                                                  internal = TRUE)$data

##putting the date in the correct format for each county
California_LosAngeles.clim$date <- as.Date(paste(California_LosAngeles.clim$year,California_LosAngeles.clim$yday,sep = "-"),"%Y-%j")
California_Orange.clim$date <- as.Date(paste(California_Orange.clim$year,California_Orange.clim$yday,sep = "-"),"%Y-%j")
California_SanBernardino.clim$date <- as.Date(paste(California_SanBernardino.clim$year,California_SanBernardino.clim$yday,sep = "-"),"%Y-%j")
California_SanDiego.clim$date <- as.Date(paste(California_SanDiego.clim$year,California_SanDiego.clim$yday,sep = "-"),"%Y-%j")
Florida_Hillsborough.clim$date <- as.Date(paste(Florida_Hillsborough.clim$year,Florida_Hillsborough.clim$yday,sep = "-"),"%Y-%j")
Florida_Lee.clim$date <- as.Date(paste(Florida_Lee.clim$year,Florida_Lee.clim$yday,sep = "-"),"%Y-%j")
Florida_MiamiDade.clim$date <- as.Date(paste(Florida_MiamiDade.clim$year,Florida_MiamiDade.clim$yday,sep = "-"),"%Y-%j")
Florida_Osceola.clim$date <- as.Date(paste(Florida_Osceola.clim$year,Florida_Osceola.clim$yday,sep = "-"),"%Y-%j")
Florida_Pasco.clim$date <- as.Date(paste(Florida_Pasco.clim$year,Florida_Pasco.clim$yday,sep = "-"),"%Y-%j")
Florida_Polk.clim$date <- as.Date(paste(Florida_Polk.clim$year,Florida_Polk.clim$yday,sep = "-"),"%Y-%j")
Florida_St.Johns.clim$date <- as.Date(paste(Florida_St.Johns.clim$year,Florida_St.Johns.clim$yday,sep = "-"),"%Y-%j")

##putting counties into a list
clim.dat <- list()
clim.dat[[1]] = California_LosAngeles.clim
clim.dat[[2]] = California_Orange.clim
clim.dat[[3]] = California_SanBernardino.clim
clim.dat[[4]] = California_SanDiego.clim
clim.dat[[5]] = Florida_Hillsborough.clim
clim.dat[[6]] = Florida_Lee.clim
clim.dat[[7]] = Florida_MiamiDade.clim
clim.dat[[8]] = Florida_Osceola.clim
clim.dat[[9]] = Florida_Pasco.clim
clim.dat[[10]] = Florida_Polk.clim
clim.dat[[11]] = Florida_St.Johns.clim

##plotting the minimum and maximum temperatures for each county

for(i in 1:11){
  
  plot(clim.dat[[i]]$m, clim.dat[[i]]$tmin..deg.c., xlab = "Time", ylab = "Minimum Temperature (Degrees C)", main = subset.counties[i])
}

for(i in 1:11){
  
  plot(clim.dat[[i]]$year, clim.dat[[i]]$tmax..deg.c., xlab = "Time", ylab = "Maximum Temperature (Degrees C)", main = subset.counties[i])
}



##plotting the monthly average minimum and maximum temperatures for each county

for(i in 1:11){
  
  plot(clim.dat.monthly[[i]]$date, clim.dat.monthly[[i]]$tmin..deg.c., xlab = "Time", ylab = "Minimum Temperature (Degrees C)", main = subset.counties[i])
}

for(i in 1:11){
  
  plot(clim.dat.monthly[[i]]$date, clim.dat.monthly[[i]]$tmax..deg.c., xlab = "Time", ylab = "Maximum Temperature (Degrees C)", main = subset.counties[i])
}


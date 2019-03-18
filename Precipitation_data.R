##plotting the monthly average precipitation for each county

for(i in 1:11){
  
  plot(clim.dat.monthly[[i]]$date, clim.dat.monthly[[i]]$`California_LosAngeles.clim$prcp..mm.day.`, xlab = "Time", ylab = "Minimum Temperature (Degrees C)", main = subset.counties[i])
}

for(i in 1:11){
  
  plot(clim.dat.monthly[[i]]$date, clim.dat.monthly[[i]]$`California_LosAngeles.clim$prcp..mm.day.`, xlab = "Time", ylab = "Maximum Temperature (Degrees C)", main = subset.counties[i])
}

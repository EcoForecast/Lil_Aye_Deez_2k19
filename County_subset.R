# This file reads in the aedes data provided from the CDC,
# and subsets the counties to a manageable number for hindcasting
# and (potentially) forecasting

library(tidyverse)

# read csv files from the web
csv <-  c("https://predict.cdc.gov/api/v1/attachments/aedes_challenge_2019/aedes_collections_california.csv", "https://predict.cdc.gov/api/v1/attachments/aedes_challenge_2019/aedes_collections_connecticut.csv",
         "https://predict.cdc.gov/api/v1/attachments/aedes_challenge_2019/aedes_collections_florida.csv", "https://predict.cdc.gov/api/v1/attachments/aedes_challenge_2019/aedes_collections_new_jersey.csv",
         "https://predict.cdc.gov/api/v1/attachments/aedes_challenge_2019/aedes_collections_new_york.csv", "https://predict.cdc.gov/api/v1/attachments/aedes_challenge_2019/aedes_collections_north_carolina.csv",
         "https://predict.cdc.gov/api/v1/attachments/aedes_challenge_2019/aedes_collections_texas.csv", "https://predict.cdc.gov/api/v1/attachments/aedes_challenge_2019/aedes_collections_wisconsin.csv")


# read csv files in working directory
# csv <- list.files(pattern = "aedes_collections_.*\\.csv")


# put all csv files in a list
data.list <- lapply(csv, read.csv, na.strings = "NA")

# create one large data frame, includes all counties
data.all <- do.call(rbind, data.list) 

# combine state and county columns just in case county names
# are repeated across states
data.all <- unite(data.all, "state_county", state, county, sep = "_") 

# vector of unique county names
counties <- unique(data.all$state_county)

# determine which counties have enough information for a hindcast
# right now lets say there has to be at least two years (2016 and 2017)
# of data but we can change this if we want. Also get rid of all zero 
# rows to exclude counties with all zero data, excluding presence data too

subset.counties <- vector() # initialize
for(i in 1:length(counties)){
  df <- filter(data.all, state_county == counties[i]) %>% # work with one county at a time
    filter(year < 2016) %>%                           # filter to years less than 2016
    filter(trap_type != "GAT-PRES") %>%               # filter out presence only rows 
    filter(num_aegypti_collected > 0) %>%             # filter 0 rows for aegypti
    filter(num_albopictus_collected > 0)              # filter 0 rows for albo
  if(nrow(df) > 0){                       # if there is data, add county name to vector
    subset.counties[i] <- counties[i] 
  }
}

# remove NAs
subset.counties <- subset.counties[which(!is.na(subset.counties))]

# keep counties we want
data.subset <- subset(data.all, state_county %in% subset.counties) 

# training data
data.training <- data.subset %>% filter(year < 2018)

# validation data (for hindcast)
data.validation <- data.subset %>% filter(year == 2018)

# run the following two lines, you'll see that the number of counties
# differs between training and validation, this is because some counties
# don't have 2018 data!!
length(table(data.training$state_county))
length(table(data.validation$state_county))

# unique counties
county.training <- unique(data.training$state_county)
county.validation <- unique(data.validation$state_county)

# find which counties are different
diff.county <- setdiff(county.training, county.validation)

# subset training data to only include counties in validation data
data.training <- subset(data.training, !(state_county %in% diff.county))

# unique counties again...
county.training <- unique(data.training$state_county)
county.validation <- unique(data.validation$state_county)

# check that counties match
if(length(which(county.training != county.validation)) != 0){
  stop("Validation and training counties do not match")
}

# forecasting data set - includes 2018 - to be used for CDC forecast
data.forecast <- subset(data.subset, state_county %in% county.training)

# write CSVs 
write_csv(data.training, "Aedes_Training_Data.csv")       # training data for hindcast
write_csv(data.validation, "Aedes_Validation_Data.csv")   # hindcast validation
write_csv(data.forecast, "Aedes_Forecast_Data.csv")       # training data for forecast

# quick and dirty time series plots by county
# top row is number of mosquitoes collected by species
# bottom row is a rough look at trapping effort
par(mfrow = c(2,2))
for(i in 1:length(unique(data.training$state_county))){
  data.plot <- subset(data.training, state_county == county.training[i]) %>%
    unite("year_month", year, month, sep = "_")
  time <- 1:nrow(data.plot)
  plot(time, data.plot$num_aegypti_collected,
       type = "l",
       main = county.training[i],
       ylab = "Number Ae. aegypti collected")
  plot(time, data.plot$num_albopictus_collected,
       type = "l",
       main = county.training[i],
       ylab = "Number Ae. albopictus collected")
  plot(time, data.plot$num_collection_events,
       type = "l",
       ylab = "Number of collection events")
  plot(time, data.plot$num_trap_nights,
       type = "l",
       ylab = "Number of trap nights")
}



# Full Palmius Script Results

#set directory to currently open script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read data in
data<- readRDS("../ownalgo/finalBinnedDataNL.rds")

# load auxiliary functions
source("../../thesis/scripts/auxFuns.R")


# read in all functions necessary
file.sources <- list.files(c("functions"), 
                           pattern="*.R$", full.names=TRUE, 
                           ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)

#select the month of march
dataExample <- select(data,time,lat,lon,accuracy,nextMeas) %>%
  arrange(time) %>% 
  mutate( nextLon = lead(lon),
          nextLat = lead(lat),
          index = 1:length(lat),
          nextTimeSec = nextMeas
  ) %>% 
  as_tbl_time(time) %>% 
  filter_time( ~ '2017-03')

# filter the data based on Palmius' filtering script
filtered<- filteringPalmius(data = dataExample)

# downsample the data based on Palmius' method
downsampledPalmius <-  downSamplingPalmius(data = filtered)

## calculate deviance

deviancePalmius <- left_join(dataExample %>% thicken("5 min"),
                  downsampledPalmius,
          by =c("time_5_min" = "time_5min")) %>% 
  select(lon,lat,lonF,latF,accuracy, time_5_min)

deviancePalmius$dist <- raster::pointDistance(as.matrix(cbind(test$lon,test$lat),ncol = 2),
                      as.matrix(cbind(test$lonF,test$latF),ncol = 2),
                      lonlat = TRUE)

# extract deviance measures
summary(deviancePalmius$dist)

# Calculate percentage withhin accuracy
deviancePalmius %>%  summarise(mean(accuracy >= dist)) 

# by bin
deviancePalmius %>% group_by(time_5_min) %>% summarise(distMean = mean(dist),
                                            distSD   = sd(dist)) %>% summarise(mean(distMean))

# Create a plot to investigate results
library(ggplot2)
library(ggthemes)
ggplot(deviancePalmius, aes(x=accuracy, y=dist)) +
  geom_point()+theme_tufte()+
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0,
              na.rm = FALSE, show.legend = NA)

## calculate imputation methods

# get time periods from downsampled data frame
timePeriods<- downsampledPalmius %>% 
  pull(time_5min)

# get the index of randomly removed 5 minute parts
remove_ind_5min<- getRemoveIndex(fin,"5 min")

# adapt the index to the downsampled period
remove_ind <- which(as.character(timePeriods) %in% remove_ind_5min[[2]])

# remove the indexed parts 
downsampled5minRemoved<- downsampledPalmius
downsampled5minRemoved[remove_ind,c("lonF","latF")] <- NA

# set interval paramaetersand extract features pre-imputation
interval <- 300
preImp<- featureExtractPalmius(downsampled5minRemoved)

#impute thhe missing values
imputedP<- palmiusImputeLoop(preImp) 

# get distance measures for imputed values
calcImputeDist(imputedP,remove_ind,downsampled = downsampledPalmius)

#how many did it fail to predict?

is.na(downsampledPalmius[remove_ind,c("lonF")]) %>% sum() #276 out of the 2228 are missing
332-276 #failed to impute


# test two: remove 25% of hours at random

# get the  randomly removed 5 minute parts
remove_ind_hour<- getRemoveIndex(fin,"1 hour",seed = 2003)
removedHours<- remove_ind_hour[[2]]

# thicken into timeperiods
timePeriods<- downsampledPalmius %>% thicken("hour",colname = "hour")

# create an index
remove_ind <- which(as.character(timePeriods$hour) %in% removedHours)

# remove measurements
timePeriods[remove_ind,c("lonF")] %>% is.na() %>% sum()# it has 256 NA's
timePeriods[remove_ind,c("lonF","latF")] <- NA

#extract features for imputation
preImp<- featureExtractPalmius(timePeriods[,-4]) 
interval <- 300

# get imputation
imputedP<- palmiusImputeLoop(preImp) 

# calculate distance measures
calcImputeDist(imputedP,remove_ind,downsampled = downsampledPalmius) 

# test three: remove 25% of days at random

#get the days removed
removedDays <- getRemoveIndexDay(fin, seed = 1115)[[1]]

# gather remove index
timePeriods<- downsampledPalmius %>% thicken("day",colname = "day")
remove_ind <- which(as.character(timePeriods$day) %in% removedDays)

#remove the indexed measurments and count NA's
timePeriods[remove_ind,c("lonF")] %>% is.na() %>% sum()# it has 374 NA's
timePeriods[remove_ind,c("lonF","latF")] <- NA

# feature extraction for imputation
preImp<- featureExtractPalmius(timePeriods[,-4]) 

# get imputation
interval <- 300
imputedP<- palmiusImputeLoop(preImp)

#calculate imputation distance
calcImputeDist(imputedP,remove_ind,downsampled = downsampled)


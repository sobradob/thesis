# Full Palmius Script Results
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data<- readRDS("../ownalgo/finalBinnedDataNL.rds")

# load auxiliary functions
source("../../thesis/scripts/auxFuns.R")

file.sources <- list.files(c("functions"), 
                           pattern="*.R$", full.names=TRUE, 
                           ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)

#select the month of march
data <-  fin
dataExample <- select(data,time,lat,lon,accuracy,nextMeas) %>%
  arrange(time) %>% 
  mutate( nextLon = lead(lon),
          nextLat = lead(lat),
          index = 1:length(lat),
          nextTimeSec = nextMeas
  ) %>% 
  as_tbl_time(time) %>% 
  filter_time( ~ '2017-03')

filtered<- filteringPalmius(data = dataExample) #no unique locations removed

downsampled <-  downSamplingPalmius(data = filtered) #change name of time column, lat and lon

## calculate deviance

test <- left_join(dataExample %>% thicken("5 min"),
          downsampled,
          by =c("time_5_min" = "time_5min")) %>% 
  select(lon,lat,lonF,latF,accuracy, time_5_min)

test$dist <- raster::pointDistance(as.matrix(cbind(test$lon,test$lat),ncol = 2),
                      as.matrix(cbind(test$lonF,test$latF),ncol = 2),
                      lonlat = TRUE)

summary(test$dist)

test %>%  summarise(mean(accuracy >= dist)) #each measurement
# by bin
test %>% group_by(time_5_min) %>% summarise(distMean = mean(dist),
                                            distSD   = sd(dist)) %>% summarise(mean(distMean))
library(ggplot2)
library(ggthemes)
ggplot(test, aes(x=accuracy, y=dist)) +
  geom_point()+theme_tufte()+
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0,
              na.rm = FALSE, show.legend = NA)


#there is something wierd happening with these outliers
test %>% 
  filter(dist>100000) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircles(lng = ~lon,lat = ~lat, label = ~as.character(time_5_min))%>% 
  addCircles(lng = ~lonF, lat= ~latF, color = "red",label = ~as.character(time_5_min))
# create plot
library(leaflet)

downsampled %>%
  as_tbl_time(time_5min) %>% 
  filter_time(~"2017-03-01") %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircles(lng = ~lonF, lat= ~latF)



## calculate imputation methods

d2<- downsampled %>% 
  pull(time_5min)

remove_ind <- which(as.character(d2) %in% remove_ind_5min[[2]])

remove_ind <- sample(seq_len(nrow(downsampled)), size = smp_size)

downsampled5minRemoved<- downsampled
downsampled5minRemoved[remove_ind,c("lonF","latF")] <- NA

interval <- 300
preImp<- featureExtractPalmius(downsampled5minRemoved) #extract features for imputation

imputedP<- palmiusImputeLoop(preImp) # get imputation

calcImputeDist(imputedP,remove_ind,downsampled = downsampled) #median 0, mean 43

#how many did it fail to predict?

is.na(downsampled[remove_ind,c("lonF")]) %>% sum() #276 out of the 2228 are missing
332-276 #failed to impute

imputedP %>%
  as_tbl_time(time_5min) %>% 
  filter_time(~"2017-03-07") %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircles(lng = ~lonF, lat= ~latF) %>% 
  addCircles(lng = ~impLon, lat= ~impLat, color = "red")

#its obviously modelling noise!

# test two: remove 25% of hours at random
removedHours<- remove_ind_hour[[2]]


d2<- downsampled %>% thicken("hour",colname = "hour")

remove_ind <- which(as.character(d2$hour) %in% removedHours)

d2[remove_ind,c("lonF")] %>% is.na() %>% sum()# it has 256 NA's

d2[remove_ind,c("lonF","latF")] <- NA


#do the same for only march for the current neural net. But finish Palmius hour/day first and then Barnett 
preImp<- featureExtractPalmius(d2[,-4]) #extract features for imputation
interval <- 300
imputedP<- palmiusImputeLoop(preImp) # get imputation
calcImputeDist(imputedP,remove_ind,downsampled = downsampled) #median 5, mean 677

# test three: remove 25% of days at random
removedDays<- downsampled %>% 
  thicken("day",colname = "day") %>%
  distinct(day) %>% 
  sample_frac(.25)  %>% pull() %>% as.character()


d2<- downsampled %>% thicken("day",colname = "day")
remove_ind <- which(as.character(d2$day) %in% removedDays)

d2[remove_ind,c("lonF")] %>% is.na() %>% sum()# it has 374 NA's

d2[remove_ind,c("lonF","latF")] <- NA


#do the same for only march for the current neural net. But finish Palmius hour/day first and then Barnett 
preImp<- featureExtractPalmius(d2[,-4]) #extract features for imputation
interval <- 300
imputedP<- palmiusImputeLoop(preImp) # get imputation
calcImputeDist(imputedP,remove_ind,downsampled = downsampled) #none imputed


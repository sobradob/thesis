# Master data Prep

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#load required packages
library(dplyr)
library(leaflet)

#read the data in
Sys.time()
data<- readRDS("/Users/boazsobrado/Desktop/Academic/Utrecht/year2/thesisFiles/data/boaz/myLocationHistory09012018.rds")

home<- c(5.113919,52.10421)
# filter to measurements within 300 km from home in Utrecht

test <- data %>% mutate( distanceHome = raster::pointDistance(matrix(c(lon,lat),ncol = 2),
                                                              home,
                                                              longlat = T),
                         timestampMs = as.numeric(time))%>%
  filter(distanceHome < 200000) %>% 
  select(time,lon,lat,accuracy, timestampMs,distanceHome) %>%
  arrange(timestampMs)

rm(data)

# load up custom functions
file.sources <- list.files(c("functions"), 
                           pattern="*.R$", full.names=TRUE, 
                           ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)

# this extracts pauses 
test2 <- clusterFunc(data = test,
                     timeLim = 300,
                     distLim = 50,
                     minPause = 100,
                     accuracyLim = 250)

# Extract the pause locations
toClust <- test2 %>% filter( isPause == 1  & !(distMeanEast > 2*meanAcc |
                                                 distMeanWest >2*meanAcc |
                                                 distMeanNorth >2*meanAcc |
                                                 distMeanSouth >2*meanAcc)) %>%
  select(meanLon,meanLat) %>% as.matrix()

colnames(toClust) <- c("lon","lat")

# merge all clusters within 300 meters of each other  
#clusterMap <- mergePoints(toClust, d = 300)
clusterMap1 <- mergePoints(toClust, d = 150)
clusterMap2 <- mergePoints(toClust, d = 450)

# all measurements binned into a pause cluster
test3<- binMeasurements(test,clusterMap)

# extract additional features
test3 <- featureFunc2(test3,test2)
# all points method of using rasters for computational help

pathClust<- gridFunc(test3,d = 500)

routePoints <- pathClust %>% mutate(clust = 1:nrow(pathClust))
allPoints <- routePoints %>%
  mutate(clust = clust + max(clusterMap$clust),
         type = 0) %>% 
  rbind( clusterMap %>% mutate(type = 1)) %>% 
  arrange(clust)%>%
  select(lon,lat,type) %>% 
  mergePoints2(d = 100)

pause <-   test3 %>% filter(!is.na(pauseClust)) %>% 
  binMeasurements(.,allPoints %>% filter(type == 1))

# merge non-pauses
notpause <-  test3 %>% filter(is.na(pauseClust)) %>% 
  binMeasurements(.,allPoints)

fin<- rbind(pause, notpause) %>% arrange(timestampMs)

rm(pause,notpause,routePoints,pathClust,test3,clusterMap,toClust,test2)
Sys.time()

saveRDS(fin,file = "finalBinnedDataNL.rds")
saveRDS(allPoints, file = "finalAllClusts2.rds")

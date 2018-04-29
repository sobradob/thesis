# test path & pause clustering
# this is a draft script created while coding.

library(dplyr)
library(leaflet)
library(ggplot2)

data<- readRDS("/Users/boazsobrado/Desktop/Academic/Utrecht/year2/thesisFiles/data/boaz/myLocationHistory09012018.rds")

home<- c(5.113919,52.10421)
#extract measurements within 300 km from home in Utrecht
test <- data %>% mutate( distanceHome = raster::pointDistance(matrix(c(lon,lat),ncol = 2),
                                                              home,
                                                              longlat = T),
                         timestampMs = as.numeric(time))%>%
  filter(distanceHome < 300000) %>% 
  select(time,lon,lat,accuracy, timestampMs,distanceHome) %>%
  arrange(timestampMs)

rm(data)

# analyse time spent in one area

# load up custom functions
file.sources <- list.files(c("functions"), 
                          pattern="*.R$", full.names=TRUE, 
                          ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)

# this extracts pauses 
# to do : use accuracy better e.g. take its value to modify distlim
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
# merge all clusters within 400 meters of each other  
clusterMap <- mergePoints(toClust, d = 300)

# all measurements binned into a pause cluster
test3<- binMeasurements(test,clusterMap)


# compute additional features
test3 <- featureFunc2(test3,test2)

test3 %>% filter(isPause == 1) %>% 
ggplot( aes(x = accuracy, y = distClustMin))+
  geom_point()+
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0,
              na.rm = FALSE, show.legend = NA)

# calculate percentage
test3 %>% filter(isPause == 1) %>% summarise(mean(accuracy <= distClustMin))

test3 %>%  filter(distClustMin > accuracy & isPause == 1) %>%
  leaflet() %>%
  addTiles() %>% 
  addCircles(lng = ~lon, lat = ~lat, label = ~as.character(clust)) %>% 
  addCircles(data = clusterMap, lng=~lon,lat=~lat, color = "red", label = ~as.character(clust))

routeMap <- test3 %>% mutate(timeDay = as.Date(time)) %>% 
  group_by(nextPauseClust,prevPausClust) %>% 
  dplyr::summarise(n = n(),
                   nDats = n_distinct(timeDay)) %>% 
  filter( (nextPauseClust != prevPausClust)) %>% 
  arrange(desc(n))

# to expore a single route
mergePointsRoute(data= test3, start = 44, end = 32, d = 150, maxAcc = 100) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircles(lng = ~lon, lat = ~lat)

#clusterPaths
routePoints<- extractAllPaths(test3,routeMap, d = 150, maxAcc = 150, minVisit = 1)

# try two
Sys.time()
routePoints2<- test3 %>% filter(is.na(isPause) & accuracy <=250) %>%
  select(lon,lat) %>% 
  as.matrix() %>%
  mergePoints(., d = 200)
Sys.time()



#try trhee
toClust2<- cbind(toClust,grid = as.vector(over(xy,myPointsGrid)))
allRoute <- list()
allRouteClusts <- list()
gridBins<- unique(toClust2[,3])
i <- 1
for(grid in gridBins){
  cat(paste0(Sys.time()," doing grid ", i, "\n total rows ", nrow(toClust2[which(toClust2[,3] == grid),1:2])),"\n")
  allRoute[[i]]<- mergePoints(toClust2[which(toClust2[,3] == grid),1:2], d = 200)
  i <- i+1
}
tBins<- bind_rows(allRoute)

tBins %>% filter(visits == 42) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircles(lng = ~lon, lat = ~lat, label = ~visits)

tBins %>% group_by(visits) %>% count()
# cluster path clusters

routePoints <- mergePoints( routePoints %>% select(lon,lat), d = 50)

routePoints <- tBins %>% mutate(clust = 1:nrow(tBins))
allPoints <- routePoints %>%
  mutate(clust = clust + max(clusterMap$clust),
         type = 0) %>% 
  rbind( clusterMap %>% mutate(type = 1)) %>% 
  arrange(clust)%>%
  select(lon,lat,type) %>% 
  mergePoints2(d = 100)

# split up code to make it more interpretable
# merge pauses
pause <-   test3 %>% filter(!is.na(pauseClust)) %>% 
  binMeasurements(.,allPoints %>% filter(type == 1))

# merge non-pauses
notpause <-  test3 %>% filter(is.na(pauseClust)) %>% 
  binMeasurements(.,allPoints)

fin<- rbind(pause, notpause) %>% arrange(timestampMs) 

#calculate distance from measurement and cluster to confirm its right.

temp<- left_join( fin %>%  select(clust, lon, lat, accuracy), allPoints, by = "clust")
temp$distClust <- raster::pointDistance(temp[,c("lon.x","lat.x")], temp[,c("lon.y","lat.y")], longlat = T)
#seems to beright

#all
ggplot(fin, aes(x = accuracy, y = distClustMin))+
  geom_point()+
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0,
              na.rm = FALSE, show.legend = NA)

# pauses seem ok
ggplot(fin %>% filter(isPause == 1), aes(x = accuracy, y = distClustMin))+
  geom_point()+
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0,
              na.rm = FALSE, show.legend = NA)

test3 %>% filter(isPause == 1) %>% summarise(mean(accuracy <= distClustMin))

# paths
test3 %>% filter(is.na(isPause)) %>% summarise(mean(accuracy <= distClustMin))

ggplot(fin %>% filter(is.na(isPause)), aes(x = accuracy, y = distClustMin))+
  geom_point()+
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0,
              na.rm = FALSE, show.legend = NA, color = "red")

#what are high accuracy points (sub 2500) that are far from their clusters?

#simplify more and just take nonPath clusters immediately?

fin %>% filter(distClustMin > 5000 ) %>% select(accuracy) %>% summary() # remove accuracy filter above

domain<- fin %>% filter(distClustMin > 5000 ) %>% pull(clust) %>% unique()

fin %>%  filter( distClustMin  > 5000) %>%
  leaflet() %>%
  addTiles() %>% 
  addCircles(lng = ~lon, lat = ~lat, label = ~as.character(paste0(nextPauseClust,"-",prevPausClust))) %>% 
  addCircles(data = allPoints %>% filter(clust %in% domain), lng=~lon,lat=~lat, color = "red", label = ~as.character(clust))
# analysis of results

# mostly train tracks in addition to some wierd cases where the appropriate cluster isnt selected as closest
allPoints %>%
  leaflet() %>%
  addTiles() %>% 
  addCircles(lng = ~lon, lat = ~lat, color = ~type) 
checkPath(test3, start = 44, end = 32,clusterMap)

# to do: check single pass clusters
# maybe reduce pause length requirement

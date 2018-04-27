# Distance clustering

#load required packages
library(dplyr)
library(leaflet)
#read the data in
Sys.time()
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

# to do : use accuracy better e.g. take its value to modify distlim
test2 <- clusterFunc(data = test,
                     timeLim = 200,
                     distLim = 100,
                     minPause = 120,
                     accuracyLim = 50)

# Extract the pause locations

toClust <- test2 %>% filter( isPause == 1 & duration >= 300) %>%
  select(lon,lat) %>% as.matrix()

# merge all clusters within 400 meters of each other  
clusterMap <- mergePoints(toClust, d = 400)

clusterMap %>%
  leaflet() %>%
  addTiles() %>% 
  addCircles(lng = ~lon, lat = ~lat)

# all measurements binned into a pause cluster
test3<- binMeasurements(test,clusterMap)

# compute additional features
test3 <- featureFunc(test3,timeLim = 200, distLim = 100, minPause = 120, accuracyLim = 50)

routeMap <- test3 %>% mutate(timeDay = as.Date(time)) %>% 
  group_by(nextPauseClust,prevPausClust) %>% 
  dplyr::summarise(n = n(),
            nDats = n_distinct(timeDay)) %>% 
  filter( (nextPauseClust != prevPausClust)) %>% 
  arrange(desc(n))

checkPath(test3, start = 44, end = 32,clusterMap)

mergePointsRoute(data= test3, start = 44, end = 32, d = 150, maxAcc = 100) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircles(lng = ~lon, lat = ~lat)

#clusterPaths
routePoints<- extractAllPaths(test3,routeMap)

# cluster path clusters

routePoints <- mergePoints( routePoints %>% select(lon,lat), d = 50)


allPoints <- routePoints %>%
  mutate(clust = clust + max(clusterMap$clust),
         type = 0) %>% 
  rbind( clusterMap %>% mutate(type = 1)) %>% 
  arrange(clust)%>%
  select(lon,lat,type) %>% 
  mergePoints2(d = 100)

# split up code to make it more interpretable
# merge pauses
pause <-   test3 %>% filter(isPause == 1) %>% 
  binMeasurements(.,allPoints %>% filter(type == 1))
# merge non-pauses

notpause <-  test3 %>% filter(isPause == 0) %>% 
  binMeasurements(.,allPoints)

fin<- rbind(pause, notpause) %>% arrange(timestampMs)


Sys.time()
# check out top clusters classified
top100<- fin %>% select(clust) %>%group_by(clust) %>%  tally() %>% arrange(desc(n)) %>% top_n(25) %>% pull(clust)

pal <- colorFactor(c("navy", "red"), domain = c(0, 1))

allPoints %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(lng =~lon, lat = ~lat, color = ~pal(type),radius = ~ifelse(type == 0, 6, 10), label = ~as.character(clust))

  library(leaflet.extras)
  routePoints%>% 
    leaflet() %>% 
    addTiles() %>%
    addHeatmap(lng = ~lon, lat = ~lat, intensity = ~visits)
  
  # SINGLE PATH
  start <- 32
  end <- 44
  tripRaw <- fin %>% 
    filter((nextPauseClust == start &  prevPausClust == end)|
             (nextPauseClust == end &  prevPausClust == start))
  
  trip <- allPoints %>% filter( clust %in%intersect(allPoints$clust,tripRaw$clust))
  
  
  ggplot(tripRaw, aes(x=lon, y=lat, color=factor(clust))) +
    geom_point(shape=1,alpha = 0.4) +
    geom_point(data = trip, aes( x = lon, y= lat)) +theme_bw()+ coord_fixed() + theme(legend.position="none")
  
  ggplot(fin, aes(x = accuracy, y = distClustMin))+
    geom_point()+
    geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0,
                            na.rm = FALSE, show.legend = NA)
  
  fin %>% filter(isPause == 0) %>% 
    summarise(withinA = mean(accuracy <= distClustMin))

# explore the relationship between accuracy and 
fin %>% filter(distClustMin > 20000) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircles(lng = ~lon, lat = ~lat)
  
fin %>% filter(distClustMin < 20000 & accuracy < 5000) %>%
ggplot(aes(x = accuracy, y = distClustMin))+
  geom_point()

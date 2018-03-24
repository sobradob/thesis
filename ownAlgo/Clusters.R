# Distance clustering

#s
library(dplyr)

data<- readRDS("/Users/boazsobrado/Desktop/Academic/Utrecht/year2/thesisFiles/data/boaz/myLocationHistory09012018.rds")

home<- c(5.113919,52.10421)
test <- data %>% mutate( distanceHome = raster::pointDistance(matrix(c(lon,lat),ncol = 2),
                                                              home,
                                                              longlat = T),
                         timestampMs = as.numeric(time))%>%
  filter(distanceHome < 300000) %>% 
  select(time,lon,lat,accuracy, timestampMs,distanceHome) %>%
  arrange(timestampMs)

rm(data)

timeLim <- 600
distLim <- 100
minPause <- 60
accuracyLim <-25

test2 <- test %>% filter(accuracy <accuracyLim) %>% 
  mutate( prevLon = lag(lon),
                   prevLat = lag(lat),
                   nextLon = lead(lon),
                   nextLat = lead(lat),
                   nextDist = raster::pointDistance(matrix(c(lon,lat),ncol = 2),
                                                    matrix(c(nextLon,nextLat),ncol = 2),
                                                    longlat = T),
                   prevDist = raster::pointDistance(matrix(c(lon,lat),ncol = 2),
                                                    matrix(c(prevLon,prevLat),ncol = 2),
                                                    longlat = T),
                   nextMeas = as.numeric(timestampMs-lead(timestampMs)),
                   prevMeas = as.numeric(lag(timestampMs)-timestampMs),
                   isPause = case_when(prevMeas <= timeLim &nextMeas <= timeLim & nextDist <= distLim~1,
                                       TRUE ~0)
) %>%
  group_by(run = {run = rle(isPause); rep(seq_along(run$lengths), run$lengths)}) %>%
  summarize(isPause = mean(isPause),
            lon = mean(lon),
            lat = mean(lat),
            t0  = min(timestampMs),
            t1  = max(timestampMs),
            duration = t1-t0)
  
#perhaps take weighted mean
# incorporate minimum missing
# filter accuracies

# missing group and name

toClust <- test2 %>% filter( isPause == 1 & duration > 600) %>%
  select(lon,lat) %>% as.matrix()
  
library(sp)
library(rgdal)
library(geosphere)

# convert data to a SpatialPointsDataFrame object
xy <- SpatialPointsDataFrame(
  toClust, data.frame(ID=seq(1:nrow(toClust))),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

rm(toClust)
# use the distm function to generate a geodesic distance matrix in meters
mdist <- distm(xy)

# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(mdist), method="complete")

# define the distance threshold, in this case 50 m
d=200

# define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
xy$clust <- cutree(hc, h=d)

xy <- cbind(xy@data,xy@coords)

xy %>% as.data.frame() %>% 
  group_by(clust) %>% 
  summarise(lon = mean(lon),
            lat = mean(lat),
            n   = n()) %>% 
  leaflet() %>% 
  addTiles() %>%
  addCircles(lng = ~lon, lat = ~lat,
             radius = 50, fillOpacity = 0.02,color = "#18206F",label = ~as.character(n))

# save
saveRDS(test2,"clusters.rds")
rm(mdist)

test2 %>% filter( isPause == 1 & duration > 6000) %>% 
  leaflet() %>% 
  addTiles() %>%
  addCircles(lng = ~lon, lat = ~lat,
             radius = 30, fillOpacity = 0.02,color = "#18206F",label = ~as.character(duration))

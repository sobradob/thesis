# Distance clustering

#load required packages
library(dplyr)

#read the data in
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

test2 <- clusterFunc(data = test,
                     timeLim = 300,
                     distLim = 100,
                     minPause = 60,
                     accuracyLim = 50)

# Extract the pause locations

toClust <- test2 %>% filter( isPause == 1 & duration >= 300) %>%
  select(lon,lat) %>% as.matrix()

# merge all clusters within 400 meters of each other  
clusterMap <- mergePoints(toClust, d = 400)

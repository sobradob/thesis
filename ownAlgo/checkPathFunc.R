checkPath <- function( data2 = data, start = 14, end = 3,clustMap = clustMap){
  library(leaflet)
  library(dplyr)
  data2 %>%
    filter(nextPauseClust == end & prevPausClust == start) %>% 
    leaflet() %>% 
    addTiles() %>% 
    addCircles(lng = ~lon, lat = ~lat, fillOpacity = 0.02,color = "#18206F",label = ~as.character(clust)) %>% 
    addCircles(data = filter(clustMap, clust ==end),lng = ~lon, lat = ~lat, color = "red", radius = 100) %>% 
    addCircles(data = filter(clustMap, clust ==start),lng = ~lon, lat = ~lat, color = "green", radius = 100)
}

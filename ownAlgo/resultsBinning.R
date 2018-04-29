# results for evalutating binning
# read in allpoints and end results of binning if not already loaded

library(ggplot2)
library(ggthemes)

# show clustering parameter effect
leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles( data = clusterMap1, lng = ~lon, lat = ~lat, color = "#18206F") %>% 
  addCircles( data = clusterMap2, lng = ~lon, lat = ~lat, color = "#DF2935")
  
addCircles(lng = ~jitter(lon.measured), lat = ~jitter(lat.measured), color = "#18206F") %>% 
  addCircles(lng = ~lon.cluster,lat = ~lat.cluster, color = "#DF2935")
# calculate percentage within accuracy
fin %>%
  summarise(mean(accuracy >= distClustMin))

# calculate minimum deviance
fin %>%
  select(distClustMin) %>% summary(d)

#generate plots
fin %>% 
  ggplot(aes(x = accuracy, y = distClustMin))+
  geom_point()+
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0,
              na.rm = FALSE, show.legend = NA, colour = "grey")+
  theme_tufte()+
  ggtitle("Deviance vs Accuracy",subtitle = "Within 300km Utrecht")+
  xlab("Accuracy")+ylab("Deviance")

# calculate percentage March
fin %>%
  as_tbl_time(time) %>% 
  filter_time(~"2017-03") %>% 
  summarise(mean(accuracy >= distClustMin))

# calculate deviance March
fin %>%
  as_tbl_time(time) %>% 
  filter_time(~"2017-03") %>% 
  select(distClustMin) %>% summary(d)

fin %>%
  as_tbl_time(time) %>% 
  filter_time(~"2017-03") %>% 
  ggplot(aes(x = accuracy, y = distClustMin))+
  geom_point()+
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0,
              na.rm = FALSE, show.legend = NA, colour = "grey")+
  theme_tufte()+
  ggtitle("Deviance vs Accuracy",subtitle = "March 2017")+
  xlab("Accuracy")+ylab("Deviance")

# Create a plot compared to Palmius
plotBin<- left_join(fin %>% as_tbl_time(time) %>% filter_time(~"2017-03-02"),allPoints,by="clust", suffix=c(".measured",".cluster"))

plotBin <- left_join(plotBin %>% thicken("5 min"),
          downsampled,
          by =c("time_5_min" = "time_5min"))

plotBin %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircles(lng = ~jitter(lon.measured), lat = ~jitter(lat.measured), color = "#18206F") %>% 
  addCircles(lng = ~lon.cluster,lat = ~lat.cluster, color = "#DF2935")
  
addCircles(lng = ~lonF,lat = ~latF, color = "yellow",label = ~as.character(time_5_min)) %>% 

# ggplot with lines

ggplot(plotBin, aes(x = lon.cluster, y = lat.cluster))+
  geom_path()+
  geom_path(aes(x = lonF, y = latF, colour = "red"))+
  coord_fixed()+
  theme_tufte()+
  xlab("Longitude")+
  ylab("Latitude")+
  geom_point(aes(x = lon.measured, y = lat.measured), shape = 1, colour = "blue")+
  theme(legend.position="bottom ") +
  geom_point( data = filter(plotBin, accuracy >800), aes(x = lon.measured, y = lat.measured,color = "green"))+
  ggtitle("Binning Method Comparison",subtitle = "01-03-2017")


# check weighted deviance

fin %>%
  as_tbl_time(time) %>% 
  filter_time(~"2017-03") %>% 
  thicken("5 min", "thickened") %>% 
  group_by(thickened) %>% 
  summarise(mDev = mean(distClustMin)) %>% 
  summarise(mmDev = mean(mDev),
            medDev = median(mDev))

# amsterdam - utrecht train

plotBin<- left_join(fin %>% as_tbl_time(time) %>% filter_time(~"2017-03"),allPoints,by="clust", suffix=c(".measured",".cluster"))

plotBin <- left_join(plotBin %>% thicken("5 min"),
                     downsampled,
                     by =c("time_5_min" = "time_5min"))

leaflet(width=500,height=400)



m<- plotBin %>% 
  leaflet(width=500,height=400) %>%
  setView(lng = 5.003981, lat = 52.24854, zoom = 12) %>% 
  addTiles() %>% 
  addCircles(lng = ~jitter(lon.measured), lat = ~jitter(lat.measured), color = "#18206F") %>% 
  addCircles(lng = ~lon.cluster,lat = ~lat.cluster, color = "#DF2935") %>% 
  addCircles(lng = ~lonF,lat = ~latF, color = "yellow",label = ~as.character(time_5_min))


getBox <- function(m){
  view <- m$x$setView
  lat <- view[[1]][1]
  lng <- view[[1]][2]
  zoom <- view[[2]]
  zoom_width <- 360 / 2^zoom
  lng_width <- m$width / 256 * zoom_width
  lat_height <- m$height / 256 * zoom_width
  return(c(lng - lng_width/2, lng + lng_width/2, lat - lat_height/2, lat + lat_height/2))
}
getBox(m)


dataExample<- select(fin,time,lat,lon,accuracy,nextMeas) %>%
  arrange(time) %>% 
  mutate( nextLon = lead(lon),
          nextLat = lead(lat),
          index = 1:length(lat),
          nextTimeSec = nextMeas
  )

filteredAll<- filteringPalmius(data = dataExample) #no unique locations removed

downsampledPAll <-  downSamplingPalmius(data = filteredAll) #change name of time column, lat and lon

test <- left_join(dataExample %>% thicken("5 min"),
                  downsampledPAll,
                  by =c("time_5_min" = "time_5min")) %>% 
  select(lon,lat,lonF,latF,accuracy, time_5_min)

test$dist <- raster::pointDistance(as.matrix(cbind(test$lon,test$lat),ncol = 2),
                                   as.matrix(cbind(test$lonF,test$latF),ncol = 2),
                                   lonlat = TRUE)
test %>% filter(dist > 1000)

plotBin <- fin %>% 
  filter(lon > 4.918150 & lon < 5.089812 & lat > 52.179875 & lat < 52.317205) %>%
  left_join(.,allPoints,by="clust",suffix=c(".measured",".cluster")) %>% 
  thicken("5 min") %>% 
  left_join(.,downsampled,
          by =c("time_5_min" = "time_5min"))


plotBin <- fin %>% 
  filter(lon > 5.486984 & lon < 6.475754 & lat > 52.009401 & lat < 52.362603) %>%
  left_join(.,allPoints,by="clust",suffix=c(".measured",".cluster")) %>% 
  thicken("5 min") %>% 
  left_join(.,downsampled,
            by =c("time_5_min" = "time_5min"))
  
plotBin %>% 
  leaflet() %>%
  addTiles() %>% 
  addCircles(lng = ~lon.measured, lat = ~lat.measured, color = "#18206F") %>% 
  addCircles(lng = ~lon.cluster,lat = ~lat.cluster, color = "#DF2935") %>% 
  addCircles(lng = ~lonF,lat = ~latF, color = "yellow",label = ~as.character(time_5_min))
  
plotBin <- fin %>% 
  left_join(.,allPoints,by="clust",suffix=c(".measured",".cluster")) %>% 
  thicken("5 min") %>% 
  left_join(.,downsampled,
            by =c("time_5_min" = "time_5min"))

plotBin$pDist <- raster::pointDistance(as.matrix(cbind(plotBin$lon.measured,plotBin$lat.measured),ncol = 2),
                                       as.matrix(cbind(plotBin$lonF,plotBin$latF),ncol = 2),
                                       lonlat = TRUE)

plotBin %>%
  filter(pDist > 800) %>%
  leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(lng = ~lon.measured, lat = ~lat.measured, color = "#18206F") %>% 
  addCircles(lng = ~lon.cluster,lat = ~lat.cluster, color = "#DF2935") %>% 
  addCircles(lng = ~lonF,lat = ~latF, color = "orange",label = ~as.character(time_5_min))

plotBin %>%
  filter(distClustMin > 1000) %>% 
  leaflet() %>%
  addTiles() %>% 
  addCircles(lng = ~lon.measured, lat = ~lat.measured, color = "#18206F") %>% 
  addCircles(lng = ~lon.cluster,lat = ~lat.cluster, color = "#DF2935") %>% 
  addCircles(lng = ~lonF,lat = ~latF, color = "yellow",label = ~as.character(time_5_min))


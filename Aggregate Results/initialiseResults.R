# Example result: time spent at home

# initialise values

library(leaflet)

# extract the cluster which counts as home
leaflet(allPoints) %>% 
  addTiles() %>% 
  addCircles(lng = ~lon, lat = ~lat, label = ~as.character(clust))

# hard code home coordinates
home <- c(5.114051,52.10415)

# generate test data frame
test <- fin %>% 
  as_tbl_time(index = time) %>%
  filter_time(~ "2017-03") %>% 
  select(time, clust, lon, lat) %>% 
  thicken("5 min", "thick") %>% 
  left_join(., allPoints, by = "clust", suffix = c(".measured",".clust")) %>% 
  mutate( distHClust = raster::pointDistance(p1 = home, p2 = as.matrix(cbind(lon.clust,lat.clust)), lonlat = T))

# Check which clusters are within 250 meters of home
test %>%
  filter(distH<250) %>% 
leaflet() %>% 
  addTiles() %>% 
  addCircles(lng = ~lon.clust, lat = ~lat.clust, label = ~as.character(clust))

# results: clusts 49, 348 and 336

# create secondary data frame with distance between measured and hard coded home
test2 <- test %>% group_by(thick) %>% 
  summarise(lon  = mean(lon.measured),
            lat  = mean(lat.measured),
            clust = Mode(clust),
            home1 = clust %in% c(49,348,336),
            home2 = raster::pointDistance(p1 = home, p2 = as.matrix(cbind(lon,lat)), lonlat = T))

# calculate percentage at home without imputations
test2 %>% pad() %>% summarise(atHomeProp = sum(home1, na.rm = T)/nrow(.),
                              missingProp = sum(is.na(home1))/nrow(.))

# Now run the PPMI, Palmis & Barnett & Onella imputations.


# Plot results

plotAgg <- data.frame(
  home = c(0.536,0.625,.604,.625),
  missing = c(0.126,0.00729,.00971,0),
  method = c("Raw","Palmius","Barnett & Onnela", "PMMI")
)

plotAgg$nothome <-  1-(plotAgg$home+plotAgg$missing)

positions <- c("Raw","Barnett & Onnela", "Palmius", "PMMI")

plotAgg %>%
  select(method,home,missing,nothome) %>%
  gather(type,value,home:nothome) %>% 
  ggplot( aes( x= method, y = value, fill = type))+
  geom_bar(stat = "identity")+
  theme_tufte()+ylab("Proportion of time")+
  ggtitle("Time spent at home",subtitle = "March 2017")+
  scale_fill_manual(values=c("black",muted("red"), "grey50"),labels=c("At Home", "Missing","Not Home"))+
  labs(x = "Method", fill = "")+ scale_x_discrete(limits = positions)

# results: aggregation time spent at home

leaflet(allPoints) %>% 
  addTiles() %>% 
  addCircles(lng = ~lon, lat = ~lat, label = ~as.character(clust))

home <- c(5.114051,52.10415)

test <- fin %>% 
  as_tbl_time(index = time) %>%
  filter_time(~ "2017-03") %>% 
  select(time, clust, lon, lat) %>% 
  thicken("5 min", "thick") %>% 
  left_join(., allPoints, by = "clust", suffix = c(".measured",".clust")) %>% 
  mutate( distHClust = raster::pointDistance(p1 = home, p2 = as.matrix(cbind(lon.clust,lat.clust)), lonlat = T))

library(ggplot2)
plot(test %>%as_tbl_time(time) %>% 
       filter_time(~"2017-03-07") %>% pull(distH))

test %>%as_tbl_time(time) %>% 
  filter_time(~"2017-03-07") %>%
  filter(distH<400) %>% 
leaflet() %>% 
  addTiles() %>% 
  addCircles(lng = ~lon.clust, lat = ~lat.clust, label = ~as.character(clust))

#clusts 49, 348 and 336

test2 <- test %>% group_by(thick) %>% 
  summarise(lon  = mean(lon.measured),
            lat  = mean(lat.measured),
            clust = Mode(clust),
            home1 = clust %in% c(49,348,336),
            home2 = raster::pointDistance(p1 = home, p2 = as.matrix(cbind(lon,lat)), lonlat = T))

test2 %>%  group_by(home1) %>% summarise(m = min(home2), mean = mean(home2), max = max(home2))  

test2 %>%  filter ( home1 == T) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircles(lng = ~lon, lat = ~lat)


test2 %>% pad() %>% summarise(atHomeProp = sum(home1, na.rm = T)/nrow(.),
                              missingProp = sum(is.na(home1))/nrow(.))

# predicted values
t <- test2 %>%  pad()
t[which(t$thick %in% rems),"clust"] <- as.integer(model %>% predict_classes(pred))

# model prediction
t %>% mutate( home1 = clust %in% c(49,348,336)) %>% summarise(atHomeProp = sum(home1)/nrow(.))

#barnett & onella

data <- fin %>% select(timestampMs,time,lat,lon,accuracy) %>%
  as_tbl_time(time) %>%
  filter_time(~"2017-03")

downsampledIan <- downSampleMean(data,interval = '300 sec') #downsamples and adds XY coordinates
mobmatmiss<- pauseFlight(downsampledIan, r = sqrt(300), w = mean(data$accuracy)) # extracts pauses and flights using algo
Sys.time()
mobmatmiss <- mobmatmiss[-nrow(mobmatmiss),] #clean a bit

# Guessing Pauses
Sys.time()
mobmat = GuessPause(mobmatmiss,mindur=minpausedur,r=minpausedist)
Sys.time()
obj=InitializeParams(mobmat)
spread_pars=c(10,1)
wtype <- "TL"
Sys.time()
out3 <- SimulateMobilityGaps(mobmat,obj,wtype,spread_pars) # it is returning NA's only for

evalDf<- evalIan(out3,downsampledIan = downsampledIan)

homeIan <- c(26042,2828)
evalDf %>% 
  mutate(
  distHian= raster::pointDistance(p1 = homeIan, p2 = as.matrix(cbind(x,y)), lonlat = F),
  home = case_when( distHian < 250 ~ 1,
                    distHian >= 250 ~ 0)
  ) %>% 
  summarise(m = mean(home, na.rm = T),
            na = sum(is.na(home))/nrow(evalDf))

#Palmius

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
interval <- 300
preImp<- featureExtractPalmius(downsampled) #extract features for imputation

imputedP<- palmiusImputeLoop(preImp) # get imputation

imputedP %>% mutate(
  home1 = raster::pointDistance(p1 = home, p2 = as.matrix(cbind(impLon,impLat)), lonlat = T),
  home2 = raster::pointDistance(p1 = home, p2 = as.matrix(cbind(lonF,latF)), lonlat = T),
  home = case_when( home2 < 250 |home1 <250 ~ 1,
                    home2 >= 250|home1 >=250 ~ 0)
) %>% 
  summarise(m = mean(home, na.rm = T),
            na = sum(is.na(home))/nrow(.))

# missing: create plot

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

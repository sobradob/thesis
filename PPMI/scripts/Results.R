# Results bit:

#Sampling frequency in herz for march

fin %>%as_tbl_time(time) %>%
  filter_time(~"2017-03") %>% 
  thicken("day") %>% 
  group_by(time_day) %>% 
  summarise(total = n()/(24*60*60)) %>%
  pull(total) %>% hist()

# baseline models

# MODEL: always previous

## accuracy of the entire log:
fin %>% mutate(lagClust = lag(clust)) %>%
  summarise( prev = mean(clust == lagClust, na.rm= T))

## mean deviation distance
fin %>% mutate(lagClust = lag(clust)) %>% select(clust, lagClust) %>% 
  left_join(allPoints,by="clust") %>% 
  left_join(allPoints,by=c("lagClust" = "clust"),suffix = c(".clust",".predC")) %>% 
  na.omit() %>% 
  filter(clust != lagClust) %>% 
  mutate( dist = raster::pointDistance(matrix(c(lon.clust,lat.clust),ncol = 2),
                                                matrix(c(lon.predC,lat.predC),ncol = 2),
                                                longlat = T)) %>% 
  summarise(distMedian = median(dist),
            distMean = mean(dist))

# model: dailyMode

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

dailyMode<- fin %>%
  thicken('day') %>%
  group_by(time_day) %>% 
  summarise(modeClust = Mode(clust))

left_join(fin %>% thicken("day"),dailyMode, by="time_day") %>% 
  select(clust,modeClust) %>% 
  summarise( prev = mean(clust == modeClust, na.rm= T))

left_join(fin %>% thicken("day"),dailyMode, by="time_day") %>% 
  select(clust,modeClust) %>% 
  left_join(allPoints,by="clust") %>% 
  left_join(allPoints,by=c("modeClust" = "clust"),suffix = c(".clust",".predC")) %>% 
  na.omit() %>% 
  filter(clust != modeClust) %>% 
  mutate( dist = raster::pointDistance(matrix(c(lon.clust,lat.clust),ncol = 2),
                                       matrix(c(lon.predC,lat.predC),ncol = 2),
                                       longlat = T)) %>% 
  summarise(distMedian = median(dist),
            distMean = mean(dist))


# Check results
t<- test %>% mutate(
  predC = as.integer(model %>% predict_classes(as.matrix(x_test)))
) %>% as_tbl_time(time)

# accuracy
t %>% summarise( nn =  mean(clust == predC))

t  %>% select(clust, predC) %>% filter(clust != predC) %>%
  left_join(allPoints,by="clust") %>% 
  left_join(allPoints,by=c("predC" = "clust"),suffix = c(".clust",".predC")) %>% 
  na.omit() %>% 
  mutate( dist = raster::pointDistance(matrix(c(lon.clust,lat.clust),ncol = 2),
                                       matrix(c(lon.predC,lat.predC),ncol = 2),
                                       longlat = T)) %>% 
  summarise(distMedian = median(dist),
            distMean = mean(dist))

#89%, but prev and next are 91.2%

# problem: it isn't learning the autocorrelation enough
# it is guessing out of range
# solutions: add area? 
# add previous two/next two? doesn't work
# use self defined loss function based on distance
# add distance of all clusters from previous cluster?
# change the architecture of the neural network

t %>% filter( clust != predC) %>%
  sample_n(1) %>%
  select(clust,predC,lagClust,leadClust) %>%
  left_join(allPoints,by="clust") %>% 
  left_join(allPoints,by=c("predC" = "clust"),suffix = c(".clust",".predC")) %>% 
  left_join(allPoints,by=c("leadClust" = "clust")) %>%
  left_join(allPoints,by=c("lagClust" = "clust"),suffix = c(".lead",".lag")) %>% 
  leaflet() %>% 
  addTiles() %>%
  addCircles(lng = ~lon.clust,lat = ~lat.clust, color = "green", radius = 100, label = ~as.character(clust),group = "Actual") %>% 
  addCircles(lng = ~lon.predC,lat = ~lat.predC, color = "red", radius = 100, label = ~as.character(predC),group = "Pred") %>% 
  addCircles(lng = ~lon.lead,lat = ~lat.lead, color = "yellow", radius = 100, label = ~as.character(leadClust),group = "Next") %>%
  addCircles(lng = ~lon.lag,lat = ~lat.lag, color = "blue", radius = 100, label = ~as.character(lagClust), group = "Prev") %>% 
  addLayersControl(
    overlayGroups = c("Actual", "Pred","Next","Prev"),
    options = layersControlOptions(collapsed = FALSE)
  )


# randomly extracted 5 minute bits.
# aggregate measures for remaining data

# distance travelled: how does it work?

# barnett & onella will wokr better for aggregate measures due to change in Lon/Lat

#keep in mind the lugtig distance from measurement stuff 

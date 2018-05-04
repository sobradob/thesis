
# Get results from the Palmius model

# create remove index
removedPeriod<- objLog %>% thicken("5 min", "thick5") %>% pull(thick5) %>% as.numeric()
remove_ind <- which(downsampledIan$timestampMs %in% removedPeriod)

# filter down to march
dataExample <- select(fin,time,lat,lon,accuracy,nextMeas) %>%
  arrange(time) %>% 
  mutate( nextLon = lead(lon),
          nextLat = lead(lat),
          index = 1:length(lat),
          nextTimeSec = nextMeas
  ) %>% 
  as_tbl_time(time) %>% 
  filter_time( ~ '2017-03')

# filter the data as Palmius does
filtered<- filteringPalmius(data = dataExample)]

# downsample data as Palmius does
downsampled <-  downSamplingPalmius(data = filtered)

# remove values which are in the removed period
downsampledObjPalmius<- downsampled
downsampledObjPalmius[remove_ind,c("lonF","latF")] <- NA

# set parameters and extract features for imputation
interval <- 300
preImp<- featureExtractPalmius(downsampledObjPalmius) 

# get imputation
imputedP<- palmiusImputeLoop(preImp)

# calculate imputed distance for available values
calcImputeDist(imputedP,remove_ind,downsampled = downsampled)

# calculate true distances
imputedP %>% slice(remove_ind) %>%
  select(timestampMs,impLon,impLat) %>%
  left_join(., objLog %>%thicken("5 min", "thick5") %>% mutate(timestampMs = as.numeric(thick5)), by = "timestampMs") %>% 
  mutate( dist = raster::pointDistance(p1 = as.matrix(cbind(impLon,impLat)), p2 = as.matrix(cbind(lon,lat)),lonlat = T )) %>% 
  summary()
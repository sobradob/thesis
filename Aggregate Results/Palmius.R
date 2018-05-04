
#Imputing time spent at home using Palmius' method

# get the month of march
dataExample <- select(fin,time,lat,lon,accuracy,nextMeas) %>%
  arrange(time) %>% 
  mutate( nextLon = lead(lon),
          nextLat = lead(lat),
          index = 1:length(lat),
          nextTimeSec = nextMeas
  ) %>% 
  as_tbl_time(time) %>% 
  filter_time( ~ '2017-03')

# filter based on Palmius' method
filtered<- filteringPalmius(data = dataExample) 

# downsample based on Palmius' method
downsampled <-  downSamplingPalmius(data = filtered)

# set interval to 5 minutes/300 seconds
interval <- 300

# extract features for imputation
preImp<- featureExtractPalmius(downsampled) 
# get imputation

imputedP<- palmiusImputeLoop(preImp) 

# Calculate time at home
imputedP %>% mutate(
  home1 = raster::pointDistance(p1 = home, p2 = as.matrix(cbind(impLon,impLat)), lonlat = T),
  home2 = raster::pointDistance(p1 = home, p2 = as.matrix(cbind(lonF,latF)), lonlat = T),
  home = case_when( home2 < 250 |home1 <250 ~ 1,
                    home2 >= 250|home1 >=250 ~ 0)
) %>% 
  summarise(m = mean(home, na.rm = T),
            na = sum(is.na(home))/nrow(.))
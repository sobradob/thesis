# get results from Barnett & Onella log

# create remove index
removedPeriod<- objLog %>% thicken("5 min", "thick5") %>% pull(thick5) %>% as.numeric()
remove_ind <- which(downsampledIan$timestampMs %in% removedPeriod)

# remove values
downsampledObj<- downsampledIan
downsampledObj[remove_ind,c("lon","lat","x_v","y_v")] <- NA
downsampledObj[remove_ind,c("code")] <- 4

# extract pauses and flights
mobmatmiss <- pauseFlight(downsampledObj, r = sqrt(300), w = mean(data$accuracy)) 

# remove erroneous last value
mobmatmiss <- mobmatmiss[-nrow(mobmatmiss),]

# guess pause locations
mobmat <- GuessPause(mobmatmiss,mindur=minpausedur,r=minpausedist)

# initalise parameters for imputation
obj <- InitializeParams(mobmat)
spread_pars <- c(10,1)
wtype <- "TL"

# impute mobility matrix
out3 <- SimulateMobilityGaps(mobmat,obj,wtype,spread_pars)
evalDf<- evalIan(out3,downsampledIan = downsampledIan)

# get distance matrix for available periods
evalDf %>%
  filter( timestampMs %in% (downsampledIan[remove_ind,"timestampMs"] %>% 
                              pull())) %>% 
  unique() %>%
  summary()

#get XY Coordinates for the missing spots to compute distances
temp<- rbind(objLog %>% select(lon,lat), data %>% select(lon,lat))
temp <- cbind(objLog,as.data.frame(LatLong2XY(temp$lat, temp$lon))[1:nrow(objLog),])
temp<- temp %>% thicken("5 min", "thick5") %>% mutate(timestampMs = as.numeric(thick5))

# get distances for unavailable periods
eval <- left_join(evalDf[remove_ind,], temp, by = "timestampMs")%>%
  mutate( distObj = raster::pointDistance(p1 = as.matrix(cbind(x_v.y,y_v.y)), p2 = as.matrix(cbind(x,y)),lonlat = F ))

# show results
summary(eval)

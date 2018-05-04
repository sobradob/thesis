# barnett & onella's missing data imputation for the month of march 2017

# get the march data
data <- fin %>% select(timestampMs,time,lat,lon,accuracy) %>%
  as_tbl_time(time) %>%
  filter_time(~"2017-03")

#downsamples and add XY coordinates
downsampledIan <- downSampleMean(data,interval = '300 sec')

# extracts pauses and flights using algo
mobmatmiss<- pauseFlight(downsampledIan, r = sqrt(300), w = mean(data$accuracy)) 

#clean last row with erroneous value
mobmatmiss <- mobmatmiss[-nrow(mobmatmiss),] 

# Guess Pauses
mobmat = GuessPause(mobmatmiss,mindur=minpausedur,r=minpausedist)

#initialise parameters
obj=InitializeParams(mobmat)
spread_pars=c(10,1)
wtype <- "TL"

# impute missing values
out3 <- SimulateMobilityGaps(mobmat,obj,wtype,spread_pars)

# get evaluation data frame
evalDf<- evalIan(out3,downsampledIan = downsampledIan)

# hard code home coordinates based on XY coordinate system
homeIan <- c(26042,2828)

# get proportion of time at home
evalDf %>% 
  mutate(
    distHian= raster::pointDistance(p1 = homeIan, p2 = as.matrix(cbind(x,y)), lonlat = F),
    home = case_when( distHian < 250 ~ 1,
                      distHian >= 250 ~ 0)
  ) %>% 
  summarise(m = mean(home, na.rm = T),
            na = sum(is.na(home))/nrow(evalDf))
# Downsample Function
# this function takes dataframe with google location data and downsamples it using desired methods and intervals
# in addition adding X Y coordinates

downSampleMean <- 
  function(data,interval = '5 min'){
    
    library(dplyr)
    library(tibbletime)
    library(padr)
    
    # first summarise via interval
    data1 <- data %>%
      select(timestampMs,time,lat,lon,accuracy) %>% 
      as_tbl_time(time) %>% 
      arrange(time) %>% 
      mutate(timestampMs = as.numeric(timestampMs)) %>% 
      thicken(interval = interval, colname = 'time2') %>%
      group_by(time2) %>%
      summarise(lon = mean(lon),# summarising function
                lat = mean(lat),
                code = 1)
    
    # then add XY coords
    data2<- cbind(data1,as.data.frame(LatLong2XY(data1$lat, data1$lon))) %>% 
      pad() %>%
      mutate(code = case_when(is.na(x_v) == T ~4,
                              is.na(x_v) == F ~1),
             timestampMs = as.numeric(time2)) %>% 
      select(code,timestampMs,lon,lat,x_v,y_v)
    return(data2)
  }


# Downsample Function
# this function takes dataframe with google location data and downsamples it using desired methods and intervals
# in addition adding X Y coordinates

downSampleMean <- 
  function(data,interval = '5 min'){
    
    library(dplyr)
    library(tibbletime)
    library(padr)
    
    #  summarise via interval and add coordinates
    data1 <- cbind(data,as.data.frame(LatLong2XY(data$lat, data$lon)))%>%
      select(timestampMs,time,lat,lon,accuracy,x_v,y_v) %>% 
      as_tbl_time(time) %>% 
      arrange(time) %>% 
      mutate(timestampMs = as.numeric(timestampMs)) %>% 
      thicken(interval = interval, colname = 'time2') %>%
      group_by(time2) %>%
      summarise(lon = mean(lon),# summarising function
                lat = mean(lat),
                x_v = mean(x_v),
                y_v = mean(y_v),
                code = 1) %>% 
      pad() %>%
      mutate(code = case_when(is.na(x_v) == T ~4,
                              is.na(x_v) == F ~1),
             timestampMs = as.numeric(time2)) %>% 
      select(code,timestampMs,lon,lat,x_v,y_v)
    
    
    return(data1)
  }


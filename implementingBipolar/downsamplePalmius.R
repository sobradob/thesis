# Function to downsampale

downSamplingPalmius <- function(data, sd = 14.14){
  
  library(dplyr)
  library(tibbletime)
  library(padr)
  
  #sd value comes from sqrt(10^2+10^2)

  # takes raw data, calculates the mean hourly latitude and longitude
  # and the standard deviation in distance within the hour
  # pads and filters out those which have a higher standard deviation
  # d3 then has hourly data with mean latitude and longitude
  
  d3<- data %>% thicken('hour') %>% 
    group_by(time_hour) %>%
    summarise(distancesMeterSd = sd(distancesMeter , na.rm = T),
              meanLat = mean(lat),
              meanLon = mean(lon)) %>% 
    pad() %>% 
    fill_by_value( "NA") %>%
    filter(distancesMeterSd < sd) %>%
    select(time_hour,meanLat,meanLon)
  
  # takes the raw data, thickens it by hour then joins d3 containing mean hourly lattitude and longitude
  # then it mutates it so that lonF1 is the mean longitude when the mean longitude is not missing
  dF<- data %>% thicken('hour') %>%
    left_join(d3) %>%
    mutate(
      lonF1 = case_when(
        is.na(meanLon) == F ~ meanLon
      ),
      latF1 = case_when(
        is.na(meanLat) == F ~ meanLat
      )
    )
  
  # thickens by five minutes and takes the median longitude and lattitude
  dF2<- dF %>% thicken(by = "time","5 min",colname= "time_5min") %>% 
    group_by(time_5min) %>% 
    summarise(lonF2 = median(lon),
              latF2 = median(lat)
    )
  # thickens by five minutes and adds the two filtered types of data together
  d.filt <- dF %>% thicken(by = "time","5 min",colname= "time_5min") %>% 
    left_join(dF2, by = "time_5min") %>%
    select(time, time_5min, lat,lon, accuracy, latF1,lonF1,latF2,lonF2) %>% 
    mutate(
      lonF = case_when(
        is.na(lonF1) ==F ~ lonF1,
        is.na(lonF1) == T ~lonF2),
      latF = case_when(
        is.na(lonF1 ) == F ~ latF1,
        is.na(lonF1 ) == T ~ latF2
      )
    )
  
  # selects uniques and pads
  d.filt<- d.filt %>% 
    select(time_5min, lonF,latF)%>%
    distinct() %>%
    as_tbl_time(time_5min) %>% 
    pad()
  return(d.filt)
}
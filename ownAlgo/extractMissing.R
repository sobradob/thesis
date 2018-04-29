# extract missing values
# feature extract
# put in ML mode

getMissing <- function(data = fin, remove_ind = remove_ind, rems = rems){
  library(padr)
  library(tidyr)
  
  test <- fin %>% slice(-remove_ind) %>% 
    select(time, clust, timestampMs)%>%
    mutate(isna = 0) %>% 
    thicken( interval = "5 min", colname = "thick") %>% 
    pad(by = "thick") %>% 
    mutate(timestampMs2 = as.numeric(thick),
           day  = strftime(thick,"%u"),
           month = strftime(thick,"%m"),
           secMidnight = lubridate::period_to_seconds(lubridate::hms(strftime(thick,"%T"))),
           sinTime = sin(2*pi*secMidnight/seconds_in_day),
           cosTime = cos(2*pi*secMidnight/seconds_in_day),
           nextMeas = as.numeric(lead(timestampMs)-timestampMs),
           prevMeas = as.numeric(timestampMs-lag(timestampMs)),
           lagClust = lag(clust),
           leadClust = lead(clust),
           timestampLast = as.numeric(thick),
           timestampNext = as.numeric(thick),
           clustNext = clust,
           clustPrev = clust) %>% 
    fill(timestampLast,clustPrev, .direction = "down") %>% 
    fill(timestampNext,clustNext, .direction = "up") %>%
    mutate( prevMeas = timestampMs2 - timestampLast,
            nextMeas = timestampNext - timestampMs2,
            lagClust = clustPrev,
            leadClust = clustNext) %>% 
    mutate(nextMeas = scales::rescale(nextMeas, from = c(0,3600000)),
           prevMeas = scales::rescale(prevMeas, from = c(0,3600000))) %>% 
    filter(thick %in% rems)
  
  x_test <- cbind(test[,c("nextMeas","prevMeas","sinTime","cosTime")],
                  to_categorical(test$day, num_classes = 8),
                  to_categorical(test$month, num_classes = 13),
                  to_categorical(test$lagClust,num_classes = 807),
                  to_categorical(test$leadClust, num_classes = 807)) %>% as.matrix()
  return(x_test)
}



  
  
  

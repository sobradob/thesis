# cluster extraction funcion

clusterFunc <- function(data, timeLim, accuracyLim,distLim,minPause){
  
  c<- data %>% filter(accuracy <accuracyLim) %>% 
    mutate( prevLon = lag(lon),
            prevLat = lag(lat),
            nextLon = lead(lon),
            nextLat = lead(lat),
            nextDist = raster::pointDistance(matrix(c(lon,lat),ncol = 2),
                                             matrix(c(nextLon,nextLat),ncol = 2),
                                             longlat = T),
            prevDist = raster::pointDistance(matrix(c(lon,lat),ncol = 2),
                                             matrix(c(prevLon,prevLat),ncol = 2),
                                             longlat = T),
            nextMeas = as.numeric(timestampMs-lead(timestampMs)),
            prevMeas = as.numeric(lag(timestampMs)-timestampMs),
            isPause = case_when(prevMeas <= timeLim &nextMeas <= timeLim & nextDist <= distLim~1,
                                TRUE ~0)
    ) %>%
    group_by(run = {run = rle(isPause); rep(seq_along(run$lengths), run$lengths)}) %>%
    summarize(isPause = mean(isPause),
              lon = mean(lon),
              lat = mean(lat),
              t0  = min(timestampMs),
              t1  = max(timestampMs),
              duration = t1-t0)
  
  return(c)
  #perhaps take weighted mean
  # incorporate minimum missing
  # filter accuracies
}
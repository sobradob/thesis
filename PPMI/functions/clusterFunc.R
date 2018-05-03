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
              meanLon = mean(lon),
              meanLat = mean(lat),
              meanAcc = mean(accuracy),
              t0  = min(timestampMs),
              t1  = max(timestampMs),
              t0TS = min(time),
              t1TS = max(time),
              duration = t1-t0,
              minLon = min(lon),
              minLonsLat = lat[which.min(lon)],
              maxLon = max(lon),
              maxLonsLat = lat[which.max(lon)],
              minLat = min(lat),
              minLatsLon = lon[which.min(lat)],
              maxLat = max(lat),
              maxLatsLon = lon[which.max(lat)],
              distMeanNorth = raster::pointDistance(matrix(c(meanLon,meanLat),ncol = 2),
                                                    matrix(c(maxLon,maxLonsLat),ncol = 2),
                                                    longlat = T),
              distMeanSouth = raster::pointDistance(matrix(c(meanLon,meanLat),ncol = 2),
                                                    matrix(c(minLon,minLonsLat),ncol = 2),
                                                    longlat = T),
              distMeanWest = raster::pointDistance(matrix(c(meanLon,meanLat),ncol = 2),
                                                    matrix(c(minLatsLon,minLat),ncol = 2),
                                                    longlat = T),
              distMeanEast = raster::pointDistance(matrix(c(meanLon,meanLat),ncol = 2),
                                                   matrix(c(maxLatsLon,maxLat),ncol = 2),
                                                   longlat = T)
              ) %>% 
                mutate(isPause = case_when(duration <= minPause ~ 0,
                                           duration > minPause & isPause == 1 ~ 1,
                                           TRUE ~0)
              )
  
  return(c)
  #perhaps take weighted mean
  # incorporate minimum missing
  # filter accuracies
}
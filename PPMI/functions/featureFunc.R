#Calculates features for the ML model

featureFunc <- function(data, timeLim = 200, distLim = 100,minPause = 120, accuracyLim = 50){
  
  #calculate wether it is within accuracy
  # add other metrics
  data2<- data %>%
    arrange(timestampMs) %>% mutate(
      withinAcc = case_when(accuracy >= distClustMin ~1,
                            TRUE ~ 0),
      prevLon = lag(lon),
      prevLat = lag(lat),
      nextLon = lead(lon),
      nextLat = lead(lat),
      nextDist = raster::pointDistance(matrix(c(lon,lat),ncol = 2),
                                       matrix(c(nextLon,nextLat),ncol = 2),
                                       longlat = T),
      prevDist = raster::pointDistance(matrix(c(lon,lat),ncol = 2),
                                       matrix(c(prevLon,prevLat),ncol = 2),
                                       longlat = T),
      nextMeas = as.numeric(lead(timestampMs)-timestampMs),
      prevMeas = as.numeric(timestampMs-lag(timestampMs)),
      isPause = case_when(prevMeas <= timeLim &nextMeas <= timeLim & nextDist <= distLim~1,
                          TRUE ~0),
      pauseClust = case_when(isPause == 1 & withinAcc == 1~ as.numeric(clust),
                             TRUE ~NA_real_),
      nextPauseClust = pauseClust,
      prevPausClust = pauseClust
    ) %>% 
    fill(nextPauseClust,.direction ="up") %>%
    fill(prevPausClust,.direction ="down")
  
  return(data2)
}

featureFunc2 <- function(data,pauseExtracted,distCoef=2){
pauseStart<- pauseExtracted %>% filter(isPause == 1) %>%
select(t0TS,t1TS,meanAcc,distMeanEast,distMeanWest,distMeanSouth,distMeanNorth) %>%
filter(!(distMeanEast > distCoef*meanAcc |
distMeanWest >distCoef*meanAcc |
distMeanNorth >distCoef*meanAcc |
distMeanSouth >distCoef*meanAcc)) %>%
select(t0TS,t1TS) %>% pull(t0TS)
pauseEnd<- pauseExtracted %>% filter(isPause == 1) %>%
select(t0TS,t1TS,meanAcc,distMeanEast,distMeanWest,distMeanSouth,distMeanNorth) %>%
filter(!(distMeanEast > distCoef*meanAcc |
distMeanWest >distCoef*meanAcc |
distMeanNorth >distCoef*meanAcc |
distMeanSouth >distCoef*meanAcc)) %>%
select(t0TS,t1TS) %>% pull(t1TS)
pauseDf<- rbind(
data.frame(time = pauseStart,pauseID = 1:length(pauseStart), type = "start"),
data.frame(time = pauseEnd,pauseID = 1:length(pauseEnd), type = "end")
) %>%
arrange(time)
temp <- left_join(data,pauseDf) %>% mutate(typeLag = lag(type))
temp[which(temp$type == "start"),"typeLag"] <- "start"
temp2<- temp %>%
fill(typeLag, .direction = "down") %>%
filter(typeLag == "start") %>%
select(time,pauseID) %>%
mutate(isPause = 1) %>%
fill(pauseID,.direction = "down")
data2<- left_join(data,temp2) %>%
mutate(
nextMeas = as.numeric(lead(timestampMs)-timestampMs),
prevMeas = as.numeric(timestampMs-lag(timestampMs)),
pauseClust = case_when(isPause == 1 ~ as.numeric(clust),
TRUE ~NA_real_),
nextPauseClust = pauseClust,
prevPausClust = pauseClust
) %>%
fill(nextPauseClust,.direction ="up") %>%
fill(prevPausClust,.direction ="down")
return(data2)
}

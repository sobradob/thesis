# feature extract palmius

#change the data format as to make the imputation possible
featureExtractPalmius <- function(data){
  preImp <- data %>% mutate( timestampMs = as.numeric(time_5min),
                             tMissEnd = case_when(
                               is.na(lonF) ==  T~ NA_real_,
                               !is.na(lonF)== T ~ timestampMs),
                             tMissStart = tMissEnd,
                             lonMissEnd =case_when(
                               is.na(lonF) ==  T~ NA_real_,
                               !is.na(lonF)== T ~ lonF),
                             lonMissStart = lonMissEnd,
                             latMissEnd = case_when(
                               is.na(lonF) ==  T~ NA_real_,
                               !is.na(lonF)== T ~ latF),
                             latMissStart = latMissEnd) %>% 
    fill(tMissEnd,lonMissEnd,latMissEnd, .direction ="up") %>% 
    fill(tMissStart,lonMissStart,latMissStart, .direction ="down") %>% 
    mutate(code = case_when(
      is.na(lonF) ==  T~ 0,
      !is.na(lonF)== T ~ 1),
      priorCode = lag(code),
      hour24 = as.numeric(strftime(time_5min, "%H")),
      firstMissing = case_when(
        code == 0 & priorCode == 1 ~ 1,
        TRUE ~ 0))
  
  #how many missing? 
  preImp %>% summarise(mean(is.na(lonF))) #about 12 percent
  preImp %>% summarise(mean(code)) #non missing
  
  #add additional features needed for the imputation
  home<- c(5.113919,52.10421)
  preImp$distEndHome<- apply(preImp, 1, FUN = function(row) {
    raster::pointDistance(home,
                          c(as.numeric(as.character(row["lonMissEnd"])), as.numeric(as.character(row["latMissEnd"]))),
                          lonlat = T) # Parameter 'lonlat' has to be TRUE!
  })
  
  preImp$distStartHome<- apply(preImp, 1, FUN = function(row) {
    raster::pointDistance(home,
                          c(as.numeric(as.character(row["lonMissStart"])), as.numeric(as.character(row["latMissStart"]))),
                          lonlat = T) # Parameter 'lonlat' has to be TRUE!
  })
  
  preImp$delta_dist<- apply(preImp, 1, FUN = function(row) {
    raster::pointDistance(c(as.numeric(as.character(row["lonMissEnd"])), as.numeric(as.character(row["latMissEnd"]))),
                          c(as.numeric(as.character(row["lonMissStart"])), as.numeric(as.character(row["latMissStart"]))),
                          lonlat = T) # Parameter 'lonlat' has to be TRUE!
  })
  return(preImp)
}


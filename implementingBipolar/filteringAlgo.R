# Palmius filtering algorithm implemented in R.

# set wd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#read data
data<- readRDS("../../data/boaz/myLocationHistory09012018.rds")

# load auxiliary functions
source("../../thesis/scripts/auxFuns.R")

#filter down to a year

dataExample <- select(data,time,lat,lon,accuracy) %>%
  arrange(time) %>% 
  as_tbl_time(time) %>% 
  filter_time( ~ '2017') %>%
  mutate( nextLon = shift.vec(lon,-1),
          nextLat = shift.vec(lat,-1),
          nextTimeSec = shift.vec(time,-1)-time,
          index = 1:length(lat)
)

readr::write_csv(dataExample,"balazs2017.csv")
rm(data)

d<- dataExample
dPrimeSource<- 100
dPrimeDest <- 1
removed <- data.frame(lon = 1, lat = 1)

while(dPrimeSource >= 10 & dPrimeDest < 10){
  
  #calculate distance between points
  d$distancesMeter<- apply(d, 1, FUN = function(row) {
    raster::pointDistance(c(as.numeric(as.character(row["lon"])),
                            as.numeric(as.character(row["lat"]))),
                          c(as.numeric(as.character(row["nextLon"])), as.numeric(as.character(row["nextLat"]))),
                          lonlat = T) # Parameter 'lonlat' has to be TRUE!
  })
  
  #add index and calculate speed between points
  d <- mutate(d,
              speedMeterSecond= distancesMeter/as.numeric(nextTimeSec),
              index = 1:nrow(dataExample)
  )
  
  # set speed parameter
  maxSpeed <- 27.7 #equivalent to 100 kmh
  
  # extract source 
  filteredSource <- d %>% 
    filter(speedMeterSecond >= maxSpeed)
  
  # extract destination
  filteredDestination <- d %>%
    filter(index %in% as.numeric(filteredSource$index+1))
  
  # extract modes
  
  sourceMode<- filteredSource %>% 
    count(lon, lat) %>%
    slice(which.max(n))
  
  destMode<- filteredDestination %>% 
    count(lon, lat) %>%
    slice(which.max(n))
  
  if(sourceMode$n >= destMode$n & sourceMode$n >10){
    d <<- filter(d, lon != sourceMode$lon & lat != sourceMode$lat)
    removed <- rbind(removed, sourceMode[,c("lon","lat")])
    } else if(destMode$n >= 10){
    d <<- filter(d, lon != destMode$lon & lat != destMode$lat)
    removed <- rbind(removed, sourceMode[,c("lon","lat")])
  }
  
  dPrimeSource<- sourceMode$n
  dPrimeDest <- destMode$n
  
}

# visual exploration for particular dates

exploreDates(df = d, Data_Start = "2017-02-22",Data_End = "2017-02-22")
exploreDates(df = dataExample, Data_Start = "2017-02-22",Data_End = "2017-02-22")

# view 

dRem <- filter(dataExample,
               lon %in% removed$lon & lat %in% removed$lat)
exploreDates(df = dRem,Data_Start = "2017",Data_End = "2017")

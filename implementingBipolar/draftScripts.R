# Palmius Recreation Script workspace

# set wd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#read data
data<- readRDS("../../data/boaz/myLocationHistory09012018.rds")

# load auxiliary functions
source("../../thesis/scripts/auxFuns.R")

library(tidyr)
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

filtered<- filteringPalmius(data = dataExample)
downsampled <-  downSamplingPalmius(data = filtered) #change name of time column, lat and lon

preImp <- downsampled %>% mutate( timestampMs = as.numeric(time_5min),
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
  fill(tMissStart,lonMissStart,latMissStart, .direction ="down")

# filter down to periods when living in utrecht

# create NA's for variables where it makes sene
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
# filter down to periods when living in utrecht
utrecht<- preImp %>% filter(distEndHome < 500000) %>%
  mutate( code = case_when(
    is.na(lonF) ==  T~ 0,
    !is.na(lonF)== T ~ 1),
    priorCode = shift.vec(code,1),
    hour24 = as.numeric(strftime(time_5min, "%H")),
    firstMissing = case_when(
      code == 0 & priorCode == 1 ~ 1,
      TRUE ~ 0)
    )

summary(utrecht)
plot(utrecht$distEndHome)

i <-  40
interval <- 300

indxFirstMissing<- which(utrecht$firstMissing == 1)

utrecht$impLon <- NA
utrecht$impLat <- NA

for(i in indxFirstMissing){
  
  data<- utrecht[i,]
  # extract variables
  tMissEnd<- data["tMissEnd"]
  tMissStart<- data["tMissStart"]
  
  #distance to end etc
  distEndHome <- data["distEndHome"]
  distStartHome <- data["distStartHome"]
  
  #location missing end and beginning
  locMissEnd<- data[c("lonMissEnd", "latMissEnd")]
  locMissStart<- data[c("lonMissStart", "latMissStart")]
  
  
  #Hour in PM needed
  hour24 <- data["hour24"]
  delta_dist <- data["delta_dist"]
  
  # create for loop to do this to each row.
  # Calculate time elapsed
  delta_t <- tMissEnd - tMissStart
  # length of missing intervals
  lengthMiss <- (delta_t/interval)-1 

  # Calculate midpoints
  midpoint <- colMeans(rbind(locMissStart,setNames(locMissEnd, names(locMissStart))))
  
  #impute 
  imputed <- imputePalmiusA1(distEndHome = distEndHome,
                             distStartHome = distStartHome,
                             delta_t = delta_t,
                             hour24 = hour24,
                             locMissStart = locMissStart,
                             locMissEnd = locMissEnd,
                             delta_dist = delta_dist)
  #end result for loop
  utrecht[i:as.numeric(i+lengthMiss-1),c("impLon","impLat")] <- imputed
}

u2 <- utrecht %>% 
filter(distEndHome < 500000 & timestampMs < (1.489*10^9) & (impLat > 24 | is.na(impLat)))

utrecht %>% 
  filter(!is.na(impLon)) %>%
  summary()

saveRDS(u2,"imputedPalmius.rds")

leaflet() %>%
  addTiles() %>%
  addCircles(lng = ~lonF, lat = ~latF, data = u2, color = "#18206F", group = "Downsampled")%>%
  addCircles(lng = ~impLon, lat = ~impLat, data=u2,color = "#DF2935",group = "Imputed")%>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addLayersControl(
    overlayGroups = c("Downsampled", "Imputed"),
    options = layersControlOptions(collapsed = FALSE)
  )

# to do:
# add gitignore
# push to git
# debug benin 
# finish document and send to Palmius & Lugtig
# finish document for Barnett & Onella 
# send to lugtig and Ian

# look into neural netowrks. 
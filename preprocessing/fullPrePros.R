# Full Pre Processing Script
timestamp()
library(dplyr)
library(readr)
library(raster)
# set path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#this is the file to use for future things
#x<-readRDS("myLocationHistory.rds")

# if not yet installed install jsonlite

# read data

path <- "set\your\path\here"

# read Google's JSON file
full<-jsonlite::fromJSON(path)

# extracting the locations dataframe
loc <- full$locations
timestamp()
rm(full)

# converting time column from posix milliseconds into a readable time scale
loc$time <- as.POSIXct(as.numeric(loc$timestampMs)/1000, origin = "1970-01-01")

# converting longitude and latitude from E7 to GPS coordinates
loc$lat <- loc$latitudeE7 / 1e7
loc$lon <- loc$longitudeE7 / 1e7

# save 
saveRDS(loc, file ="myLocationHistory.rds")
timestamp()
#646376 rows

#extract activities
timestamp()
activities <- loc$activitys

list.condition <- sapply(activities, function(x) !is.null(x[[1]]))
activities  <- activities[list.condition]
rm(list.condition)
extractActivity<-function(top){
  list.condition <- sapply(top, function(x) !is.null(x[[1]]))
  top  <- top[list.condition]
  time_stamp<-sapply(top, function(x) x[[1]][[1]][[1]][1])
  main_activity<-sapply(top, function(x) x[[2]][[1]][[1]][1])
  main_confidence<-sapply(top, function(x) x[[2]][[1]][[2]][1])
  df<-data.frame(timeStamp=as.numeric(unlist(time_stamp)),activity=unlist(main_activity),confidence = as.numeric(unlist(main_confidence)))
  return(df)
}


act.df<-extractActivity(activities)
act.df$time <- as.POSIXct(as.numeric(act.df$timeStamp)/1000, origin = "1970-01-01")

rm(activities)
timestamp()

# add GPS measurements to inferred modes of transport
t<-data.frame(time = c(loc$time,act.df$time),
              act = c(rep(NA,nrow(loc)), as.character(act.df$activity)),
              confidence = c(rep(NA,nrow(loc)),act.df$confidence),
              lat = c(loc$lat,rep(NA,nrow(act.df))),
              lon = c(loc$lon,rep(NA,nrow(act.df))))

t<-t %>% arrange(time)

# Shifting vectors for latitude and longitude to include end position
shift.vec <- function(vec, shift){
  if (length(vec) <= abs(shift)){
    rep(NA ,length(vec))
  } else {
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec) - shift)]) }
    else {
      c(vec[(abs(shift) + 1):length(vec)], rep(NA, abs(shift)))
    }
  }
}


t$tb<-t$time-as.POSIXct(shift.vec(t$time,1), origin = "1970-01-01")
t$ta<-as.POSIXct(shift.vec(t$time,-1), origin = "1970-01-01")-t$time

t[which(is.na(t$lat) & t$ta<t$tb),c("lat","lon")]<- t[which(is.na(t$lat) & t$ta<t$tb)+1,c("lat","lon")]
t[which(is.na(t$lat) & t$ta>t$tb),c("lat","lon")]<- t[which(is.na(t$lat) & t$ta>t$tb)-1,c("lat","lon")]

timestamp()
#first and last row are missing
t<-na.omit(t)

t$lat.p1 <- shift.vec(t$lat, -1)
t$lon.p1 <- shift.vec(t$lon, -1)

#calculate distance
t$dist.to.prev <- apply(t, 1, FUN = function(row) {
  pointDistance(c(as.numeric(as.character(row["lat.p1"])),
                  as.numeric(as.character(row["lon.p1"]))),
                c(as.numeric(as.character(row["lat"])), as.numeric(as.character(row["lon"]))),
                lonlat = T) # Parameter 'lonlat' has to be TRUE!
})
t$dist.to.prev<-t$dist.to.prev/1000

t$Month <- as.Date(cut(t$time,
                       breaks = "month"))
t$Week <- as.Date(cut(t$time,
                      breaks = "week",
                      start.on.monday = T))
t$Day <- as.Date(cut(t$time,
                     breaks = "day"))

timestamp()
write_csv(t,path = "myActivities.csv")
timestamp()
saveRDS(t,"myGPSRDS.rds")
timestamp()
rm(list = ls())

## Implementing measures from the paper
#http://ieeexplore.ieee.org/document/7676335/

# read data
data<- readRDS("C:\\Users\\user1\\Desktop\\Life\\Professional\\Statistics Nederland\\gps\\boaz\\Takeout\\Location History\\myGPSRDS.rds")

#or just a day?

day <- readRDS("C:\\Users\\user1\\Desktop\\Life\\Academic\\Utrecht\\year2\\thesis\\kalmanFilters\\implementation\\feb17.rds")

# part 1: Filtering

## based on speed. How was speed calculated?

# distance

distances<- apply(day, 1, FUN = function(row) {
  raster::pointDistance(c(as.numeric(as.character(row["lon"])),
                  as.numeric(as.character(row["lat"]))),
                c(as.numeric(as.character(row["lonNext"])), as.numeric(as.character(row["latNext"]))),
                lonlat = T) # Parameter 'lonlat' has to be TRUE!
})

# output is in meters, convert to kilometers
# does not match original, perhaps due to lat lon mixup in data pre processing file 

distkm<- distances/1000
time.hr<- as.numeric(day$timeNextMeasure, units="hours")

speed<- distkm/time.hr

day$speed <- speed

#inspect

library(leaflet)
library(sp)

speedingFilter<- which(day$speed>100)
speeding<- day[speedingFilter,]
notSpeeding <- day[-speedingFilter,]
lns <- Line(cbind(day$lon,day$lat))

#addCircles(lng = ~lon, lat = ~lat, weight = 1,
#           radius = ~accuracy,fillOpacity = 0.02,color = "#DF2935")%>%
  

leaflet(day)%>%
  addTiles() %>%
#  addPolylines(data = lns,color = "#DF2935")%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(data=speeding,lng = ~lon, lat = ~lat, weight = 1,
             radius = ~accuracy,fillOpacity = 0.02,color = "#5F53AD")%>%
  addPolylines(data = lns2,color = "#5F53AD")

# basically it gets stuck there. Remove those locations?
#stopped here

pointsToRemove<- day[which(day$speed>100),c("lon","lat")]

day %>% filter(lat == 52.09203)

lns2 <- Line(cbind(day$lon,day$lat))

library(ggmap)

day<-readRDS("C:\\Users\\user1\\Desktop\\Life\\Academic\\Utrecht\\year2\\thesis\\kalmanFilters\\implementation\\feb17.rds")
library(leaflet)
library(dplyr)
library(sp)

#initializing variables
count <- nrow(day) # amount of data points in the day
z <- cbind(day$lon,day$lat) #measurements

#Allocate space:
xhat <- matrix(rep(0,2*count),ncol =2) #a posteri estimate at each step
P <- array(0,dim=c(2,2,count))  #a posteri error estimate
xhatminus <- matrix(rep(0,2*count),ncol =2) #a priori estimate
Pminus <- array(0,dim=c(2,2,count)) #a priori error estimate
K <- array(0,dim=c(2,2,count)) #gain

#Initializing matrices
A <-diag(2)
H<-diag(2)
R<-function(k) diag(2)* day$accuracy[k]^2#estimate of measurement variance
Q<-function(k) diag(2)* as.numeric(day$timeNextMeasure[k])^1.5# the process variance

#initialise guesses:
xhat[1,] <- z[1,]
P[,,1] <- diag(2)

for (k in 2:count){
  #time update
  #project state ahead
  xhatminus[k,] <- A %*% xhat[k-1,] #+ B %*% u[k-1]
  
  #project error covariance ahead
  Pminus[,,k] <- A %*% P[,,k-1] %*%  t(A) + (Q(k))
  
  #measurement update
  # kalman gain
  K[,,k] <- Pminus[,,k] %*% t(H)/ (H %*% Pminus[,,k] %*% t(H) + R(k))
  
  #what if NaaN?
  K[,,k][which(is.nan(K[,,k]))]<-0
  
  # update estimate with measurement
  xhat[k,] <-  xhatminus[k,] + K[,,k] %*% (z[k,] - H %*% xhatminus[k,])
  #update error covariance
  P[,,k] = (diag(2) - K[,,k]%*% H) %*% Pminus[,,k]
}

lns <- Line(na.omit(cbind(day$lon,day$lat)))
lnsKF<-Line(na.omit(xhat))

index<- 350:430
lns <- Line(na.omit(cbind(day$lon[index],day$lat[index])))
lnsKF<-Line(na.omit(xhat[index,]))


## ggmap attempt

myLocation <- day[1,c("lon","lat")]
myLocation <- c(5.150,52.09)
utrecht<- get_map(myLocation,zoom = 13)

mmap <- get_map(location = c(11.33,44.49,11.36,44.50),source = "osm")

library(RgoogleMaps)

u<- GetMap.bbox(c(min(day$lon),max(day$lon)),c(min(day$lat),max(day$lat)))
ggmap(u)
ggmap(utrecht)+
  geom_point(aes(x = lon, y = lat), data = day,
             alpha = .5, color="darkred", )


library(rgeos)
library(sp)


install.packages("rgdal")
library(rgdal)

day2<- day

day <- day2[350:430,]# so far so good, perhaps remove obs at the

klm<- data.frame(xhat[350:430,])
colnames(klm) <- c("lon","lat")

d <- SpatialPointsDataFrame(coords = day[,c("lon","lat")], 
                            data = day, 
                            proj4string = CRS("+init=epsg:4326"))


d_mrc <- spTransform(d, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))

d_mrc_bff_mrc <- gBuffer(d_mrc, byid = TRUE, width = day$accuracy)
d_mrc_bff <- spTransform(d_mrc_bff_mrc, CRS("+init=epsg:4326"))
d_mrc_bff_fort <- fortify(d_mrc_bff)


ggmap(utrecht)+
  geom_point(aes(x = lon, y = lat), data = day,
             alpha = .5, color="#DF2935")+
  geom_path(data=d_mrc_bff_fort, aes(long, lat, group=group), color="#DF2935")+
  geom_path(data= day, aes(x = lon, y =lat), color = "#DF2935")+
  geom_path(data= klm, aes(x = lon, y =lat), color = "#18206F")

#first do it with google map, then mapbox

#was this a waste of time, attemtp 2

library(leaflet)
day2 <- day
day <- day2[350:430,]
lns2 <- lns[350:430,]
lns2KF <- lnsKF[350:430,]

m<- leaflet(day,options = leafletOptions(zoomControl = FALSE))%>%
  addPolylines(data = lns,color = "#DF2935") %>%
  addPolylines(data = lnsKF,color = "#18206F")%>%
  addCircles(lng = ~lon, lat = ~lat, weight = 1,
             radius = ~accuracy,fillOpacity = 0.02,color = "#DF2935")%>%
  addProviderTiles(providers$CartoDB.Positron)

m

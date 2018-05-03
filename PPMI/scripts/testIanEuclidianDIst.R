# testing that the distances in Ian's coordinate system are close to meters

# test 1: figure out if euclidian distance in this way is the same or approximatelt the same as meters

per <- 1:1000
for(i in 1:1000){
  randomIndex<- sample(1:nrow(data2),2)
  euc <- dist(data2[randomIndex,c("x_v","y_v")],method = "euclidean")
  gcdist <- raster::pointDistance(data2[randomIndex[1],c("lon","lat")],data2[randomIndex[2],c("lon","lat")],lonlat = TRUE)
  per[i]<- euc/gcdist
}
summary(per)
#approx the same. underestimates less than 1 percent on average

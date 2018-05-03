# Cluster & Paths

# Run clustering function to extract clusters

# extract frequently travelled paths

clustMap <- xy %>% as.data.frame() %>% 
  group_by(clust) %>% 
  summarise(lon = mean(lon),
            lat = mean(lat),
            n   = n())

# or better dist matrix like before, with accuracy as value 

# table with all paths


checkPath(data2, start = 144, end = 3)
checkPath(data2, start = 48, end = 36)
checkPath(data2, start = 55, end = 139)

d3<- data2 %>% filter( (nextPauseClust ==3|nextPauseClust ==36|nextPauseClust ==139) &  (prevPausClust ==144|prevPausClust ==48|prevPausClust ==55))
saveRDS(d3,"data2.rds")

checkPath(d3, start = 144, end = 3)
checkPath(d3, start = 48, end = 36)
checkPath(d3, start = 55, end = 139)

# key lessons learned: high accuracy movement patterns will often fit into a bin when there is movement

# analyse whether the individual was actually in the cluster based on time spent there using the prior code. 

# add isPause variable, add next/prior cluster, time, pause duration, etc. 

saveRDS(data2,"nnData.rds")


# write script to extract two clusters, draw path

# fpr all paths between clusters
# how to time segment trips.
# Daily one trip assumption reasonable ?
# or calculate average trip time & use that ?
start <- 100
end <- 33
test<- data2 %>% 
filter((nextPauseClust == start &  prevPausClust == end)|
         (nextPauseClust == end &  prevPausClust == start)) %>% 
  mutate(trip = case_when( nextPauseClust == start ~ 1,
                           nextPauseClust == end ~ 0)) %>% 
  thicken(by = "time","1 day")
  
sample<- split(test[,c("trip","lon","lat")], f=list(test$trip,test$time_day), drop = T)

for(i in 1:length(sample)){
  if(sample[[i]][1,3] == 1){
    sample[[i]] <- sample[[i]]%>% arrange(-row_number())  
  }
  sample[[i]] <- sample[[i]][,c("lon","lat")]
}
library(dtw)

# error, cant cluster if only two

distMatrix <- dist(sample, method="DTW") #create a distance matrix
hc <- hclust(distMatrix, method="single") #hierarchical clustering
plot(hc)
# take path between two clusters
clust<- data.frame( code = cutree(hc, k = NULL, h = 0.2),
                    trip = names(cutree(hc, k = NULL, h = 0.2)))

tripMat <- left_join( test %>%
                        mutate( trip = paste0(trip,'.',time_day)),
                      clust) %>% #merge the clusters with the data
  filter( code ==1) %>% #filter to cluster 1
  select(lon,lat) %>% 
  as.matrix()

library(LPCM)
x0 <- filter(clustMap, clust %in% c(start,end)) %>% select(lon,lat) %>% as.matrix()

# not ideal. It should connect the two points. 
# look into alternative lpc implemetations.

#something funny with weights?
tripMat <- rbind(x0,tripMat)
w <- rep(1,nrow(tripMat))
w[1:2] <- 1000

# set bandwith and t0 depending on length of trip and frequency

lpc(tripMat, h = 0.1, t0 = 0.1, pen = 0,control = lpc.control(iter = 1000), weights = w, x0 = x0[1,]) %>% 
  plot(asp = T) # how to set params? 

#idea, take weights for start and end and also use accuracy. 
# use algorithm as in the other paper to play around with weights.

as.data.frame(tripMat) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers() %>% 
  addCircles(data = filter(clustMap, clust ==start),lng = ~lon, lat = ~lat, color = "red", radius = 100) %>% 
  addCircles(data = filter(clustMap, clust ==end),lng = ~lon, lat = ~lat, color = "green", radius = 100)  
# compute path quantity 

# draw points

ggplot()+geom_point(x = ~lon, y = ~lat,data = as.data.frame(tripMat))
# line drawer  has issues.
# trip extractor has issues. 

install.packages("cobs")
library(cobs)
x <- seq(-1,3,,150)
y <- (f.true <- pnorm(2*x)) + rnorm(150)/10
## specify pointwise constraints (boundary conditions)
con <- rbind(c( 0,min(x),-1), # f(min(x)) >= 0
             c( 0,max(x),1)) # f(max(x)) <= 1
## obtain the median  REGRESSION  B-spline using automatically selected knots
Rbs <- cobs(x,y, constraint= "increase", pointwise = con)
Rbs
plot(Rbs, lwd = 2.5, asp = T)
lines(spline(x, f.true), col = "gray40")
lines(predict(cobs(x,y)), col = "blue")
mtext("cobs(x,y)   # completely unconstrained", 3, col= "blue")
## compute the median  SMOOTHING  B-spline using automatically chosen lambda
Sbs <- cobs(x,y, constraint="increase", pointwise= con, lambda= -1)
plot(Sbs)


plot(tripMat)

amsterdam <- get_map(location = "Amsterdam",zoom = 10,source = "stamen", force = T)
ggmap(amsterdam) +  geom_point(data = as.data.frame(tripMat), mapping = aes(x = lon, y = lat))

con <- filter(clustMap, clust %in% c(start,end)) %>% select(lon,lat) %>% as.matrix()
con<- cbind(c(0,0),con)

## obtain the median  REGRESSION  B-spline using automatically selected knots

y <- tripMat[,1] 
x <- tripMat[,2] 
Rbs <- cobs(x = x,y=y, pointwise = con, nknots = 20, method = "uniform", maxiter = 1000)
Rbs
plot(Rbs, lwd = 2.5, asp = T)
lines(spline(x, f.true), col = "gray40")
lines(predict(cobs(x,y)), col = "blue")
mtext("cobs(x,y)   # completely unconstrained", 3, col= "blue")


install.packages("ggmap")
library(ggmap)
ggplot(data = as.data.frame(tripMat), aes( x = lon, y = lat))+
  geom_point()

# two issues at least:
#lon lat is ugly. Convert to meters if possible using Barnett & Onnella or the other method used in the clustering algo.

# line drawing is bad. try the different algos using the methods given.

library(sp)
library(rgdal)
library(geosphere)

# convert data to a SpatialPointsDataFrame object
xy <- SpatialPointsDataFrame(coords = as.data.frame(tripMat),
                             data = data.frame(1:nrow(tripMat)),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

res <- spTransform(xy, CRS("+proj=utm +zone=31 ellps=WGS84"))
res
as(res, "SpatialPoints")

plot(res@coords)
# Get UTM

long2UTM <- function(long) {
  (floor((long + 180)/6) %% 60) + 1
}

long2UTM(4.9)


xy<- LatLong2XY(tripMat[,"lon"],tripMat[,"lat"])
xy$x_v
plot(xy$x_v,xy$y_v)

library(sp)
sl <- SpatialLines(list(Lines(list(Line(coords = pathPoints)), ID = "1")))
sl2 <- SegmentSpatialLines(sl, length = 50, merge.last = TRUE)
# plot
plot(sl2, col = rep(c(1, 2), length.out = length(sl2)), axes = T)

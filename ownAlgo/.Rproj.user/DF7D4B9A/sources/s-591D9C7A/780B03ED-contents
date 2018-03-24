# route extraction and conversion to path

install.packages("LPCM")

# consider changing into XY coordinates?

library(LPCM)

# work through example

data(calspeedflow)
lpc1 <- lpc(calspeedflow[,3:4])
plot(lpc1)

# try own data

# the data loaded is in the workspace
test<- t2 %>% thicken(by = "time","1 day")%>%
  mutate( trip = case_when(
    (tNextUSec < 60*60 & tPrevHSec < 60*60)~1,
    (tPrevUSec < 60*60 & tNextHSec < 60*60)~2,
    TRUE ~0
  )) %>% 
  filter(trip !=0) %>%
  filter(accuracy < 16) %>% 
  select(lon,lat,trip,time_day,time)

lpc2 <- lpc(as.matrix(test),scaled = T)
plot(lpc2)


lpc2 <- lpc(as.matrix(test),h=.001,  depth=2, scaled=F, control = lpc.control(iter = 1000,boundary = 0.001))
plot(lpc2)
plot(lpc2$LPC)

# example in the thing.

ex<- c(rep(0,40), seq(0,1,length=20), seq(0,1,length=20), seq(0,1,length=20))
ey<- c(seq(0,2,length=40), rep(0,20), rep(1,20), rep(2,20))
sex<-rnorm(100,0,0.01); sey<-rnorm(100,0,0.01)
eex<-rnorm(100,0,0.1);  eey<-rnorm(100,0,0.1)
ex1<-ex+sex; ey1<-ey+sey
ex2<-ex+eex; ey2<-ey+eey
e1<-cbind(ex1,ey1); e2<-cbind(ex2,ey2)
lpc.e1 <- lpc(e1, h= c(0.1,0.1),  depth=2, scaled=FALSE)
plot(lpc.e1, type=c("curve","mass", "start"))

#idea: take lpc of each instance, then take lpc-s and plot those. Not likely to be good. Why not downsample via area?

# Why not downsample via distance from endpoint?

# select all points within x meters from endpoint
# take median distance of points
# sample from point closest to median distance
# select all within radius except point and delete
# check to see if there are remaining


# another idea: check dynamic time warping

install.packages("dtw")
## A noisy sine wave as query
idx<-seq(0,6.28,len=100);
query<-sin(idx)+runif(100)/10

plot(query)
rnd <- sample(1:100,50)
plot(query[-rnd])
q<- query
## A cosine is for template; sin and cos are offset by 25 samples
template<-cos(idx)

## Find the best match with the canonical recursion formula
library(dtw);
alignment<-dtw(q,template,keep=TRUE);

## Display the warping curve, i.e. the alignment curve
plot(alignment,type="threeway")

## Align and plot with the Rabiner-Juang type VI-c unsmoothed recursion
plot(
  dtw(query,template,keep=TRUE,
      step=rabinerJuangStepPattern(6,"c")),
  type="twoway",offset=-2);

## See the recursion relation, as formula and diagram
rabinerJuangStepPattern(6,"c")
plot(rabinerJuangStepPattern(6,"c"))

## And much more!  

# dynamic time warping works for unequal sparcity
# in theory also for multivariate data
# https://rdatamining.wordpress.com/2011/08/23/time-series-analysis-and-mining-with-r/



m1 <- matrix(rep(1:100,2),ncol = 2)
m2 <- matrix(rep(1:100,2)+runif(200),ncol = 2)
m3 <- matrix(rep(100:1,2),ncol = 2)
sample2<- list(m1,m2,m3)

distMatrix <- dist(sample2, method="DTW")
hc <- hclust(distMatrix, method="average")
plot(hc)
# perhaps it will work!
# implement using days

sample<- split(test[,1:3], f=list(test$trip,test$time_day), drop = T)

for(i in 1:length(sample)){
  if(sample[[i]][1,3] == 1){
    sample[[i]] <- sample[[i]]%>% arrange(-row_number())  
  }
  sample[[i]] <- sample[[i]][,1:2]
}

distMatrix <- dist(sample, method="DTW") #create a distance matrix
hc <- hclust(distMatrix, method="single") #hierarchical clustering
plot(hc)

h <- 0.2

clust<- data.frame( code = cutree(hc, k = NULL, h = 0.2),
            trip = names(cutree(hc, k = NULL, h = 0.2)))

tripMat <- left_join( test %>%
             mutate( trip = paste0(trip,'.',time_day)),
           clust) %>% #merge the clusters with the data
  filter( code ==1) %>% #filter to cluster 1
  select(lon,lat) %>% 
  as.matrix()

  lpc(tripMat, h = 0.03, scaled = T,control = lpc.control(iter = 1000)) %>% 
  plot()

  leaflet() %>% 
  addTiles() %>%
  addCircles(lng = ~lon, lat = ~lat,
             radius = 30, fillOpacity = 0.02,color = "#18206F",label = ~time,group= ~as.factor(trip2)) %>% 
  addLayersControl(
    overlayGroups = ~as.factor(trip2),
    options = layersControlOptions(collapsed = FALSE)
  )
  
  # algorithm to draw points every x meters
  
  x<- lpc(tripMat, h = 0.03, scaled = T,control = lpc.control(iter = 1000))
  
  data <- unscale(x)[["LPC"]]
  start <- home
  space  <- 100
  end <- uithof
  
spacePath <- function(data = x,start = home, end = uithof, space = 100){
  i <- 1
  points <- list()
  distanceFromEnd <- raster::pointDistance(data[1,], end, longlat = T)
  
  while(distanceFromEnd > space){
    distance <-  raster::pointDistance(data, start, longlat = T)
    indx <- which(distance/space>=1)[1]
    if(is.na(indx)){break}
    points[[i]] <- data[indx,]
    distanceFromEnd <- raster::pointDistance(data[indx,], end, longlat = T)
    start <- data[indx,]
    data <- data[indx:nrow(data),]
    i <-  i+1
  }
  points<- matrix(unlist(points),ncol = 2, byrow = T)
  return(points)
}

spaced<- spacePath(data = data, start = home, end = uithof, space = 100)
plot(spaced)

# bin points 

tripMat
spaced

dists<- raster::pointDistance(tripMat,spaced,longlat = T)
trip<- data.frame(tripMat, index = apply(dists,MARGIN = 1, FUN = which.min))
spaced <- data.frame(spaced, index = 1:nrow(spaced))
colnames(spaced[,1:2]) <- c("sLon","sLat")
trip<- left_join(trip,spaced)

ggplot(trip, aes(x=lon, y=lat, color=factor(index))) +
  geom_point(shape=1,alpha = 0.2) +
  geom_point(data = spaced, aes( x = sLon, y= sLat)) +theme_bw()

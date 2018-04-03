# Path drawing function based on local curves

drawPath <- function(tripMat,start,end,h=0.1,t0=0.1,endWeight= 1000,clustMap = clustMap){
  library(LPCM)
  x0 <- filter(clustMap, clust %in% c(start,end)) %>% select(lon,lat) %>% as.matrix()
  
  tripMat<- NLlatlon2UTM(tripMat)
  x0<- NLlatlon2UTM(x0)

  #something funny with weights?
  tripMat <- rbind(x0,tripMat)
  w <- rep(1,nrow(tripMat))
  w[1:2] <- endWeight # weight the ends
  
  # set bandwith and t0 depending on length of trip and frequency
  
  data<- lpc(tripMat, h = 0.25, t0 = t0,x0[1,], pen = 0,control = lpc.control(iter = 1000), weights = w)
  
  #idea, take weights for start and end and also use accuracy. 
  # use algorithm as in the other paper to play around with weights.
  # calculate distance between the two
  plot(data, asp = T)
  data<- unscale(data)[["LPC"]]
  return(data)
}

drawPath(tripList[[1]], start =start, end = end, t0 = 10, clustMap = clustMap)

tripMat <- tripList[[1]]

x<- lpc(tripMat, h = 0.1, t0 = 0.1, pen = 0,control = lpc.control(iter = 1000), weights = w, scaled = F)

data <- unscale(x)[["LPC"]]
plot(data, asp = T)

data(gvessel)
gvessel.self <-lpc.self.coverage(gvessel[,c(2,4,5)], x0=c(35, 1870,
                                                          6.3), print=FALSE, plot.type=0)
h <- select.self.coverage(gvessel.self)$select
gvessel.lpc <- lpc(gvessel[,c(2,4,5)], h=h[1],  x0=c(35, 1870, 6.3))
lpc.coverage(gvessel.lpc, gridsize=10, print=FALSE)

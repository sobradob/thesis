# function which returns data frame that makes the evaluation of Ian's model simple

evalIan <- function(out3,downsampledIan){
  
  pauses<- which(out3[,1] == 2)
  (duration <- (out3[pauses,"t1"]-out3[pauses,"t0"])/300)
  
  matList <- list()
  for(i in 1:length(pauses)){
    currentPause <- pauses[i]
    currentPauseStart <- out3[currentPause,"t0"]
    currentPauseEnd <- out3[currentPause,"t1"]
    mat <- matrix(rep(out3[currentPause,c("x0","y0")], duration[i]),ncol = 2, byrow = T)
    mat <-cbind(mat, seq(from = currentPauseStart, to = currentPauseEnd, by = 300))
    colnames(mat) <- c("x","y","ts")
    matList[[i]] <- as.data.frame(mat)
  }
  allPause<- bind_rows(matList)
  
  # solve code 1: flights in a format that can be leftjoined with the downsampled
  
  flights<- which(out3[,1] == 1)
  duration <- ((out3[flights,"t1"]-out3[flights,"t0"])/300)-1
  matList <- list()
  for(i in 1:length(flights)){
    currentFlight <- flights[i]
    start <- out3[currentFlight,c("x0","y0")]
    end <- out3[currentFlight,c("x1","y1")]
    currentFlightStart <- out3[currentFlight,"t0"]
    currentFlightEnd <- out3[currentFlight,"t1"]
    
    x0<- as.numeric(start[1]) + as.numeric((end[1]-start[1])/(duration[i]+1))*(1:as.numeric(duration[i]))
    y0<- as.numeric(start[2]) + as.numeric((end[2]-start[2])/(duration[i]+1))*(1:as.numeric(duration[i]))
    ts <- seq(from = currentFlightStart, to = currentFlightEnd, by = 300)
    move<- cbind(x0,y0)
    
    if (duration[i] < 1){
      mat<- cbind(rbind(start,end),ts)
    }else{
      mat <- cbind(rbind(start,move,end),ts)
    }
    matList[[i]] <- as.data.frame(mat)
  }
  
  allFlight<- bind_rows(matList)
  colnames(allFlight) <- c("x","y",'ts')
  
  allIan <- rbind(allFlight,allPause) %>% arrange(ts)
  
  # start evaluating
  evalDf<- left_join(downsampledIan,allIan, by = c("timestampMs" = "ts"))%>%
    mutate( distA = raster::pointDistance(p1 = as.matrix(cbind(x_v,y_v)), p2 = as.matrix(cbind(x,y)),lonlat = F ))
  return(evalDf)
}


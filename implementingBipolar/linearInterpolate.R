# Linear interpolate
# simple function with Euclidean distance
# not suitable for larger distances

linearInterpolate <- function(start,
                              end,
                              delta_t = delta_t,
                              interval = 300){
  
  if(floor(delta_t/interval)==0){
  return()  
  }
  
  iLon<- as.numeric(start[1]) + as.numeric((end[1]-start[1])/delta_t)*(1:as.numeric(delta_t/interval))
  iLat<- as.numeric(start[2]) + as.numeric((end[2]-start[2])/delta_t)*(1:as.numeric(delta_t/interval))
  interpolated<- cbind(lon = iLon, lat =iLat)
  colnames(interpolated) <- c("lon","lat")
  return(interpolated)
}

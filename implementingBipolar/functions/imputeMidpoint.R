imputeMidpointPalmius <- function(tMissStart = tMissStart,
                                  tMissEnd = tMissEnd,
                                  locMissStart = locMissStart,
                                  locMissEnd   = locMissEnd,
                                  delta_t = delta_t,
                                  midpoint = midpoint,
                                  ...){
  
  #time to midpoint from begin while travelling at 80kmh
  t_prime<- raster::pointDistance(midpoint,
                                  locMissStart,
                                  lonlat = T)/22.23
  if( delta_t <= 2* t_prime){
    # evenly spaced points between missing two ends
    cat("Simple interpolation \n")
    imp <- linearInterpolate(locMissStart,locMissEnd,delta_t)
  }else{
    # goes to middle
    cat("Midpoint wait interpolation \n")
    imp1 <- linearInterpolate(locMissStart,midpoint,t_prime/2)
    imp2 <- linearInterpolate(midpoint,midpoint,delta_t-t_prime)
    imp3 <- linearInterpolate(midpoint,locMissEnd,t_prime/2)
    imp <- rbind(imp1,imp2,imp3)
  }
  return(imp)
}
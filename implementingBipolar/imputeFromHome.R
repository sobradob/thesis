# Impute distance from Home

imputeFromHome <- function(tMissStart = tMissStart,
                         tMissEnd = tMissEnd,
                         locMissStart = locMissStart,
                         locMissEnd   = locMissEnd,
                         delta_t = delta_t,
                         distEndHome = distEndHome){
  #time to midpoint from begin while travelling at 80kmh
  t_prime<- distEndHome/22.23
  
  if(delta_t <= t_prime){
    imp <- linearInterpolate(locMissStart,locMissEnd,delta_t)
  }else if(delta_t-t_prime<= 2*60*60){
    
    imp1 <- linearInterpolate(locMissStart,locMissEnd,delta_t - t_prime)
    imp2 <- linearInterpolate(locMissEnd,locMissEnd,t_prime)
    imp <- rbind(imp1,imp2)
  }else{
    imp3 <- linearInterpolate(locMissEnd,locMissEnd,2*60*60)
    tMissEnd <- tMissEnd-2*60*60
    if(tMissEnd- tMissStart-t_prime - 2*60*60 >30*60){
      t_cross <- min(2*60*60,as.numeric(tMissEnd- tMissStart-t_prime - 2*60*60))
      imp1 <- linearInterpolate(locMissStart,locMissStart,tMissEnd-t_cross)
      tMissStart <- t_cross
    }else{
      imp1 <- NULL
    }
    imp2 <- linearInterpolate(locMissStart,locMissEnd,tMissEnd-tMissStart)
    imp <- rbind(imp1,imp2,imp3)
    return(imp[-1])
  }
  
  #check order of imputation, is it fine?
  return(imp)
}
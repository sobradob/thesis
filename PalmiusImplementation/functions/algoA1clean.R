# Palmius Algorithm A1
imputePalmiusA1 <- function(distEndHome = distEndHome,
                            distStartHome = distStartHome,
                            delta_t = delta_t,
                            hour24 = hour24,
                            locMissStart = locMissStart,
                            locMissEnd = locMissEnd,
                            delta_dist = delta_dist,
                            midpoint = midpoint,
                            tMissEnd = tMissEnd,
                            tMissStart = tMissStart){
  # prior functions to massage the data before
  imp <- NA
  type <- "None"
  if (delta_dist < 1000 & (delta_t <= 6*60*60 | (hour24 >= 21 & delta_t <= 12 * 60 * 60))) {
    cat("Midpoint interpolation 1 \n")
    imp <- imputeMidpointPalmius(tMissStart = tMissStart,
                                 tMissEnd = tMissEnd,
                                 locMissStart = locMissStart,
                                 locMissEnd = locMissEnd,
                                 midpoint = midpoint,
                                 delta_t = delta_t)
    type <- "Midpoint Imputatation 1"
    
    
  } else if (distStartHome < 750 & distEndHome < 750 & (hour24 > 21 & delta_t <= 18 * 60 * 60)) {
    cat("Midpoint interpolation 2 \n")
    imp <- imputeMidpointPalmius(tMissStart = tMissStart,
                                 tMissEnd = tMissEnd,
                                 locMissStart = locMissStart,
                                 locMissEnd = locMissEnd,
                                 midpoint = midpoint,
                                 delta_t = delta_t)
    type <- "Midpoint Imputatation 2"
  } else if (distStartHome < 250 & distEndHome < 250 & (hour24 >= 21 & delta_t <= 18 * 60 * 60)) {
    cat("Midpoint interpolation 3 \n")
    imp <- imputeMidpointPalmius(tMissStart = tMissStart,
                                 tMissEnd = tMissEnd,
                                 locMissStart = locMissStart,
                                 locMissEnd = locMissEnd,
                                 midpoint = midpoint,
                                 delta_t = delta_t)
    type <- "Midpoint Imputatation 3"
    
  } else if (distStartHome > 750 & distEndHome < 750 & (delta_t < 6 * 60 * 60 | (hour24 >= 20 & delta_t <= 18 * 60 * 60))) {
    cat("To Home interpolation \n")
    imp <- imputeToHome(tMissStart = tMissStart,
                        tMissEnd = tMissEnd, 
                        locMissStart = locMissStart,
                        locMissEnd = locMissEnd,
                        delta_t = delta_t,
                        distStartHome = distStartHome)
    type <- "toHomeImputatation 1"
  } else if (distStartHome < 750 & distEndHome > 750 & (delta_t < 6 * 60 * 60 | (hour24 >= 20 & delta_t <= 18 * 60 * 60))) {
    cat("From Home interpolation \n")
    imp <- imputeFromHome(tMissStart = tMissStart,
                          tMissEnd = tMissEnd,
                          locMissStart = locMissStart,
                          locMissEnd   = locMissEnd,
                          delta_t = delta_t,
                          distEndHome = distEndHome)
    type <- "From HommeImputatation 2"
  }
  result<- list(imp,type)
  
  return(result)
}

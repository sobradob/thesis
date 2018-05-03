palmiusImputeLoop <- function(inputDF){
  
  timeNow <- Sys.time()
  indxFirstMissing<- which(inputDF$firstMissing == 1)
  
  inputDF$impLon <- NA
  inputDF$impLat <- NA
  inputDF$impType <- NA
  mismatch <- 0
  mismatchList <- list()
  for(i in indxFirstMissing){
    cat(i)
    data<- inputDF[i,]
    # extract variables
    tMissEnd<- data["tMissEnd"]
    tMissStart<- data["tMissStart"]
    
    #distance to end etc
    distEndHome <- data["distEndHome"]
    distStartHome <- data["distStartHome"]
    
    #location missing end and beginning
    locMissEnd<- data[c("lonMissEnd", "latMissEnd")]
    locMissStart<- data[c("lonMissStart", "latMissStart")]
    
    
    #Hour in PM needed
    hour24 <- data["hour24"]
    delta_dist <- data["delta_dist"]
    
    # Calculate time elapsed
    delta_t <- tMissEnd - tMissStart
    # length of missing intervals
    lengthMiss <- (delta_t/interval)-1 
    
    # Calculate midpoints
    midpoint <- colMeans(rbind(locMissStart,setNames(locMissEnd, names(locMissStart))))
    
    #impute 
    imputed <- imputePalmiusA1(distEndHome = distEndHome,
                               distStartHome = distStartHome,
                               delta_t = delta_t,
                               hour24 = hour24,
                               locMissStart = locMissStart,
                               locMissEnd = locMissEnd,
                               delta_dist = delta_dist,
                               midpoint = midpoint,
                               tMissEnd = tMissEnd,
                               tMissStart = tMissStart)
    inputDF[(i:as.numeric(i+lengthMiss-1)),"impType"] <- imputed[[2]]
    imputed <- imputed[[1]]
    #end result for loop
    #NA breaks it
    if(is.na(imputed)){next}
    
    if(dim(imputed)!= dim(inputDF[i:as.numeric(i+lengthMiss-1),c("impLon","impLat")])){
      cat(paste0("dimension mismatch \n"))
      mismatch <- mismatch + 1
      mismatchList[mismatch] <- nrow(imputed)-length(i:as.numeric(i+lengthMiss-1))
      imputed <- matrix(colMeans(imputed),ncol = 2)
    }
    inputDF[i:as.numeric(i+lengthMiss-1),c("impLon")] <- imputed[,1]
    inputDF[i:as.numeric(i+lengthMiss-1),c("impLat")] <- imputed[,2]
    print(inputDF[i:as.numeric(i+lengthMiss-1),c("impLon","impLat")])

  }
  timeEnd <- Sys.time()
  
  cat(paste0("Started ",timeNow," finished ",timeEnd,"\n the mismatch is :",mismatch," times\n mean mismatch of ", mean(unlist(mismatchList))))
  return(inputDF)
}
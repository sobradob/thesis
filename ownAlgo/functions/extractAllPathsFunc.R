# Extract all paths & bin

# bin measurements

# to do: code all measurements into bins.

# be clever about pause bins & data drift

# write up stuff & send in one week
extractAllPaths<- function(test3,routeMap, d = 150, maxAcc = 100, minVisit = 2){
  allRoute <- list()
  allRouteClusts <- list()
  for(i in 1:nrow(routeMap)){
    
    start <- routeMap[[i,1]]
    end <- routeMap[[i,2]]
    route <- paste0(
      min(start,end),"-",
      max(start,end))
    cat(paste0("Running ",route," which is ",i," out of ", nrow(routeMap),"\n"))
    if((route %in% allRoute) |routeMap[[i,3]]==1){
      cat("skipped...\n")
      next}
    rPoints<- mergePointsRoute(data= test3, start = start, end = end, d = d, maxAcc = maxAcc, minVisit = minVisit) %>%
      mutate(route = route)
    allRouteClusts[[i]] <- rPoints
    allRoute[[i]] <- route
  }
  allRouteClusts <- bind_rows(allRouteClusts)
  return(allRouteClusts)
}


#not doen
extractAllPaths2<- function(toClust, d = 150, maxAcc = 100){
  
  #BIN BY CLUSTER and then work on clusters
  allRoute <- list()
  allRouteClusts <- list()
  for(i in 1:nrow(routeMap)){
    
    start <- routeMap[[i,1]]
    end <- routeMap[[i,2]]
    route <- paste0(
      min(start,end),"-",
      max(start,end))
    cat(paste0("Running ",route," which is ",i," out of ", nrow(routeMap),"\n"))
    if((route %in% allRoute) |routeMap[[i,3]]==1){
      cat("skipped...\n")
      next}
    rPoints<- mergePointsRoute(data= test3, start = start, end = end, d = d, maxAcc = maxAcc, minVisit = minVisit) %>%
      mutate(route = route)
    allRouteClusts[[i]] <- rPoints
    allRoute[[i]] <- route
  }
  allRouteClusts <- bind_rows(allRouteClusts)
  return(allRouteClusts)
}

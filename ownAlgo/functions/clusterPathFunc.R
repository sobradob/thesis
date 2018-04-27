
clusterPaths <- function(data,start,end, thickenParam = "1 day", h = 0.2, clustMap = clusterMap){
  library(padr)
  library(dtw)
  library(LPCM)
  d1<- data %>% 
    filter((nextPauseClust == start &  prevPausClust == end)|
             (nextPauseClust == end &  prevPausClust == start)) %>% 
    mutate(trip = case_when( nextPauseClust == start ~ 1,
                             nextPauseClust == end ~ 0)) %>% 
    thicken(by = "time",thickenParam)
  
  sample<- split(d1[,c("trip","lon","lat")], f=list(d1$trip,d1$time_day), drop = T)
  
  for(i in 1:length(sample)){
    if(sample[[i]][1,3] == 1){
      sample[[i]] <- sample[[i]]%>% arrange(-row_number())  
    }
    sample[[i]] <- sample[[i]][,c("lon","lat")]
  }
  tripMatList <- list()
  if( length(sample)> 2){
    distMatrix <- dist(sample, method="DTW") #create a distance matrix
    hc <- hclust(distMatrix, method="single") #hierarchical clustering
    # take path between two clusters
    clust<- data.frame( code = cutree(hc, k = NULL, h = h),
                        trip = names(cutree(hc, k = NULL, h = h)))
    n_clust<- unique(clust$code)
    for(i in n_clust){
      
      tripMatList[[i]]<- left_join( d1 %>%
                                    mutate( trip = paste0(trip,'.',time_day)),
                                  clust) %>% #merge the clusters with the data
        filter( code ==i) %>% #filter to cluster 1
        select(lon,lat) %>% 
        as.matrix()
    }
  }else{
    tripMatList[[1]] <- d1 %>% select(lon,lat) %>% as.matrix()
  }
  cat(paste0("You have ", length(sample), " independent trips with ", length(tripMatList), " unique path cluster"))
  return(tripMatList)
}

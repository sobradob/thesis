# calculates distances between real and imputed

calcImputeDist <- function(imputedP,remove_ind, downsampled){
  #calculate mean and median
  imp5<- imputedP %>% select(impLon,impLat) %>%
    slice(remove_ind)
  actual<- downsampled[remove_ind,c("lonF","latF")]
  dists<- raster::pointDistance(as.matrix(imp5),as.matrix(actual), lonlat = T)
  
  cat(paste0(dists %>% is.na() %>% sum()," not imputed \n"))
  print(summary(na.omit(dists)))
}
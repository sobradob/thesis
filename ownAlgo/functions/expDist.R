# calculate expected distance

expDist <- function(model,x_test,dClustM){
  
  probs <- model %>% predict_proba(as.matrix(x_test))
  
  dOrdered<- dClustM[,t %>% select(clust) %>% pull()]
  expDistance<- diag(probs[,-1] %*% dOrdered)
  return(expDistance)
}
# this function converts matrices of a certain form to flight, pause, missing or in between missing

pauseFlight <- 
  function(data,r, w){
    avgmat <- as.matrix(data)
    #added line to stop error where end time is missing
    indx <- which(avgmat[,1] == 4)
    avgmat[indx,3] <- avgmat[indx+1,2] 

    outmat=c()
    curind=1
    cat("Convert from X/Y to flights/pauses...\n") # here comes the amazing flight extracting algo
    for(i in 1:nrow(avgmat)){
      #ProgressBar(nrow(avgmat),i)
      if(avgmat[i,1]==4){
        outmat=rbind(outmat,ExtractFlights(avgmat[curind:(i-1),c(5,6,2)],r,w),
                     c(avgmat[i,1],NA,NA,avgmat[i,2],NA,NA,avgmat[i,3]))
        curind=i+1
      }
    }
    if(curind<=nrow(avgmat)){  #extract last bit
      outmat=rbind(outmat,ExtractFlights(avgmat[curind:nrow(avgmat),c(4,3,2)],r,w))# why not work?
    }
    rownames(outmat)=NULL
    colnames(outmat)=c("Code","x0","y0","t0","x1","y1","t1") # code stands for missing (4), pause (2) or flight (1) 
    
    return(outmat)
  }

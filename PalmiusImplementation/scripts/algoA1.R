# Palmius Algorithm A1

# Calculate time elapsed
delta_t <-  tMissEnd-tMissStart  
delta_dist <- delta_dist # calculate distance

if( delta_dist < 1000 & 
    (delta_t <= 6 | (tMissStart >= 9 & delta_t <= 12*60*60))){
  
  imputeMidpointPalmius(tMissStart = tMissStart,
                        tMissEnd = tMissEnd,
                        locMissStart = locMissStart,
                        locMissEnd   = locMissEnd)
  
}else if(distStartHome < 750 & distEndHome < 750 & (tMissStart > 9 & delta_t <=18*60*60)){
  
  imputeMidpointPalmius(tMissStart = tMissStart,
                        tMissEnd = tMissEnd,
                        locMissStart = locMissStart,
                        locMissEnd   = locMissEnd)
  
}else if(distStartHome < 250 & distEndHome < 250 &
         (tMissStart >= 9 & delta_t <= 18*60*60)){
  
  imputeMidpointPalmius(tMissStart = tMissStart,
                        tMissEnd = tMissEnd,
                        locMissStart = locMissStart,
                        locMissEnd   = locMissEnd)
  
}else if(distStartHome > 750 & distEndHome < 750 &
        (delta_t < 6*60*60 | (tMissStart >= 8 & delta_t <= 18*60*60))){
           
           travellingToHomePalmius(tMissStart = tMissStart,
                                   tMissEnd = tMissEnd,
                                   locMissStart = locMissStart,
                                   locMissEnd   = locMissEnd)
           
         }else if(distStartHome < 750 & distEndHome > 750 &
                  (delta_t < 6*60*60 | (tMissStart >= 8 & delta_t <= 18*60*60))){
                    
                    travellingFromHomePalmius(tMissStart = tMissStart,
                                              tMissEnd = tMissEnd,
                                              locMissStart = locMissStart,
                                              locMissEnd   = locMissEnd)
                  }
            
# extract missing values\
# unfinished, using the nnFeatureFunc to do this


library(padr)
library(tidyr)
data2 %>% top_n(250) %>% 
  select(time, clust, timestampMs,timestampMs)%>%
  thicken( interval = "60 sec") %>% 
  pad(by = "time_60_sec") %>% 
  mutate(timestampLast = as.numeric(time),
         timestampNext = as.numeric(time),
         timestampCurr = as.numeric(time_60_sec),
         nextClust = clust,
         prevClust = clust) %>% 
  fill(timestampLast,prevClust, .direction = "down") %>% 
  fill(timestampNext,nextClust, .direction = "up") %>%
  mutate( secLast = timestampCurr - timestampLast,
          secNext = timestampNext - timestampCurr) 


# turn the above into a function


# take the time and the timestamp, with NA's

train <- data  %>%
  slice(remove_ind) %>%
  select(time, timestampMs)
  
  
  

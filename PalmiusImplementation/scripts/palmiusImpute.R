# unify missing fuction

data<- d.f1

which(is.na(data$lonF))
library(tidyr)
d1 <- data %>% mutate( timestampMs = as.numeric(time_5min),
                 tMissEnd = case_when(
                   is.na(lonF) ==  T~ NA_real_,
                  !is.na(lonF)== T ~ timestampMs),
                 tMissStart = tMissEnd,
                 lonMissEnd =case_when(
                   is.na(lonF) ==  T~ NA_real_,
                   !is.na(lonF)== T ~ lonF),
                 lonMissStart = lonMissEnd,
                 latMissEnd = case_when(
                   is.na(lonF) ==  T~ NA_real_,
                   !is.na(lonF)== T ~ latF),
                 latMissStart = latMissEnd) %>% 
  fill(tMissEnd,lonMissEnd,latMissEnd, .direction ="up") %>% 
  fill(tMissStart,lonMissStart,latMissStart, .direction ="down")

# missing: distance to home

imputePalmiusA1

#testing script
tMissStart <- as.numeric(d1[35,"tMissStart"])
tMissEnd <- as.numeric(d1[35,"tMissEnd"])

lonMissEnd <- as.numeric(d1[35,"lonMissEnd"])
lonMissStart <- as.numeric(d1[35,"lonMissStart"])

latMissEnd <- as.numeric(d1[35,"latMissEnd"])
latMissStart <- as.numeric(d1[35,"latMissStart"])

tMissStart <- as.numeric(d1[129,"tMissStart"])
tMissEnd <- as.numeric(d1[129,"tMissEnd"])

lonMissEnd <- as.numeric(d1[129,"lonMissEnd"])
lonMissStart <- as.numeric(d1[129,"lonMissStart"])

latMissEnd <- as.numeric(d1[129,"latMissEnd"])
latMissStart <- as.numeric(d1[129,"latMissStart"])
locMissStart = c(lonMissStart,latMissStart)
locMissEnd   = c(lonMissEnd,latMissEnd)

imputeMidpointPalmius(tMissStart = tMissStart,
                      tMissEnd = tMissEnd,
                      locMissStart = locMissStart,
                      locMissEnd   = locMissEnd)

# algorithm A1
# needs distance from home

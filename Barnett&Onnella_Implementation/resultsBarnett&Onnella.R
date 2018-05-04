# Full results of Barnett & Onnella's Method
# this section cannot be reproduced without the functions  & files provided by the authors of the methods
# please contact them for their scripts


# Load needed functions
source("/functions/downSample.R")
source("/functions/pauseFlight.R")

# Load Barnett & Onnella's functions
file.sources = list.files(pattern="*.R",path = "gpsmobility/R/")
setwd("gpsmobility/R/")
sapply(file.sources,source,.GlobalEnv)

# select the needed data from the final pre-processed data
data <- fin %>% select(timestampMs,time,lat,lon,accuracy)

#downsamples and adds XY coordinates
downsampledIan <- downSampleMean(data,interval = '5 min') 


# remove data for results
## remove 5 minute bits
## this takes approximately an hour on a macbook pro

# take the time periods
remove_ind_5min<- getRemoveIndex(fin,"5 min")

# create a remove index
d2<- downsampledIan %>%
  mutate( time = as.POSIXct(timestampMs, origin="1970-01-01")) %>% 
  pull(time)

remove_ind <- which(as.character(d2) %in% remove_ind_5min[[2]])

# removed the selected time periods
downsampled5minRemoved<- downsampledIan
downsampled5minRemoved[remove_ind,c("lon","lat","x_v","y_v")] <- NA
downsampled5minRemoved[remove_ind,c("code")] <- 4

# set parameters
minpausedur=120
minpausedist=50


# aggregate into pauses and flights
mobmatmiss<- pauseFlight(downsampled5minRemoved, r = sqrt(300), w = mean(data$accuracy)) 

# clear the data: the last row is erroneous
mobmatmiss <- mobmatmiss[-nrow(mobmatmiss),] 

# Guess pauses 
mobmat = GuessPause(mobmatmiss,mindur=minpausedur,r=minpausedist)

# initialise parameters

obj=InitializeParams(mobmat)

# set spread parameters
spread_pars=c(10,1)
wtype <- "TL"

# calculate mobility gaps
out3 <- SimulateMobilityGaps(mobmat,obj,wtype,spread_pars)

# deaggregate from pauses & flights to 5 minute window points and calculate distance
evalDf<- evalIan(out3,downsampledIan = downsampledIan)

# inspect distance measures
evalDf %>% filter( timestampMs %in% (downsampledIan[remove_ind,"timestampMs"] %>%
                                       pull())) %>% 
  unique() %>%
  summary()


# Inspect deviance measures and their relation to accuracy

devianceIan <- data %>% thicken("5 min") %>% 
  mutate(time_5_min = as.numeric(time_5_min)) %>%
  select(time,accuracy,time_5_min,x_orig,y_orig) %>% 
  left_join(.,evalDf, by = c("time_5_min" = "timestampMs")) %>%
  mutate(distB = raster::pointDistance(p1 = as.matrix(cbind(x_orig,y_orig)), p2 = as.matrix(cbind(x,y)),lonlat = F ))

# Calculate what percentage of values are within accuracy
devianceIan %>% summarise( mean( accuracy >= distA, na.rm = T)) 

# Calculate average deviance measures
summary(devianceIan) 


# do the same for the one hour period

remove_ind_hour<- getRemoveIndex(fin,"1 hour",seed = 2003)
removedHours<- remove_ind_hour[[2]]

# take the time periods
d2<- downsampledIan %>%
  mutate( time = as.POSIXct(timestampMs, origin="1970-01-01")) %>% 
  thicken("hour","hour")

# create a remove index
remove_ind1hr <- which(as.character(d2$hour) %in% removedHours)

# removed the selected time periods
downsampled1hrRemoved<- downsampledIan
downsampled1hrRemoved[remove_ind1hr,c("lon","lat","x_v","y_v")] <- NA
downsampled1hrRemoved[remove_ind1hr,c("code")] <- 4

# aggregate into pauses and flights
mobmatmiss1hr <- pauseFlight(downsampled1hrRemoved, r = sqrt(300), w = mean(data$accuracy))

# clear the data: the last row is erroneous
mobmatmiss1hr <- mobmatmiss1hr[-nrow(mobmatmiss1hr),]

# guess pauses
mobmat1h <- GuessPause(mobmatmiss1hr,mindur=minpausedur,r=minpausedist)

# initialise parameters
obj <- InitializeParams(mobmat2)
spread_pars <- c(10,1)
wtype <- "TL"

# impute missing
out31hr <- SimulateMobilityGaps(mobmat2,obj,wtype,spread_pars)

# deaggregate from pauses & flights to 5 minute window points and calculate distance
evalDf1hr<- evalIan(out31hr,downsampledIan = downsampledIan)

# inspect distance measures
evalDf1hr %>% filter( timestampMs %in% (downsampledIan[remove_ind1hr,"timestampMs"] %>% pull())) %>% 
  unique() %>%
  summary()


# Again, but for one day.

# create an index of removed days
remove_ind_day<- getRemoveIndexDay(fin, seed = 1115)
removedDays<- remove_ind_day[[2]]

d2<- downsampledIan %>%
  mutate( time = as.POSIXct(timestampMs, origin="1970-01-01")) %>% 
  thicken("day","day")

remove_indday <- which(as.Date(d2$day) %in% as.Date(removedDays))

# remove days from downsampled input
downsampled1dayRemoved<- downsampledIan
downsampled1dayRemoved[remove_indday,c("lon","lat","x_v","y_v")] <- NA
downsampled1dayRemoved[remove_indday,c("code")] <- 4

# extracts pauses and flights
mobmatmissday <- pauseFlight(downsampled1dayRemoved, r = sqrt(300), w = mean(data$accuracy)) 

#clean the last row which is erroneous
mobmatmissday <- mobmatmissday[-nrow(mobmatmissday),]

#guess pause locations
mobmatday <- GuessPause(mobmatmissday,mindur=minpausedur,r=minpausedist)

# initialise parameters for imputation
obj <- InitializeParams(mobmatday)
spread_pars <- c(10,1)
wtype <- "TL"

# impute mobility gaps
out3day <- SimulateMobilityGaps(mobmat,obj,wtype,spread_pars)

# create evaluation dataframe
evalDfday<- evalIan(out3day,downsampledIan = downsampledIan)

# check results
evalDfday %>%
  filter( timestampMs %in% (downsampledIan[remove_indday,"timestampMs"] %>% 
                              pull())) %>% 
  unique() %>%
  summary()


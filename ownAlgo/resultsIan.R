# full Ian results 
directory <- "/Users/boazsobrado/Desktop/Academic/Utrecht/year2/thesisFiles/harvard/Example"
setwd(directory)


#from finite
data <- fin %>% as_tbl_time(time) %>% filter_time(~"2017-03")%>% select(timestampMs,time,lat,lon,accuracy)

# Load needed functions
source("/Users/boazsobrado/Desktop/Academic/Utrecht/year2/thesisFiles/thesis/scripts/downSample.R")
source("/Users/boazsobrado/Desktop/Academic/Utrecht/year2/thesisFiles/thesis/scripts/pauseFlight.R")
file.sources = list.files(pattern="*.R",path = "gpsmobility/R/")
setwd("gpsmobility/R/")
sapply(file.sources,source,.GlobalEnv)

data <- fin %>% select(timestampMs,time,lat,lon,accuracy)
downsampledIan <- downSampleMean(data,interval = '300 sec') #downsamples and adds XY coordinates


# remove data for results

# 5 minute bits

Sys.time()
d2<- downsampledIan %>%
  mutate( time = as.POSIXct(timestampMs, origin="1970-01-01")) %>% 
  pull(time)

remove_ind <- which(as.character(d2) %in% remove_ind_5min[[2]])

downsampleSys.time()
d5minRemoved<- downsampledIan
downsampled5minRemoved[remove_ind,c("lon","lat","x_v","y_v")] <- NA
downsampled5minRemoved[remove_ind,c("code")] <- 4

#params
minpausedur=120
minpausedist=50
Sys.time()
mobmatmiss<- pauseFlight(downsampled5minRemoved, r = sqrt(300), w = mean(data$accuracy)) # extracts pauses and flights using algo
Sys.time()
mobmatmiss <- mobmatmiss[-nrow(mobmatmiss),] #clean a bit

# Guessing Pauses
Sys.time()
mobmat = GuessPause(mobmatmiss,mindur=minpausedur,r=minpausedist)
Sys.time()
obj=InitializeParams(mobmat)
spread_pars=c(10,1)
wtype <- "TL"
Sys.time()
out3 <- SimulateMobilityGaps(mobmat,obj,wtype,spread_pars) # it is returning NA's only for

evalDf<- evalIan(out3,downsampledIan = downsampledIan)

# 5 minute thing

evalDf[remove_ind,] %>% summary() # median 5 meters mean 141


#distA is the deviance score

#check within accuracy 

devianceIan <- data %>% thicken("5 min") %>% 
  mutate(time_5_min = as.numeric(time_5_min)) %>%
  select(time,accuracy,time_5_min,x_orig,y_orig) %>% 
  left_join(.,evalDf, by = c("time_5_min" = "timestampMs")) %>%
  mutate(distB = raster::pointDistance(p1 = as.matrix(cbind(x_orig,y_orig)), p2 = as.matrix(cbind(x,y)),lonlat = F ))

devianceIan %>% summarise( mean( accuracy >= distA, na.rm = T)) # approx 86% within accuracy 

plotIan <- as.data.frame(out3) %>% filter(t0 <= 1488408900) %>% mutate( dur = t1-t0)
plotIan2 <- devianceIan %>% as_tbl_time(time) %>% filter_time(~"2017-03-01")
ggplot(plotIan, aes(x = x0, y = y0))+
  geom_point(aes(size = (dur)))+
  geom_segment(aes(x = x0, y = y0, xend = x1, yend = y1))+
  coord_fixed()+
  theme_tufte()+
  xlab("")+ylab("")+
  geom_point( data = plotIan2 , aes(x = x_orig, y = y_orig), shape =1, colour = "blue")


devianceIan %>%  summarise(mean(accuracy >= distB, na.rm = T)) #86 percent of values are within accuracy 

summary(devianceIan) # mean 343 median 8.21


# 1 hour thing
removedHours<- remove_ind_hour[[2]]


d2<- downsampledIan %>%
  mutate( time = as.POSIXct(timestampMs, origin="1970-01-01")) %>% 
  thicken("hour","hour")

remove_ind <- which(as.character(d2$hour) %in% removedHours)

downsampled1hrRemoved<- downsampledIan
downsampled1hrRemoved[remove_ind,c("lon","lat","x_v","y_v")] <- NA
downsampled1hrRemoved[remove_ind,c("code")] <- 4

#bug if last line contained in removed sample
mobmatmiss <- pauseFlight(downsampled1hrRemoved, r = sqrt(300), w = mean(data$accuracy)) # extracts pauses and flights using algo
mobmatmiss <- mobmatmiss[-nrow(mobmatmiss),] #clean a bit
mobmat <- GuessPause(mobmatmiss,mindur=minpausedur,r=minpausedist)

obj <- InitializeParams(mobmat)
spread_pars <- c(10,1)
wtype <- "TL"
out3 <- SimulateMobilityGaps(mobmat,obj,wtype,spread_pars)
evalDf<- evalIan(out3,downsampledIan = downsampledIan)

evalDf[remove_ind,] %>% summary() # median 6 meters mean 345

# 1 day thing

removedDays<- remove_ind_day[[2]]

d2<- downsampledIan %>%
  mutate( time = as.POSIXct(timestampMs, origin="1970-01-01")) %>% 
  thicken("day","day")

remove_ind <- which(as.Date(d2$day) %in% as.Date(removedDays))

downsampled1dayRemoved<- downsampledIan
downsampled1dayRemoved[remove_ind,c("lon","lat","x_v","y_v")] <- NA
downsampled1dayRemoved[remove_ind,c("code")] <- 4

mobmatmiss <- pauseFlight(downsampled1dayRemoved, r = sqrt(300), w = mean(data$accuracy)) # extracts pauses and flights using algo
mobmatmiss <- mobmatmiss[-nrow(mobmatmiss),] #clean a bit
mobmat <- GuessPause(mobmatmiss,mindur=minpausedur,r=minpausedist)

obj <- InitializeParams(mobmat)
spread_pars <- c(10,1)
wtype <- "TL"
out3 <- SimulateMobilityGaps(mobmat,obj,wtype,spread_pars)
evalDf<- evalIan(out3,downsampledIan = downsampledIan)

evalDf[remove_ind,] %>% summary() # median 7 meters mean 60 not imputed 282 NA's
evalDf[remove_ind,]

#doing this again cuz it takes ages
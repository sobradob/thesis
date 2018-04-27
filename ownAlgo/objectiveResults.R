# objective data comparison

# read in data
Sys.time()# 14:46:00

dir()
library(readr)
objLog<- read_csv2("objectiveLog.csv")
objKey<- read_csv2("objectiveKey.csv") %>% select(Location,Cluster)

objKey<- left_join(objKey,allPoints, by = c("Cluster" = "clust")) %>% select(Location,Cluster,lon,lat)

strftime(paste0(objLog$Date," ",objLog$Time),"%d-%m-%Y %H:%M:%S")



objLog <- objLog %>%
  mutate( time = as.POSIXct(paste(Date, Time), format="%d-%m-%Y %H:%M:%S")) %>% 
  as_tbl_time(time)

objLog <- left_join(objLog,objKey)

# compare MYMETHOD to the objective data
Sys.time()# 15:02:03

test<- objLog %>% mutate(timestampMs = as.numeric(time), clust = Cluster)
seconds_in_day <-  24*60*60

test<- test %>% 
  select(time,timestampMs,clust) %>% 
  mutate( day  = strftime(time,"%u"),
          month = strftime(time,"%m"),
          secMidnight = lubridate::period_to_seconds(lubridate::hms(strftime(time,"%T"))),
          sinTime = sin(2*pi*secMidnight/seconds_in_day),
          cosTime = cos(2*pi*secMidnight/seconds_in_day),
          nextMeas = NA,
          prevMeas = NA,
          lagClust = NA,
          leadClust = NA,
          timestampLast = NA,
          timestampNext = NA,
          clustNext = NA,
          clustPrev = NA,
          isMissing = 1)


train<- fin %>% 
  select(time,timestampMs,clust) %>% 
  mutate( day  = strftime(time,"%u"),
          month = strftime(time,"%m"),
          secMidnight = lubridate::period_to_seconds(lubridate::hms(strftime(time,"%T"))),
          sinTime = sin(2*pi*secMidnight/seconds_in_day),
          cosTime = cos(2*pi*secMidnight/seconds_in_day),
          nextMeas = as.numeric(lead(timestampMs)-timestampMs),
          prevMeas = as.numeric(timestampMs-lag(timestampMs)),
          lagClust = lag(clust),
          leadClust = lead(clust),
          timestampLast = as.numeric(time),
          timestampNext = as.numeric(time),
          clustNext = clust,
          clustPrev = clust,
          isMissing = 0)

all <- rbind(train,test) %>%
  arrange(timestampMs) %>% 
  fill(timestampLast,clustPrev, .direction = "down") %>% 
  fill(timestampNext,clustNext, .direction = "up") %>%
  mutate( prevMeas = timestampMs - timestampLast,
          nextMeas = timestampNext - timestampMs,
          lagClust = clustPrev,
          leadClust = clustNext) %>% 
  mutate(nextMeas = scales::rescale(nextMeas),
         prevMeas = scales::rescale(prevMeas))

test <- all %>% filter(isMissing == 1)
train <- all %>% filter(isMissing == 0)

test <- test %>% na.omit()# remove first and previous lag difference
train <- train %>% na.omit()# remove first and previous lag difference

cat(paste0("Baseline model previous Clust is: ", test %>% summarise(mean(clust == lagClust))))

test %>%
  select(clust,lagClust) %>% 
  left_join(allPoints,by="clust") %>% 
  left_join(allPoints,by=c("lagClust" = "clust"),suffix = c(".clust",".predC")) %>% 
  na.omit() %>% 
  mutate( dist = raster::pointDistance(matrix(c(lon.clust,lat.clust),ncol = 2),
                                       matrix(c(lon.predC,lat.predC),ncol = 2),
                                       longlat = T)) %>% 
  summary()
# median 555, mean 3303

dataListObj<- getModelInput(test = test, train = train)

x_train <- dataListObj[[1]]
x_test  <- dataListObj[[2]]
y_train <- dataListObj[[3]]
y_test  <- dataListObj[[4]]

modelObj <- keras_model_sequential() 
modelObj %>% 
  layer_dense(units = 250, activation = 'relu', input_shape = ncol(x_test)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 120, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = nClust, activation = 'softmax')

modelObj %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- modelObj %>% fit(
  as.matrix(x_train), as.matrix(y_train), 
  epochs = 10, batch_size = 200, 
  validation_split = 0.3
)
#plot(history)

modelObj %>% evaluate(as.matrix(x_test),as.matrix(y_test)) #24% percent accuracy

# distance matrix of all clusters
distMatrix<- geosphere::distm(allPoints[,c("lon","lat")])

t<- objLog %>% mutate(predC = as.integer(modelObj %>% predict_classes(as.matrix(x_test)))) %>%
  select(Cluster, predC,lon,lat)  %>%
  left_join(allPoints,by=c("predC" = "clust"),suffix = c(".clust",".predC")) %>% 
  na.omit() %>% 
  mutate( dist = raster::pointDistance(matrix(c(lon.clust,lat.clust),ncol = 2),
                                       matrix(c(lon.predC,lat.predC),ncol = 2),
                                       longlat = T))

summary(t) # mean 5637, median 1037

probs <- modelObj %>% predict_proba(as.matrix(x_test))
dim(probs)
probs<- probs[,-1] # extra column in hot coding

dOrdered<- distMatrix[,t %>% select(Cluster) %>% pull()]

t$expDist<- diag(probs %*% dOrdered)
t$certainty<- apply(X = probs[,-1],MARGIN = 1,max)

summary(t) # expDist mean 6342, median 1037

Sys.time() # 16:34, found bug in expected distance, going to compute it for the other NN models

# compare palmius to objective data

# compare barnett & onella 
Sys.time() # back, running a comparison with B&O 17:40

Sys.time() # back again, got distracted by the fact that MYMETHOD's errors were being inflated, fixed it by using weights 19:06

removedPeriod<- objLog %>% thicken("5 min", "thick5") %>% pull(thick5) %>% as.numeric()

remove_ind <- which(downsampledIan$timestampMs %in% removedPeriod)

downsampledObj<- downsampledIan
downsampledObj[remove_ind,c("lon","lat","x_v","y_v")] <- NA
downsampledObj[remove_ind,c("code")] <- 4

mobmatmiss <- pauseFlight(downsampledObj, r = sqrt(300), w = mean(data$accuracy)) # extracts pauses and flights using algo
mobmatmiss <- mobmatmiss[-nrow(mobmatmiss),] #clean a bit
mobmat <- GuessPause(mobmatmiss,mindur=minpausedur,r=minpausedist)

obj <- InitializeParams(mobmat)
spread_pars <- c(10,1)
wtype <- "TL"
out3 <- SimulateMobilityGaps(mobmat,obj,wtype,spread_pars)
evalDf<- evalIan(out3,downsampledIan = downsampledIan)

evalDf<- unique(evalDf)
evalDf[remove_ind,] %>% summary() # median 12 meters mean 5791

#get XY Coordinates for those spots
temp<- rbind(objLog %>% select(lon,lat), data %>% select(lon,lat))
temp <- cbind(objLog,as.data.frame(LatLong2XY(temp$lat, temp$lon))[1:nrow(objLog),])
temp<- temp %>% thicken("5 min", "thick5") %>% mutate(timestampMs = as.numeric(thick5))

eval <- left_join(evalDf[remove_ind,], temp, by = "timestampMs")%>%
  mutate( distObj = raster::pointDistance(p1 = as.matrix(cbind(x_v.y,y_v.y)), p2 = as.matrix(cbind(x,y)),lonlat = F ))

summary(eval)# median 1352, mean 5067

# palmius
Sys.time()# 19:50:21

dataExample <- select(fin,time,lat,lon,accuracy,nextMeas) %>%
  arrange(time) %>% 
  mutate( nextLon = lead(lon),
          nextLat = lead(lat),
          index = 1:length(lat),
          nextTimeSec = nextMeas
  ) %>% 
  as_tbl_time(time) %>% 
  filter_time( ~ '2017-03')

filtered<- filteringPalmius(data = dataExample) #no unique locations removed
downsampled <-  downSamplingPalmius(data = filtered) #change name of time column, lat and lon

downsampledObjPalmius<- downsampled
downsampledObjPalmius[remove_ind,c("lonF","latF")] <- NA

interval <- 300
preImp<- featureExtractPalmius(downsampledObjPalmius) #extract features for imputation

imputedP<- palmiusImputeLoop(preImp) # get imputation

calcImputeDist(imputedP,remove_ind,downsampled = downsampled) #median 0, mean 43

imputedP %>% slice(remove_ind) %>%
  select(timestampMs,impLon,impLat) %>%
  left_join(., objLog %>%thicken("5 min", "thick5") %>% mutate(timestampMs = as.numeric(thick5)), by = "timestampMs") %>% 
  mutate( dist = raster::pointDistance(p1 = as.matrix(cbind(impLon,impLat)), p2 = as.matrix(cbind(lon,lat)),lonlat = T )) %>% 
  summary()

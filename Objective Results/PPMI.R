# PPMI results

# create features for model input

# create test variable 
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

# create train variable

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

# bind test and train together to scale appropriately & fill next values
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

# separate into test and train
test <- all %>% filter(isMissing == 1)
train <- all %>% filter(isMissing == 0)

# remove lagged NA's
test <- test %>% na.omit()
train <- train %>% na.omit()

# calculate baseline model naive
cat(paste0("Baseline model previous Clust is: ", test %>% summarise(mean(clust == lagClust))))

# distance measures for naive baseline
test %>%
  select(clust,lagClust) %>% 
  left_join(allPoints,by="clust") %>% 
  left_join(allPoints,by=c("lagClust" = "clust"),suffix = c(".clust",".predC")) %>% 
  na.omit() %>% 
  mutate( dist = raster::pointDistance(matrix(c(lon.clust,lat.clust),ncol = 2),
                                       matrix(c(lon.predC,lat.predC),ncol = 2),
                                       longlat = T)) %>% 
  summary()

# generate model input
dataListObj<- getModelInput(test = test, train = train)

# separate into individual matrices
x_train <- dataListObj[[1]]
x_test  <- dataListObj[[2]]
y_train <- dataListObj[[3]]
y_test  <- dataListObj[[4]]

# define sequential model 
modelObj <- keras_model_sequential() 
modelObj %>% 
  layer_dense(units = 250, activation = 'relu', input_shape = ncol(x_test)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 120, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = nClust, activation = 'softmax')

# compile model
modelObj %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# train model
modelObj %>% fit(
  as.matrix(x_train), as.matrix(y_train), 
  epochs = 10, batch_size = 200, 
  validation_split = 0.3
)

# get model accuracy
modelObj %>% evaluate(as.matrix(x_test),as.matrix(y_test))

# get distance matrix of all clusters
distMatrix<- geosphere::distm(allPoints[,c("lon","lat")])

#Calculate distance measure for all missing timepoints
t<- objLog %>% mutate(predC = as.integer(modelObj %>% predict_classes(as.matrix(x_test)))) %>%
  select(Cluster, predC,lon,lat)  %>%
  left_join(allPoints,by=c("predC" = "clust"),suffix = c(".clust",".predC")) %>% 
  na.omit() %>% 
  mutate( dist = raster::pointDistance(matrix(c(lon.clust,lat.clust),ncol = 2),
                                       matrix(c(lon.predC,lat.predC),ncol = 2),
                                       longlat = T))

# explore results
summary(t) 

# get expected distance
probs <- modelObj %>% predict_proba(as.matrix(x_test))
probs<- probs[,-1]
dOrdered<- distMatrix[,t %>% select(Cluster) %>% pull()]
t$expDist<- diag(probs %*% dOrdered)

# calculate certainty
t$certainty<- apply(X = probs[,-1],MARGIN = 1,max)

# explore results
summary(t) 

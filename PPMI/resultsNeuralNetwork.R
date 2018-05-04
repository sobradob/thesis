# PPMI classification
# neural nets march test results

# distance matrix of all bins
distMatrix<- geosphere::distm(allPoints[,c("lon","lat")])


# get the index of removed 5 minute values
remove_ind_5min<- getRemoveIndex(fin,"5 min")

# separate into test and train sets
testTrain5min<- getTestTrain(fin,remove_ind_5min[[1]])

# get naive baseline distance measures
testTrain5min[[1]] %>%
  select(clust,lagClust) %>% 
  left_join(allPoints,by="clust") %>% 
  left_join(allPoints,by=c("lagClust" = "clust"),suffix = c(".clust",".predC")) %>% 
  na.omit() %>% 
  mutate( dist = raster::pointDistance(matrix(c(lon.clust,lat.clust),ncol = 2),
                                       matrix(c(lon.predC,lat.predC),ncol = 2),
                                       longlat = T)) %>% 
  summary()
  
# transform into model inputs  
dataList5min<- getModelInput(test = testTrain5min[[1]], train = testTrain5min[[2]])

# assing model inputs
x_train <- dataList5min[[1]]
x_test  <- dataList5min[[2]]
y_train <- dataList5min[[3]]
y_test  <- dataList5min[[4]]

# set number of unique clusters
nClust <- 1222

# define and initialise model
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 250, activation = 'relu', input_shape = ncol(x_test)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 120, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = nClust, activation = 'softmax')

# compile
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# fit the model
model %>% fit(
  as.matrix(x_train), as.matrix(y_train), 
  epochs = 10, batch_size = 200, 
  validation_split = 0.2
)

# evaluate accuracy
model %>% evaluate(as.matrix(x_test),as.matrix(y_test))

# extract removed values
t<- fin %>% slice(remove_ind_5min[[1]])

# compute distance
t<- t %>% mutate(
  predC = as.integer(model %>% predict_classes(as.matrix(x_test)))) %>%
  select(clust, predC,time)  %>%
  left_join(allPoints,by="clust") %>% 
  left_join(allPoints,by=c("predC" = "clust"),suffix = c(".clust",".predC")) %>% 
  na.omit() %>% 
  mutate( dist = raster::pointDistance(matrix(c(lon.clust,lat.clust),ncol = 2),
                                       matrix(c(lon.predC,lat.predC),ncol = 2),
                                       longlat = T))


# compute distance expectation
t$expDist <-  expDist(model,x_test,distMatrix)

# compute probabilities
probs <- model %>% predict_proba(as.matrix(x_test))

# get certainties 
certainty<- apply(X = probs[,-1],MARGIN = 1,max)

#explore certainties
summary(certainty)

# weight distance by 5 minute periods

t %>% thicken("5 min","weight5min") %>% 
  group_by(weight5min) %>% 
  summarise(m = mean(dist),
            ed = mean(expDist)) %>% summarise(mean = mean(m),
                                              median = median(m),
                                              edmean = mean(ed),
                                              edmedian = median(ed))

# baseline home model
# bin  49 is home
fin %>% slice(remove_ind_5min[[1]]) %>% 
  select(time,clust) %>% 
  left_join(allP2,by="clust") %>% 
  mutate(clust2 = 49)%>% 
  left_join(allP2,by=c("clust2" = "clust"),suffix = c(".clust",".predC")) %>% 
  na.omit() %>% 
  mutate( dist = raster::pointDistance(matrix(c(lon.clust,lat.clust),ncol = 2),
                                       matrix(c(lon.predC,lat.predC),ncol = 2),
                                       longlat = T)) %>% 
  thicken("5 min","weight5min") %>% 
  group_by(weight5min) %>% 
  summarise(m = mean(dist)) %>% summarise(mean = mean(m),
                                          median = median(m))


# repeat with 1 hour intervals

# get index of removed values
remove_ind_hour<- getRemoveIndex(fin,"1 hour",seed = 2003)

# divide into train and test data sets
testTrain1hr<- getTestTrain(fin,remove_ind_hour[[1]])

#get naive baseline method results
testTrain1hr[[1]] %>%
  select(clust,lagClust) %>% 
  left_join(allPoints,by="clust") %>% 
  left_join(allPoints,by=c("lagClust" = "clust"),suffix = c(".clust",".predC")) %>% 
  na.omit() %>% 
  mutate( dist = raster::pointDistance(matrix(c(lon.clust,lat.clust),ncol = 2),
                                       matrix(c(lon.predC,lat.predC),ncol = 2),
                                       longlat = T)) %>% 
  summary()

# transform into matrices for model input
dataListHour<- getModelInput(test = testTrain[[1]], train = testTrain[[2]])

# assign model inout
x_train_hour <- dataListHour[[1]]
x_test_hour  <- dataListHour[[2]]
y_train_hour <- dataListHour[[3]]
y_test_hour  <- dataListHour[[4]]

# define sequential model
model2 <- keras_model_sequential() 
model2 %>% 
  layer_dense(units = 250, activation = 'relu', input_shape = ncol(x_test_hour)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 120, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = nClust, activation = 'softmax')

# compile model
model2 %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# fit model
model2 %>% fit(
  as.matrix(x_train_hour), as.matrix(y_train_hour), 
  epochs = 10, batch_size = 200, 
  validation_split = 0.2
)

# evaluate accuracy
model2 %>% evaluate(as.matrix(x_test_hour),as.matrix(y_test_hour))

# compute distance for removed variables
t2<- fin %>% slice(remove_ind_hour) %>% 
  mutate(
  predC = as.integer(model2 %>% predict_classes(as.matrix(x_test_hour)))) %>%
  select(time,clust, predC)  %>%
  left_join(allPoints,by="clust") %>% 
  left_join(allPoints,by=c("predC" = "clust"),suffix = c(".clust",".predC")) %>% 
  na.omit() %>% 
  mutate( dist = raster::pointDistance(matrix(c(lon.clust,lat.clust),ncol = 2),
                                       matrix(c(lon.predC,lat.predC),ncol = 2),
                                       longlat = T))


# compute probabilities of predictions
probs <- model2 %>% predict_proba(as.matrix(x_test_hour))

# compute expected distance
dOrdered<- distMatrix[,t2 %>% select(clust) %>% pull()]
t2$expDist<- diag(probs[,-1] %*% dOrdered)

# compute certainty
t2$certainty<- apply(X = probs[,-1],MARGIN = 1,max)

# calculate 5 minute weights for comparison with other models

t2 %>% thicken("5 min","weight5min") %>% 
  group_by(weight5min) %>% 
  summarise(m = mean(dist),
            ed = mean(expDist)) %>% summarise(mean = mean(m),
                                          median = median(m),
                                          edmean = mean(ed),
                                          edmedian = median(ed))
# baseline home model
# bin  49 is home

fin %>% slice(remove_ind_hour[[1]]) %>% 
  select(time,clust) %>% 
  left_join(allPoints,by="clust") %>% 
  mutate(clust2 = 49)%>% 
  left_join(allPoints,by=c("clust2" = "clust"),suffix = c(".clust",".predC")) %>% 
  na.omit() %>% 
  mutate( dist = raster::pointDistance(matrix(c(lon.clust,lat.clust),ncol = 2),
                                       matrix(c(lon.predC,lat.predC),ncol = 2),
                                       longlat = T)) %>% 
  thicken("5 min","weight5min") %>% 
  group_by(weight5min) %>% 
  summarise(m = mean(dist)) %>% summarise(mean = mean(m),
                                          median = median(m))


# repeat for 1 day missing interval

# get removal index
remove_ind_day<- getRemoveIndexDay(fin, seed = 1115)

# separate into test and train dat sets
testTrainDay<- getTestTrain(fin,remove_ind_day[[1]])

#transform into model input 
dataListDay<- getModelInput(test = testTrainDay[[1]], train = testTrainDay[[2]])

# get the baseline naive model's dist measures
testTrainDay[[1]] %>%
  select(clust,lagClust) %>% 
  left_join(allPoints,by="clust") %>% 
  left_join(allPoints,by=c("lagClust" = "clust"),suffix = c(".clust",".predC")) %>% 
  na.omit() %>% 
  mutate( dist = raster::pointDistance(matrix(c(lon.clust,lat.clust),ncol = 2),
                                       matrix(c(lon.predC,lat.predC),ncol = 2),
                                       longlat = T)) %>% 
  summary()

# assign into separate matrices
x_train_day <- dataListDay[[1]]
x_test_day  <- dataListDay[[2]]
y_train_day <- dataListDay[[3]]
y_test_day  <- dataListDay[[4]]

# initialise sequential model
model3 <- keras_model_sequential() 
model3 %>% 
  layer_dense(units = 250, activation = 'relu', input_shape = ncol(x_test_day)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 120, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = nClust, activation = 'softmax')

summary(model3)

# compile model
model3 %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# fit model
model3 %>% fit(
  as.matrix(x_train_day), as.matrix(y_train_day), 
  epochs = 10, batch_size = 200, 
  validation_split = 0.2
)

# evaluate model
model3 %>% evaluate(as.matrix(x_test_day),as.matrix(y_test_day))

# calculate distances
t3<- fin %>% slice(remove_ind_day) %>%
  mutate(
  predC = as.integer(model3 %>% predict_classes(as.matrix(x_test_day)))) %>%
  select(time,clust, predC)  %>%
  left_join(allP2,by="clust") %>% 
  left_join(allP2,by=c("predC" = "clust"),suffix = c(".clust",".predC")) %>% 
  na.omit() %>% 
  mutate( dist = raster::pointDistance(matrix(c(lon.clust,lat.clust),ncol = 2),
                                       matrix(c(lon.predC,lat.predC),ncol = 2),
                                       longlat = T))


# calculate probabilities
probs <- model3 %>% predict_proba(as.matrix(x_test_day))

# calculate expected distances
dOrdered<- distMatrix[,t3 %>% select(clust) %>% pull()]
t3$expDist<- diag(probs[,-1] %*% dOrdered)

t3$certainty<- apply(X = probs[,-1],MARGIN = 1,max)

# explore results
summary(t3)

# weight results by 5 minute period
t3 %>% thicken("5 min","weight5min") %>% 
  group_by(weight5min) %>% 
  summarise(m = mean(dist),
            ed = mean(expDist)) %>% summarise(mean = mean(m),
                                              median = median(m),
                                              edmean = mean(ed),
                                              edmedian = median(ed))
summary(t3)

# baseline home model 
# bin  49 is home

fin %>% slice(remove_ind_day[[1]]) %>% 
  select(time,clust) %>% 
  left_join(allPoints,by="clust") %>% 
  mutate(clust2 = 49)%>% 
  left_join(allPoints,by=c("clust2" = "clust"),suffix = c(".clust",".predC")) %>% 
  na.omit() %>% 
  mutate( dist = raster::pointDistance(matrix(c(lon.clust,lat.clust),ncol = 2),
                                       matrix(c(lon.predC,lat.predC),ncol = 2),
                                       longlat = T)) %>% 
  thicken("5 min","weight5min") %>% 
  group_by(weight5min) %>% 
  summarise(m = mean(dist)) %>% summarise(mean = mean(m),
                                          median = median(m))

# accuracy 

fin %>% slice(remove_ind_day[[1]]) %>% 
  summarise(m = mean(clust == 49)) #40% accuracy


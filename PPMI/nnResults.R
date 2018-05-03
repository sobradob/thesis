#neural nets march test results
# to be run after neuralNets.R 

remove_ind_5min<- getRemoveIndex(fin,"5 min")

# missing: the features have to include the fact that they are missing, so they have to be recalcuated
# the extracted ones are wrong!!

testTrain5min<- getTestTrain(fin,remove_ind_5min[[1]])

# get baseline distance measure
testTrain5min[[1]] %>%
  select(clust,lagClust) %>% 
  left_join(allPoints,by="clust") %>% 
  left_join(allPoints,by=c("lagClust" = "clust"),suffix = c(".clust",".predC")) %>% 
  na.omit() %>% 
  mutate( dist = raster::pointDistance(matrix(c(lon.clust,lat.clust),ncol = 2),
                                       matrix(c(lon.predC,lat.predC),ncol = 2),
                                       longlat = T)) %>% 
  summary()
  
  
dataList5min<- getModelInput(test = testTrain5min[[1]], train = testTrain5min[[2]])

x_train <- dataList5min[[1]]
x_test  <- dataList5min[[2]]
y_train <- dataList5min[[3]]
y_test  <- dataList5min[[4]]

nClust <- 1222

model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 250, activation = 'relu', input_shape = ncol(x_test)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 120, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = nClust, activation = 'softmax')

summary(model)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

model %>% fit(
  as.matrix(x_train), as.matrix(y_train), 
  epochs = 10, batch_size = 200, 
  validation_split = 0.2
)
#plot(history)

model %>% evaluate(as.matrix(x_test),as.matrix(y_test)) #88 percent accuracy

# distance matrix of all clusters
distMatrix<- geosphere::distm(allPoints[,c("lon","lat")])

t<- fin %>% slice(remove_ind_5min[[1]])

#t <- t[-c(1,nrow(t)),] #remove first and due to the feature extraction loss using lead and lag

t<- t %>% mutate(
  predC = as.integer(model %>% predict_classes(as.matrix(x_test)))) %>%
  select(clust, predC,time)  %>%
  left_join(allPoints,by="clust") %>% 
  left_join(allPoints,by=c("predC" = "clust"),suffix = c(".clust",".predC")) %>% 
  na.omit() %>% 
  mutate( dist = raster::pointDistance(matrix(c(lon.clust,lat.clust),ncol = 2),
                                       matrix(c(lon.predC,lat.predC),ncol = 2),
                                       longlat = T))


t$expDist <-  expDist(model,x_test,distMatrix)

probs <- model %>% predict_proba(as.matrix(x_test))

certainty<- apply(X = probs[,-1],MARGIN = 1,max)
summary(certainty) # 0.07 to 0.18, median 0.12
tempDf <- cbind(t$dist,certainty) %>%  as.data.frame()
plot(tempDf$V1,tempDf$certainty)
summary(t)

# dist: median 0, mean 413
# expDist: mean 146, median 0

t %>% thicken("5 min","weight5min") %>% 
  group_by(weight5min) %>% 
  summarise(m = mean(dist),
            ed = mean(expDist)) %>% summarise(mean = mean(m),
                                              median = median(m),
                                              edmean = mean(ed),
                                              edmedian = median(ed))



# accuracy
t %>% summarise( nn =  mean(clust == predC))

# map
t  %>% select(clust, predC)  %>%
  left_join(allPoints,by="clust") %>% 
  left_join(allPoints,by=c("predC" = "clust"),suffix = c(".clust",".predC")) %>% 
  na.omit() %>% 
  mutate( dist = raster::pointDistance(matrix(c(lon.clust,lat.clust),ncol = 2),
                                       matrix(c(lon.predC,lat.predC),ncol = 2),
                                       longlat = T)) %>%  filter(dist >100000) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircles(lng = ~lon.clust, lat = ~lat.clust, color = "blue") %>% 
  addCircles(lng = ~lon.predC, lat = ~lat.predC, color = "red") 

#try with expected distance

cbind(t, distExp) %>% filter(predC != clust) %>% select(distExp) %>% summary()

plot(t$certainty,t$dist)
plot(t$certainty,t$expDist)

plot(t$certainty,x_test[,"sinTime"])
plot(t$certainty,x_test[,"cosTime"])

plot(t$certainty,
     strptime(paste0("2017-03-01 ", strftime(fin %>% slice(remove_ind_5min) %>% pull(time), "%R")),"%F %R")
     )

#play around with uncertainty. what are some points where its less certain?

t  %>% 
  left_join(allPoints,by="clust") %>% 
  left_join(allPoints,by=c("predC" = "clust"),suffix = c(".clust",".predC")) %>%
  filter(certainty < 0.2) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircles(lng = ~lon.clust, lat = ~lat.clust, color = "blue") %>% 
  addCircles(lng = ~lon.predC, lat = ~lat.predC, color = "red")

# the 1 hour thing

remove_ind_hour<- getRemoveIndex(fin,"1 hour",seed = 2003)

testTrain1hr<- getTestTrain(fin,remove_ind_hour[[1]])

#get naive method results
testTrain1hr[[1]] %>%
  select(clust,lagClust) %>% 
  left_join(allPoints,by="clust") %>% 
  left_join(allPoints,by=c("lagClust" = "clust"),suffix = c(".clust",".predC")) %>% 
  na.omit() %>% 
  mutate( dist = raster::pointDistance(matrix(c(lon.clust,lat.clust),ncol = 2),
                                       matrix(c(lon.predC,lat.predC),ncol = 2),
                                       longlat = T)) %>% 
  summary()

dataListHour<- getModelInput(test = testTrain[[1]], train = testTrain[[2]])

remove_ind_hour <- remove_ind_hour[[1]]

x_train_hour <- dataListHour[[1]]
x_test_hour  <- dataListHour[[2]]
y_train_hour <- dataListHour[[3]]
y_test_hour  <- dataListHour[[4]]

model2 <- keras_model_sequential() 
model2 %>% 
  layer_dense(units = 250, activation = 'relu', input_shape = ncol(x_test_hour)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 120, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = nClust, activation = 'softmax')

summary(model2)

model2 %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model2 %>% fit(
  as.matrix(x_train_hour), as.matrix(y_train_hour), 
  epochs = 10, batch_size = 200, 
  validation_split = 0.2
)
#plot(history)

model2 %>% evaluate(as.matrix(x_test_hour),as.matrix(y_test_hour))# accuracy 75%

t2<- fin %>% slice(remove_ind_hour)

#t <- t[-c(1,nrow(t)),] #remove first and due to the feature extraction loss using lead and lag

t2<- t2 %>% mutate(
  predC = as.integer(model2 %>% predict_classes(as.matrix(x_test_hour)))) %>%
  select(time,clust, predC)  %>%
  left_join(allPoints,by="clust") %>% 
  left_join(allPoints,by=c("predC" = "clust"),suffix = c(".clust",".predC")) %>% 
  na.omit() %>% 
  mutate( dist = raster::pointDistance(matrix(c(lon.clust,lat.clust),ncol = 2),
                                       matrix(c(lon.predC,lat.predC),ncol = 2),
                                       longlat = T))


probs <- model2 %>% predict_proba(as.matrix(x_test_hour))

dOrdered<- distMatrix[,t2 %>% select(clust) %>% pull()]
t2$expDist<- diag(probs[,-1] %*% dOrdered)

t2$certainty<- apply(X = probs[,-1],MARGIN = 1,max)

summary(t2)
# dist median 0, mean 1388
# exp dist, 36m mean, median 0

t2 %>% thicken("5 min","weight5min") %>% 
  group_by(weight5min) %>% 
  summarise(m = mean(dist),
            ed = mean(expDist)) %>% summarise(mean = mean(m),
                                          median = median(m),
                                          edmean = mean(ed),
                                          edmedian = median(ed))


# 1 day thing
remove_ind_day<- getRemoveIndexDay(fin, seed = 1115)


testTrainDay<- getTestTrain(fin,remove_ind_day[[1]])
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

remove_ind_day <- remove_ind_day[[1]]

x_train_day <- dataListDay[[1]]
x_test_day  <- dataListDay[[2]]
y_train_day <- dataListDay[[3]]
y_test_day  <- dataListDay[[4]]

model3 <- keras_model_sequential() 
model3 %>% 
  layer_dense(units = 250, activation = 'relu', input_shape = ncol(x_test_day)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 120, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = nClust, activation = 'softmax')

summary(model3)

model3 %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model3 %>% fit(
  as.matrix(x_train_day), as.matrix(y_train_day), 
  epochs = 10, batch_size = 200, 
  validation_split = 0.2
)
#plot(history)

model3 %>% evaluate(as.matrix(x_test_day),as.matrix(y_test_day))# accuracy 47.5%

t3<- fin %>% slice(remove_ind_day) %>% mutate(
  predC = as.integer(model3 %>% predict_classes(as.matrix(x_test_day)))) %>%
  select(time,clust, predC)  %>%
  left_join(allP2,by="clust") %>% 
  left_join(allP2,by=c("predC" = "clust"),suffix = c(".clust",".predC")) %>% 
  na.omit() %>% 
  mutate( dist = raster::pointDistance(matrix(c(lon.clust,lat.clust),ncol = 2),
                                       matrix(c(lon.predC,lat.predC),ncol = 2),
                                       longlat = T))



probs <- model3 %>% predict_proba(as.matrix(x_test_day))

dOrdered<- distMatrix[,t3 %>% select(clust) %>% pull()]
t3$expDist<- diag(probs[,-1] %*% dOrdered)

t3$certainty<- apply(X = probs[,-1],MARGIN = 1,max)

# something wrong with expectation and certainty, certainty is increasing whilst accuracy is decreasing, why is exp
summary(t3)

t3 %>% thicken("5 min","weight5min") %>% 
  group_by(weight5min) %>% 
  summarise(m = mean(dist),
            ed = mean(expDist)) %>% summarise(mean = mean(m),
                                              median = median(m),
                                              edmean = mean(ed),
                                              edmedian = median(ed))
summary(t3)

## Impute time at home 

# test with cos/sinDay
# no big change

# get remove index for missing periods when at home is missing

fin %>% 
  as_tbl_time(index = time) %>%
  filter_time(~ "2017-03") %>% 
  select(time, clust, lon, lat) %>% 
  thicken("5 min", colname = "thick", by="time") %>% 
  pad(by ="thick") %>% 
  filter(is.na(lon)) %>% 
  pull(thick) ->rems

remove_ind <- which(fin %>%
                                   thicken("5 min", colname = "thick", by="time") %>%
                                   pad(by = "thick") %>% 
                                   pull(thick) %in% rems)

testTrainHome<- getTestTrain(fin,remove_ind, nClust = 807)

# get baseline distance measure
testTrainHome[[1]] %>%
  select(clust,lagClust) %>% 
  left_join(allPoints,by="clust") %>% 
  left_join(allPoints,by=c("lagClust" = "clust"),suffix = c(".clust",".predC")) %>% 
  na.omit() %>% 
  mutate( dist = raster::pointDistance(matrix(c(lon.clust,lat.clust),ncol = 2),
                                       matrix(c(lon.predC,lat.predC),ncol = 2),
                                       longlat = T)) %>% 
  summary()


dataListHome<- getModelInput(test = testTrainHome[[1]], train = testTrainHome[[2]],nClust = 807)

x_train <- dataListHome[[1]]
x_test  <- dataListHome[[2]]
y_train <- dataListHome[[3]]
y_test  <- dataListHome[[4]]

remove_ind_5min <- remove_ind_5min[[1]]

model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 250, activation = 'relu', input_shape = ncol(x_train)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 120, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = ncol(y_test), activation = 'softmax')

summary(model)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  as.matrix(x_train), as.matrix(y_train), 
  epochs = 10, batch_size = 200, 
  validation_split = 0.2
)

model %>% evaluate(as.matrix(x_test),as.matrix(y_test)) #88 percent accuracy

pred<- getMissing(data = fin, remove_ind = remove_ind, rems = rems)

as.integer(model %>% predict_classes(pred))

# create a new baseline with all the time at home

remove_ind_day<- getRemoveIndexDay(fin, seed = 1115)

# get the baseline naive model's dist measures

fin %>% filter(clust == 49) %>% 
  summarise(lon = mean(lon), lat = mean(lat)) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircles()

# 49 is home

# recreate allPoints

allP2<- fin %>% group_by(clust) %>% 
  summarise(lon = mean(lon), lat = mean(lat))

# accuracy 

fin %>% slice(remove_ind_day[[1]]) %>% 
  summarise(m = mean(clust == 49)) #40% accuracy

fin %>% slice(remove_ind_day[[1]]) %>% 
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

fin %>% slice(remove_ind_hour[[1]]) %>% 
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



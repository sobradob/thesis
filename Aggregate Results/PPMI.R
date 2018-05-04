## Impute time at home using PPMI
# get remove index for missing periods when at home is missing

# get the five minute periods with no measurements for the single month
fin %>% 
  as_tbl_time(index = time) %>%
  filter_time(~ "2017-03") %>% 
  select(time, clust, lon, lat) %>% 
  thicken("5 min", colname = "thick", by="time") %>% 
  pad(by ="thick") %>% 
  filter(is.na(lon)) %>% 
  pull(thick) ->rems

# create a removal index
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

# extract test and train sets
dataListHome<- getModelInput(test = testTrainHome[[1]], train = testTrainHome[[2]],nClust = 807)

# assign test and train sets
x_train <- dataListHome[[1]]
x_test  <- dataListHome[[2]]
y_train <- dataListHome[[3]]
y_test  <- dataListHome[[4]]

# create a sequential neural network model 
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 250, activation = 'relu', input_shape = ncol(x_train)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 120, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = ncol(y_test), activation = 'softmax')

# compile model
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# fit model
model %>% fit(
  as.matrix(x_train), as.matrix(y_train), 
  epochs = 10, batch_size = 200, 
  validation_split = 0.2
)

# evaluate model accuracy 
model %>% evaluate(as.matrix(x_test),as.matrix(y_test))

# get the set needed for prediction
pred<- getMissing(data = fin, remove_ind = remove_ind, rems = rems)

# predicted values
t <- test2 %>%  pad()
t[which(t$thick %in% rems),"clust"] <- as.integer(model %>% predict_classes(pred))

# calculate the at model prediction
t %>% mutate( home1 = clust %in% c(49,348,336)) %>%
  summarise(atHomeProp = sum(home1)/nrow(.))

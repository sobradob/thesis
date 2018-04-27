# neural networks


library(keras)
library(tibbletime)
library(dplyr)
data2 <- fin
#data2<- readRDS('nnData.rds')
# reshape
seconds_in_day <-  24*60*60

# prepare NL data for ML
d <- data2 %>%
  mutate( day  = strftime(time,"%u"),
          month = strftime(time,"%m"),
          secMidnight = lubridate::period_to_seconds(lubridate::hms(strftime(time,"%T"))),
          sinTime = sin(2*pi*secMidnight/seconds_in_day),
          cosTime = cos(2*pi*secMidnight/seconds_in_day)) %>% 
  select(time,clust,nextMeas,prevMeas,day,sinTime,cosTime,month) %>% 
  mutate(nextMeas = scales::rescale(nextMeas),
         prevMeas = scales::rescale(prevMeas),
         lagClust = lag(clust),
         leadClust = lead(clust))

d <- d %>% na.omit()# remove first and previous lag difference

# add feature: distance of previous measurement to all other clusters
dClustM<- distm(allPoints[,c("lon","lat")])
dClustMr<- apply(X = dClustM,MARGIN = 1,FUN = scales::rescale)
# If random sample
## 75% of the sample size
smp_size <- floor(0.75 * nrow(d))
train_ind <- sample(seq_len(nrow(d)), size = smp_size)

train <- d[train_ind,]
test <- d[-train_ind,]

#atlernative
train <- d %>% as_tbl_time(time)%>%
  filter_time(("start"~ "2017-03-12"))

train <- rbind(train, d %>% as_tbl_time(time)%>%
                 filter_time("2017-03-14" ~ "end"))

test <- d %>% as_tbl_time(time)%>%
  filter_time((~ "2017-03-13"))
# alternative end

nClust<- (length(unique(allPoints$clust))+1)

# add next Pause clust? and prior pause Clust

x_train <- cbind(train[,c("nextMeas","prevMeas","sinTime","cosTime")],
                 to_categorical(train$day, num_classes = 8),
                 to_categorical(train$month, num_classes = 13),
                 to_categorical(train$lagClust,num_classes = nClust),
                 to_categorical(train$leadClust, num_classes = nClust)
                 ) %>% as.matrix()

x_test <- cbind(test[,c("nextMeas","prevMeas","sinTime","cosTime")],
                 to_categorical(test$day, num_classes = 8),
                 to_categorical(test$month, num_classes = 13),
                 to_categorical(test$lagClust,num_classes = nClust),
                 to_categorical(test$leadClust, num_classes = nClust)) %>% as.matrix()

# With distance
x_train <- cbind(train[,c("nextMeas","prevMeas","sinTime","cosTime")],
                 to_categorical(train$day, num_classes = 8),
                 to_categorical(train$month, num_classes = 13),
                 to_categorical(train$lagClust,num_classes = nClust),
                 to_categorical(train$leadClust, num_classes = nClust),
                 dClustMr[train$lagClust,]
) %>% as.matrix()

x_test <- cbind(test[,c("nextMeas","prevMeas","sinTime","cosTime")],
                to_categorical(test$day, num_classes = 8),
                to_categorical(test$month, num_classes = 13),
                to_categorical(test$lagClust,num_classes = nClust),
                to_categorical(test$leadClust, num_classes = nClust),
                dClustMr[train$lagClust,]) %>% as.matrix()

#resume normal 
y_train <- to_categorical(train$clust, num_classes = nClust)
y_test <- to_categorical(test$clust, num_classes = nClust)


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

history <- model %>% fit(
  as.matrix(x_train), as.matrix(y_train), 
  epochs = 10, batch_size = 200, 
  validation_split = 0.2
)
#plot(history)

model %>% evaluate(as.matrix(x_test),as.matrix(y_test))

# show uncertainty as a function of distance in time from measurements. 
model %>% predict_proba(as.matrix(x_test))


# Check
t<- test %>% mutate(
  predC = as.integer(model %>% predict_classes(as.matrix(x_test)))
) %>% as_tbl_time(time)

# baseline: previous value
t %>% summarise( nn =  mean(clust == predC),
                 prev = mean(clust == lagClust),
                 nextOne = mean(clust == leadClust))

t %>% filter( clust != predC) %>%
  sample_n(1) %>%
  select(clust,predC,lagClust,leadClust) %>%
  left_join(allPoints,by="clust") %>% 
  left_join(allPoints,by=c("predC" = "clust"),suffix = c(".clust",".predC")) %>% 
  left_join(allPoints,by=c("leadClust" = "clust")) %>%
  left_join(allPoints,by=c("lagClust" = "clust"),suffix = c(".lead",".lag")) %>% 
  leaflet() %>% 
  addTiles() %>%
  addCircles(lng = ~lon.clust,lat = ~lat.clust, color = "green", radius = 100, label = ~as.character(clust),group = "Actual") %>% 
  addCircles(lng = ~lon.predC,lat = ~lat.predC, color = "red", radius = 100, label = ~as.character(predC),group = "Pred") %>% 
  addCircles(lng = ~lon.lead,lat = ~lat.lead, color = "yellow", radius = 100, label = ~as.character(leadClust),group = "Next") %>%
  addCircles(lng = ~lon.lag,lat = ~lat.lag, color = "blue", radius = 100, label = ~as.character(lagClust), group = "Prev") %>% 
  addLayersControl(
    overlayGroups = c("Actual", "Pred","Next","Prev"),
    options = layersControlOptions(collapsed = FALSE)
  )

library(leaflet)

leaflet() %>% 
  addTiles() %>% 
  addCircles(data = left_join(t %>% top_n(100),allPoints, by = "clust"),lng = ~lon, lat = ~lat, color = "red", radius = 10,
             label = ~as.character(clust)) %>% 
  addCircles(data = left_join(t %>% top_n(100),allPoints, by = c("predC" = "clust")),lng = ~lon, lat = ~lat, color = "green", radius = 10,label = ~as.character(predC))

t %>% select(time, clust,predC) %>% View()

# identify & extract missings


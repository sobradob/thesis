# my own neural net lstm

# need to look into this
# got the data massaged into the right list (ish)
# but the lstm doesn't work

# constants
data_dim <- 2
timesteps <- 2
num_classes <- 10

test<- data.frame(clust = rep(1:10,10), index = 1:100, feature= runif(100))

nClust <- 10+1
x_train <- array(unlist(test), dim = c(100, timesteps, data_dim))

smp_size <- floor(0.90 * nrow(test))
train_ind <- sample(seq_len(nrow(test)), size = smp_size)
train_ind_prev <- train_ind-1
train_ind_next <- train_ind+1

# create list and then to array
selectedRandom <- 4

listMat <- list()
i <- 1
for( selectedRandom in train_ind){
  listMat[[i]]<- test[c(selectedRandom-1,selectedRandom+1),] %>% as.matrix()
  i <- i+1
}
# doesn't work! but input can be a list too. 
a<- array(as.numeric(unlist(listMat)), dim=c(2, 3, 90))


# define and compile model
# expected input data shape: (batch_size, timesteps, data_dim)
model <- keras_model_sequential() 
model %>% 
  layer_lstm(units = 250, return_sequences = TRUE, input_shape = c(timesteps, data_dim)) %>% 
  layer_lstm(units = 32) %>% # return a single vector dimension 32
  layer_dense(units = nClust, activation = 'softmax') %>% 
  compile(
    loss = 'categorical_crossentropy',
    optimizer = 'rmsprop',
    metrics = c('accuracy')
  )

# generate training data

# prepare NL data for ML
d <- fin %>%
  mutate( day  = strftime(time,"%u"),
          month = strftime(time,"%m"),
          secMidnight = lubridate::period_to_seconds(lubridate::hms(strftime(time,"%T"))),
          sinTime = sin(2*pi*secMidnight/seconds_in_day),
          cosTime = cos(2*pi*secMidnight/seconds_in_day)) %>% 
  select(time,clust,nextMeas,prevMeas,day,sinTime,cosTime,month) %>% 
  mutate(nextMeas = scales::rescale(nextMeas),
         prevMeas = scales::rescale(prevMeas)
         )

d <- d %>% na.omit()# remove first and previous lag difference
nClust<- (length(unique(allPoints$clust))+1)

d2<- cbind(d[,c("nextMeas","prevMeas","sinTime","cosTime")],
      to_categorical(d$day, num_classes = 8),
      to_categorical(d$month, num_classes = 13)
) %>% as.matrix()

smp_size <- floor(0.75 * nrow(d2))
train_ind <- sample(seq_len(nrow(d2)), size = smp_size)
#remove first and last

outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

train_ind<- outersect(c(1,nrow(d2)),train_ind)

train_x_list <- list()
i <- 1
for( selectedRandom in train_ind){
  train_x_list[[i]]<- d2[c(selectedRandom-1,selectedRandom+1),] %>% as.matrix()
  i <- i+1
}

train_y <- to_categorical(d[train_ind,"clust"])

test_ind<- outersect(train_ind,1:nrow(d2))
test_ind <- outersect(c(1,nrow(d2)),test_ind)

test_x_list <- list()
i <- 1
for( selectedRandom in test_ind){
  test_x_list[[i]]<- d2[c(selectedRandom-1,selectedRandom+1),] %>% as.matrix()
  i <- i+1
}

test_y <- to_categorical(d[test_ind,"clust"])


# define and compile model
# expected input data shape: (batch_size, timesteps, data_dim)
model <- keras_model_sequential() 
model %>% 
  layer_lstm(units = 200, return_sequences = TRUE, input_shape = c(2, 25)) %>% 
  layer_lstm(units = 60, return_sequences = TRUE) %>% 
  layer_lstm(units = 40) %>% # return a single vector dimension 32
  layer_dense(units = 1, activation = 'softmax') %>% 
  compile(
    loss = 'categorical_crossentropy',
    optimizer = 'rmsprop',
    metrics = c('accuracy')
  )

model %>% fit( 
  train_x_list, train_y, batch_size = 64, epochs = 5, validation_data = list(test_x_list, test_y)
)

model %>% predict_classes(x_train)

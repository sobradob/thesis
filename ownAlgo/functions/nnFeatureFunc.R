# extract features for the neural nets
getTestTrain <- function(data, remove_ind,nClust = 1222){
  seconds_in_day <-  24*60*60
  
  train <- data[-remove_ind,]
  test <- data[remove_ind,]
  
  # issue: train is right, test isn't!
  # changed it, check to see if it works!
  train<- train %>% 
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
            clustPrev = clust)
  
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
            clustPrev = NA)

  all <- rbind(train,test) %>%
    arrange(timestampMs) %>% 
    fill(timestampLast,clustPrev, .direction = "down") %>% 
    fill(timestampNext,clustNext, .direction = "up") %>%
    mutate( prevMeas = timestampMs - timestampLast,
            nextMeas = timestampNext - timestampMs,
            lagClust = clustPrev,
            leadClust = clustNext) %>% 
    mutate(nextMeas = scales::rescale(nextMeas, from = c(0,3600000)),
           prevMeas = scales::rescale(prevMeas, from = c(0,3600000)))
  
  test <- all %>% slice(remove_ind)
  train <- all %>% slice(-remove_ind)
  
  test <- test %>% na.omit()# remove first and previous lag difference
  train <- train %>% na.omit()# remove first and previous lag difference
  
  cat(paste0("Baseline model previous Clust is: ", test %>% summarise(mean(clust == lagClust))))
  
  return(list(test = test, train = train))
}

getModelInput <- function(train,test,nClust = 1222){
  library(keras)
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
  
  y_train <- to_categorical(train$clust, num_classes = nClust)
  y_test <- to_categorical(test$clust, num_classes = nClust)
  
  return(list(x_train,x_test,y_train,y_test))
}

  
# extract features for the neural nets
getTestTrain2 <- function(data, remove_ind,nClust = 1222){
  seconds_in_day <-  24*60*60
  
  train <- data[-remove_ind,]
  test <- data[remove_ind,]
  
  # with sin, cos time
  train<- train %>% 
    select(time,timestampMs,clust) %>% 
    mutate( sinDay  = sin((2*pi)/30*as.numeric(strftime(time,"%d"))),
            cosDay = cos((2*pi)/30*as.numeric(strftime(time,"%d"))),
            day  = strftime(time,"%u"),
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
            clustPrev = clust)
  
  test<- test %>% 
    select(time,timestampMs,clust) %>% 
    mutate( sinDay  = sin((2*pi)/30*as.numeric(strftime(time,"%d"))),
            cosDay = cos((2*pi)/30*as.numeric(strftime(time,"%d"))),
            day  = strftime(time,"%u"),
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
            clustPrev = NA)
  
  all <- rbind(train,test) %>%
    arrange(timestampMs) %>% 
    fill(timestampLast,clustPrev, .direction = "down") %>% 
    fill(timestampNext,clustNext, .direction = "up") %>%
    mutate( prevMeas = timestampMs - timestampLast,
            nextMeas = timestampNext - timestampMs,
            lagClust = clustPrev,
            leadClust = clustNext) %>% 
    mutate(nextMeas = scales::rescale(nextMeas, from = c(0,3600000)),
           prevMeas = scales::rescale(prevMeas, from = c(0,3600000)))
  
  test <- all %>% slice(remove_ind)
  train <- all %>% slice(-remove_ind)
  
  test <- test %>% na.omit()# remove first and previous lag difference
  train <- train %>% na.omit()# remove first and previous lag difference
  
  cat(paste0("Baseline model previous Clust is: ", test %>% summarise(mean(clust == lagClust))))
  
  return(list(test = test, train = train))
}


getModelInput <- function(train,test,nClust = 1222){
  library(keras)
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
  
  y_train <- to_categorical(train$clust, num_classes = nClust)
  y_test <- to_categorical(test$clust, num_classes = nClust)
  
  return(list(x_train,x_test,y_train,y_test))
}


getModelInput2 <- function(train,test,nClust = 1222){
  library(keras)
  x_train <- cbind(train[,c("nextMeas","prevMeas","sinTime","cosTime","sinDay","cosDay")],
                   to_categorical(train$day, num_classes = 8),
                   to_categorical(train$month, num_classes = 13),
                   to_categorical(train$lagClust,num_classes = nClust),
                   to_categorical(train$leadClust, num_classes = nClust)
  ) %>% as.matrix()
  
  x_test <- cbind(test[,c("nextMeas","prevMeas","sinTime","cosTime","sinDay","cosDay")],
                  to_categorical(test$day, num_classes = 8),
                  to_categorical(test$month, num_classes = 13),
                  to_categorical(test$lagClust,num_classes = nClust),
                  to_categorical(test$leadClust, num_classes = nClust)
                  ) %>% as.matrix()
  
  y_train <- to_categorical(train$clust, num_classes = nClust)
  y_test <- to_categorical(test$clust, num_classes = nClust)
  
  return(list(x_train,x_test,y_train,y_test))
}



## auxiliary data details

# time span
# observations
# missing days
# mean accuracy
# sd accuracy

# variables constructed in the playing around with thicken.R file

# function

extractDescriptives<- function(PATH="../../data/boaz/myLocationHistory.rds"){
  all<-readRDS(PATH)
  
  all <- all %>% select(time,accuracy,lat,lon)
  
  # begin descriptive construction
  
  # n observations
  observations <- all %>% nrow()
  
  # span
  spanDf<- all %>%
    as_tbl_time(index = time) %>%
    time_filter( 2000  ~ 2018) %>% 
    summarise(max = as.Date(max(time)),
              min = as.Date(min(time)))
  
  span <- paste0("From ", spanDf[["min"]], " to ", spanDf[["max"]])
  
  # range in days
  range <- spanDf[["max"]]- spanDf[["min"]]
  
  # accuracy
  meanAc <- mean(all$accuracy)
  sdAc <- sd(all$accuracy)
  
  # missing periods of 5 min
  miss5min<- all %>%
    select(time) %>%
    as_tbl_time(index = time) %>%
    time_filter(2000-01 ~ 2017-06) %>% 
    thicken('5 min') %>%
    group_by(time_5_min) %>%
    summarise(measurements = n()) %>%
    pad() %>%
    fill_by_value(value = 0) %>%
    thicken("day") %>%
    mutate(
      missing = case_when(
        measurements== 0 ~ 1,
        measurements > 0 ~ 0)) %>%
    group_by(time_5_min_day) %>%
    summarise(missing = sum(missing)/288)%>%
    filter(missing != 1) %>% 
    summarise(missing = mean(missing))
  
  missDay<- all %>%
    select(time) %>%
    as_tbl_time(index = time) %>%
    time_filter(2000-01 ~ 2017-06) %>% 
    thicken('day') %>%
    group_by(time_day) %>%
    summarise(measurements = n()) %>%
    pad() %>%
    fill_by_value(value = 0) %>%
    mutate(
      missing = case_when(
        measurements== 0 ~ 1,
        measurements > 0 ~ 0)) %>%
    summarise(missing = sum(missing))
  
  datadetails<- data.frame(
    span = span,
    days = range[[1]],
    observations = observations,
    days_missing = missDay[["missing"]],
    slots_missing = miss5min[["missing"]],
    mean_accuracy = meanAc,
    sd_accuracy   = sdAc
  )
return(datadetails)  
}

boaz <- extractDescriptives(PATH = "../../data/boaz/myLocationHistory.rds")
sipke <- extractDescriptives(PATH = "../../data/sipke/myLocationHistory.rds")
peter <- extractDescriptives(PATH = "../../data/peter/myLocationHistory.rds")

allData<- bind_rows(boaz,sipke,peter)

saveRDS(allData, "datadescriptives.rds")

# modify it now

d<- readRDS("../tempdata/datadescriptives.rds")
d$span <- gsub(pattern = "From ",replacement = "",d$span)
saveRDS(d, "../tempdata/datadescriptives.rds")

# objective data comparison

# set directory to local folder (requires R studio)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
library(readr)
objLog<- read_csv2("objectiveLog.csv")
objKey<- read_csv2("objectiveKey.csv") %>% select(Location,Cluster)

# organise data
objKey<- left_join(objKey,allPoints, by = c("Cluster" = "clust")) %>% select(Location,Cluster,lon,lat)

# add time log
objLog <- objLog %>%
  mutate( time = as.POSIXct(paste(Date, Time), format="%d-%m-%Y %H:%M:%S")) %>% 
  as_tbl_time(time)

# merge files
objLog <- left_join(objLog,objKey)




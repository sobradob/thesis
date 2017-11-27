# missing days with padr
library(dplyr)
library(padr)
library(ggplot2)
library(tibbletime)
library(ggthemes)
library(scales)
# read data
all<-readRDS("../../data/boaz/myLocationHistory.rds")
attr(all$time, "tzone") <- "Europe/Paris"

# by hour
mHour<- all %>%
  select(time) %>%
  as_tbl_time(index = time) %>%
  time_filter(2016-01 ~ 2017-01) %>% 
  thicken('hour') %>%
  group_by(time_hour) %>%
  summarise(measurements = n()) %>%
  pad() %>%
  fill_by_value(value = 0) %>%
  thicken("day") %>%
  mutate(
    type = case_when(
      measurements== 0 ~ 1,
      measurements > 0 ~ 0)) %>%
  group_by(time_hour_day) %>%
  summarise(hoursMissing = sum(type),
            measurements = sum(measurements))

library(scales)
missingDay<- ggplot(mHour, aes(x = time_hour_day, y=measurements, colour= hoursMissing))+
  geom_point()+theme_tufte()+
  xlab("")+
  ylab("Measurements")+
  ggtitle("Amount of daily measurements over time")+
  scale_colour_gradient2(low = muted("green"), mid = "grey50",
                         high = muted("red"), midpoint = 12, space = "Lab",
                         na.value = "grey50", guide = "colourbar")+
  labs(x = NULL, colour = "Missing Hours")

ggsave(missingDay,filename = "img/missingdayPeter.png",device = "png",height = 6.5, units = "cm")

# by 5 minute
m5min<- all %>%
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
    type = case_when(
      measurements== 0 ~ 1,
      measurements > 0 ~ 0)) %>%
  group_by(time_5_min_day) %>%
  summarise(min5Missing = sum(type),
            measurements = sum(measurements))

#calculating descriptives

m5min %>%
  select(type) %>%
  summarise(mean(type))

missingDay5min<- ggplot(m5min, aes(x = time_5_min_day, y=measurements, colour= min5Missing))+
  geom_point()+theme_tufte()+
  xlab("")+
  ylab("Measurements")+
  ggtitle("Missingness in daily measurements over time")+
  scale_colour_gradient2(low = "black", mid = "grey50",
                         high = muted("red"), midpoint = 144, space = "Lab",
                         na.value = "grey50", guide = "colourbar")+
  labs(x = NULL, colour = "Missing \n5 minute\n segments")

ggsave(missingDay5min,filename = "../img/missingdayBoaz5min.png",device = "png",height = 6.5, width = 18, units = "cm")



time1_p1 <- strptime(paste("2017-02-14", "00:00:00"), "%Y-%m-%d %H:%M:%S")
time2_p1 <- strptime(paste("2017-02-16", "24:00:00"), "%Y-%m-%d %H:%M:%S")
xlim_p1 <- as.POSIXct(c(time1_p1, time2_p1), origin="1970-01-01", tz="Asia/Singapore")


ggsave(missingDay5min,filename = "../img/missingdayPeter5min.png",device = "png",height = 6.5, units = "cm")

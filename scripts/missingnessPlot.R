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
  time_filter(2000-01 ~ 2017-06) %>% 
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


missingDay5min<- ggplot(m5min, aes(x = time_5_min_day, y=measurements, colour= min5Missing))+
  geom_point()+theme_tufte()+
  xlab("")+
  ylab("Measurements")+
  ggtitle("Missingness in daily measurements over time")+
  scale_colour_gradient2(low = muted("green"), mid = "grey50",
                         high = muted("red"), midpoint = 144, space = "Lab",
                         na.value = "grey50", guide = "colourbar")+
  labs(x = NULL, colour = "Missing \n5 minute\n segments")+
  theme(legend.position = c(0.3, 0.85), legend.direction = "horizontal")

ggsave(missingDay5min,filename = "../img/missingdayPeter5min.png",device = "png",height = 6.5, units = "cm")

# by 5 minute version 2
m5minv2<- all %>%
  select(time,accuracy) %>%
  as_tbl_time(index = time) %>%
  time_filter(2000-01 ~ 2017-06) %>% 
  thicken('5 min') %>%
  group_by(time_5_min) %>%
  summarise(measurements = n(),
            accuracy = mean(accuracy)) %>%
  pad() %>%
  fill_by_value(value = 0)%>%
  mutate(
    hour = as.POSIXct(
      paste0("2014-01-22 ",
           strftime(time_5_min,format = "%H:%M", tz = "Europe/Budapest"))),
    time2 = lubridate::force_tz(time_5_min, "Asia/Singapore"),
    missing = case_when(
      measurements== 0 ~ 1,
      measurements > 0 ~ 0)
      )

exampleMiss<- m5minv2 %>%
  as_tbl_time(index = time2)%>%
  time_filter(2017-02-15 ~ 2017-02-15) %>%
  ggplot( aes(x = hour, y=measurements, colour = factor(missing)))+
  geom_point()+theme_tufte()+
  xlab("")+
  ylab("Measurements")+
  ggtitle("Measurements per 5 minute window")+
  scale_x_datetime(breaks = date_breaks("4 hour"),
                   minor_breaks=date_breaks("2 hour"),
                   labels=date_format("%H:%M:%S", tz = "Asia/Singapore"))+
  scale_colour_manual(values=c("black","#DF2935"))+
  theme(legend.position="none")

ggsave(exampleMiss,filename = "../img/missingBoaz5minExample.png",device = "png",height = 6.5, units = "cm")


time1_p1 <- strptime(paste("2017-02-14", "00:00:00"), "%Y-%m-%d %H:%M:%S")
time2_p1 <- strptime(paste("2017-02-16", "24:00:00"), "%Y-%m-%d %H:%M:%S")
xlim_p1 <- as.POSIXct(c(time1_p1, time2_p1), origin="1970-01-01", tz="Asia/Singapore")


ggsave(missingDay5min,filename = "../img/missingdayPeter5min.png",device = "png",height = 6.5, units = "cm")

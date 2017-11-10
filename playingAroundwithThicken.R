# missing days with padr
library(dplyr)
library(padr)
library(ggplot2)
# read data
loc<- readRDS("..\\all.rds")

mDaily<- loc %>%
  filter(who =="peter")%>%
  select(time) %>%
  mutate(time =  lubridate::force_tz(time, "Europe/Budapest")) %>%
  top_n(207952)%>% 
  thicken('day') %>%
  group_by(time_day) %>%
  summarise(count = n()) %>%
  pad() %>%
  fill_by_value(value = 0)

mHour<- loc %>%
  filter(who =="peter")%>%
  select(time) %>%
  mutate(time =  lubridate::force_tz(time, "Europe/Budapest")) %>%
  top_n(207952)%>% 
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

pointsdaily<-loc %>% group_by(Day) %>% summarise(count = n())
pointshourly<-loc %>%
  group_by(Day, lubridate::hour(time)) %>%
  summarise(count=n())

pointsdaily
pointshourly

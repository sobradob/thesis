# daily missing plot 

# by 5 minute version 2
m5minv2<- fin %>%
  select(time,accuracy) %>%
  as_tbl_time(index = time) %>%
  filter_time("2000-01" ~ "2017-06") %>% 
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
  filter_time("2017-02-15" ~ "2017-02-15") %>%
  ggplot( aes(x = hour, y=measurements))+
  geom_point()+theme_tufte()+
  xlab("")+
  ylab("Measurements")+
  ggtitle("Measurements per 5 minute window",subtitle = "February 15 2017")+
  scale_x_datetime(breaks = date_breaks("4 hour"),
                   minor_breaks=date_breaks("2 hour"),
                   labels=date_format("%H:%M", tz = "Europe/Budapest"))+
  labs(x = NULL, colour = "")+xlab("Time")+
  scale_y_continuous(breaks = seq(0, 9, by = 2), name = "Measurements")

ggsave(exampleMiss,filename = "../manuscript/img/missin5minExample2018.png",device = "png",height = 10, width = 20, units = "cm")

# randomly select two days
sample(x = c(13,14,15,16,17),1)#YEAR
sample(x = c(1:12),1)#YEAR::
sample(c(1:31),1)#day

exampleMiss<- m5minv2 %>%
  as_tbl_time(index = time2)%>%
  time_filter(2014-10-27 ~ 2014-10-27) %>%
  ggplot( aes(x = hour, y=measurements, colour = factor(missing)))+
  geom_point()+theme_tufte()+
  xlab("")+
  ylab("Measurements")+
  ggtitle("Measurements per 5 minute window",subtitle = "2014-10-27")+
  scale_x_datetime(breaks = date_breaks("4 hour"),
                   minor_breaks=date_breaks("2 hour"),
                   labels=date_format("%H:%M:%S", tz = "Asia/Singapore"))+
  scale_colour_manual(values=c("black",muted("red")),labels=c("Not Missing", "Missing"))+
  labs(x = NULL, colour = "")
missingBoaz5minExample20141027.png
ggsave(exampleMiss,filename = "../img/missingBoaz5minExample20141027.png",device = "png",height = 6.5, width = 18, units = "cm")


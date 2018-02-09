# rolling accuracy over time


r100mean<- rollify(mean,window = 100)

r100mean(1:100000)
# by 5 minute
m5min<- all %>%
  select(time, accuracy) %>%
  as_tbl_time(index = time) %>%
  time_filter(2000-01 ~ 2017-06) %>% 
  thicken('day') %>%
  group_by(time_day) %>%
  summarise(accuracy = mean(accuracy)) %>%
  pad() %>%
  fill_by_value(value = NA) %>%
  ggplot(aes(x = time_day, y=accuracy,))+
  geom_point()+theme_tufte()+
  xlab("")+
  ylab("Measurements")+
  ggtitle("Missingness in daily measurements over time")+
  scale_colour_gradient2(low = "black", mid = "grey50",
                         high = muted("red"), midpoint = 144, space = "Lab",
                         na.value = "grey50", guide = "colourbar")+
  labs(x = NULL, colour = "Missing \n5 minute\n segments")

all %>%
  select(time, accuracy) %>%
  as_tbl_time(index = time) %>%
  time_filter(2016-01 ~ 2017-06) %>%
  mutate(rollingAcc = r100mean(accuracy))%>%
  ggplot(aes(x = time, y=rollingAcc))+
  geom_point()+theme_tufte()

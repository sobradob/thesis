# aggregator

x<- all %>%
  select(time,lon,lat,accuracy) %>%
  as_tbl_time(index = time) %>%
  time_filter(2017-02 ~ 2017-02) %>% 
  thicken('5 min', colname = 'time2') %>%
  group_by(time2) %>%
  summarise(lon = mean(lon),
            lat = mean(lat),
            accuracy = mean(accuracy)
  )%>% 
  pad()%>%
  tidyr::fill(c(lon,lat)) %>%
  mutate(distanceHome = sp::spDistsN1(pts=matrix(c(lon,lat),ncol = 2),
                                      pt = home,
                                      longlat = T))
  
exploreDates(x,"2017-02-15","2017-02-15")
# time spent at home


ggplot(x,aes(x = distanceHome, y = time2))+
  geom_point()

athome<- x %>% mutate(atHome = case_when(distanceHome <0.25 ~ 1,
                                distanceHome >0.25 ~ 0)) %>%
  thicken('day', colname = 'time') %>%
  group_by(time) %>%
  summarise(atHome2 = sum(atHome)/288) %>%
  mutate(day = strftime(time,"%A"))

athome$day <- factor(athome$day, levels= c("Monday", 
                                         "Tuesday", "Wednesday",
                                         "Thursday", "Friday", "Saturday","Sunday"))

timeUse<- ggplot(athome,aes(x = day, y = atHome2))+
  geom_point()+theme_tufte()+xlab("Day of the week")+
  ylab("Proportion of time\nspent at home")+ggtitle("Time spent at home - February 2017")

ggsave(timeUse,filename = "../img/timeUse.png",device = "png",height = 10,width = 20, units = "cm")
  

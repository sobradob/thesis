# aggregator

x<- all %>%
  select(time,lon,lat,accuracy) %>%
  arrange(time) %>%
  as_tbl_time(index = time) %>%
  time_filter(2017-02 ~ 2017-02) %>%
  mutate(nofilterDistance = c(0,sp::spDists(x=matrix(c(lon,lat),ncol = 2), segments = T,longlat = T))
         ) %>%
  thicken('5 min', colname = 'time2') %>%
  group_by(time2) %>%
  summarise(lon = mean(lon),
            lat = mean(lat),
            nofilterDistance = sum(nofilterDistance),
            accuracy = mean(accuracy)
  )%>% 
  pad()


#filtered distance covered
xFiltered<- all %>%
  arrange(time) %>%
  select(time,lon,lat,accuracy) %>%
  as_tbl_time(index = time) %>%
  time_filter(2017-02 ~ 2017-02) %>%
  filter(accuracy < 400) %>%
  mutate(filter400Distance = c(0,
                              sp::spDists(x=matrix(c(lon,lat),ncol = 2),
                                          segments = T,
                                          longlat = T))
  ) %>%
  thicken('5 min', colname = 'time2') %>%
  group_by(time2) %>%
  summarise(
            filter400Distance = sum(filter400Distance)
  )%>% 
  pad()


# distance from home with NA's
xDisthome<- x %>% na.omit() %>% mutate(distHome = sp::spDistsN1(pts=matrix(c(lon,lat),ncol = 2),
              pt = home,
              longlat = T)) %>% select(time2,distHome)

x<- left_join(x, xDisthome,"time2")

x <- left_join(x, xFiltered)

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
  

distTrav<- x %>% 
  thicken('day', colname = 'time') %>%
  group_by(time) %>%
  summarise(nofilterDistance = sum(na.omit(nofilterDistance)),
            filter400Distance = sum(na.omit(filter400Distance))) %>%
  mutate(day = strftime(time,"%A"))

distTrav$day <- factor(distTrav$day, levels= c("Monday", 
                                           "Tuesday", "Wednesday",
                                           "Thursday", "Friday", "Saturday","Sunday"))


#wide to long

library(tidyr)
l<- distTrav %>%
  select(time,day,nofilterDistance,filter400Distance) %>%
  gather(filter,distance, nofilterDistance:filter400Distance, factor_key = T)


ggplot(l,aes(x = day, y = nofilterDistance))+
  geom_point()+theme_tufte()+xlab("Day of the week")+
  ylab("Distance in KM")+ggtitle("Distance Covered- February 2017")

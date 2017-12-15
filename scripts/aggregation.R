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

athome<- x %>% mutate(atHome = case_when(distHome <0.25 ~ 1,
                                distHome >0.25 ~ 0)) %>%
  thicken('day', colname = 'time') %>%
  select(time,atHome) %>%
  group_by(time) %>%
  summarise(atHomeProp = sum(atHome, na.rm = T)/288,
            missingProp = sum(is.na(atHome))/288) %>%
  mutate(day = strftime(time,"%a"),
         propNotHome = 1-atHomeProp-missingProp)

athome$day <- factor(athome$day, levels= c("Mon", 
                                         "Tue", "Wed",
                                         "Thu", "Fri", "Sat","Sun"))

athm<- athome %>%
  select(time,day,atHomeProp,propNotHome,missingProp) %>%
  gather(type,proportion, atHomeProp:missingProp)%>%
  as_tbl_time(index = time)
  

timeUse<- ggplot(athm,aes(x = time, y = proportion, fill=type))+
  geom_bar(stat="identity")+theme_tufte()+xlab("Day of the week")+
  ylab("Proportion of time")+
  ggtitle("Time spent at home",subtitle = "February 2017")+
  scale_fill_manual(values=c("black",muted("red"), "grey50"),labels=c("At Home", "Missing","Not Home"))+
  labs(x = "Day", fill = "")

#distance travel
distTrav<- x %>% 
  thicken('day', colname = 'time') %>%
  group_by(time) %>%
  summarise(na_count = sum(is.na(lon)),
    nofilterDistance = sum(na.omit(nofilterDistance)),
            filter400Distance = sum(na.omit(filter400Distance))) %>%
  mutate(day = strftime(time,"%a"))

distTrav$day <- factor(distTrav$day, levels= c("Mon", 
                                           "Tue", "Wed",
                                           "Thu", "Fri", "Sat","Sun"))


#wide to long

library(tidyr)
l<- distTrav %>%
  select(time,na_count,day,nofilterDistance,filter400Distance) %>%
  gather(filter,distance, nofilterDistance:filter400Distance) %>% filter(distance < 200)





displot<- ggplot(l,aes(x = day, y = distance, shape = filter, colour = na_count))+
  geom_point(position = position_dodge(width = 1))+theme_tufte()+xlab("Day of the week")+
  ylab("Distance in KM")+ggtitle("Distance Covered", subtitle = "February 2017")+
  scale_colour_gradient2(low = "black", mid = "grey50",
                         high = muted("red"), midpoint = 27, space = "Lab",
                         na.value = "yellow", guide = "colourbar")+
  labs(shape = "Filter",colour = "Missing\n 5 minute\nsegments")+
  scale_shape_discrete(labels = c("Accuracy <400 m", "No Filter"))

l <- summarySE(l, measurevar="len", groupvars=c("day","filter"))

displot<- ggplot(l, aes(x=day, y=distance, fill=filter)) + 
  geom_boxplot(position=position_dodge())+
  labs(fill = "Filter")+
  scale_fill_manual(values = c("grey50","black"),labels = c("Accuracy\n<400 m", "No Filter"))+
  ylab("Distance in KM")+xlab("Day of the Week")+
  ggtitle("Distance Covered", subtitle = "February 2017")+
  theme_tufte()
  



library(gridExtra)
aggPlot<- grid.arrange(displot,timeUse,ncol=2)

ggsave(aggPlot,filename = "../img/aggPlot.png",device = "png",height = 10,width = 20, units = "cm")


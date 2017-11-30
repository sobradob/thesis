# accurcay plot with rolling distance from previous point
# plz load packages and data first
#

rolmean<- rollify(mean,window = 3)
rolmax<- rollify(max,window = 3)

home<- c(5.113919,52.10421)

events <- data.frame(name = c("Journey 1", "Journey 2", "Journey 3","Journey 4","Journey 5","Journey 6"),
                     time =c(as.POSIXct("2017-02-15 08:00:00",
                                      tz= "Europe/Budapest"),
                     as.POSIXct("2017-02-15 10:00:00",
                                tz= "Europe/Budapest"),
                     as.POSIXct("2017-02-15 14:30:00",
                                tz= "Europe/Budapest"),
                     as.POSIXct("2017-02-15 17:30:00",
                                tz= "Europe/Budapest"),
                     as.POSIXct("2017-02-15 19:00:00",
                                tz= "Europe/Budapest"),
                     as.POSIXct("2017-02-15 20:45:00",
                                tz= "Europe/Budapest"))
                     )

  
distanceTime<- all %>%
  select(time,lon,lat,accuracy) %>%
  as_tbl_time(index = time) %>%
  time_filter(2017-02-15 ~ 2017-02-15)%>%
  mutate( distanceHome = sp::spDistsN1(pts=matrix(c(lon,lat),ncol = 2),
                                       pt = home,
                                       longlat = T),
          euclon = lon-home[1],
          euclat = lat-home[2],
          distancePrev = c(0,
                           sp::spDists(x=matrix(c(lon,lat),ncol = 2), segments = T)),
          rDistPrev = distancePrev) %>%
  ggplot( aes(x = time, y=rDistPrev*1000))+
  geom_line()+theme_tufte()+
  xlab("")+ylab("Distance over time")+
  scale_x_datetime(breaks = date_breaks("4 hour"),
                   minor_breaks=date_breaks("2 hour"),
                   labels=date_format("%H:%M:%S", tz = "Europe/Budapest"),
                   limits = xlim_p1)+
  geom_text(data=events,
            aes(x=time,y=50,label=name),size=3,angle = 90, colour= muted("red"))+
  ylim(c(-1,57))

accuracyTime <- all %>%
  select(time,lon,lat,accuracy) %>%
  as_tbl_time(index = time) %>%
  time_filter(2017-02-15 ~ 2017-02-15)%>%
  mutate( distanceHome = sp::spDistsN1(pts=matrix(c(lon,lat),ncol = 2),
                                       pt = home,
                                       longlat = T),
          euclon = lon-home[1],
          euclat = lat-home[2],
          distancePrev = c(0,
                           sp::spDists(x=matrix(c(lon,lat),ncol = 2), segments = T)),
          rDistPrev = accuracy) %>%
  ggplot( aes(x = time, y=rDistPrev))+
  geom_line()+theme_tufte()+
  xlab("Time")+ylab("Accuracy")+
  scale_x_datetime(breaks = date_breaks("4 hour"),
                   minor_breaks=date_breaks("2 hour"),
                   labels=date_format("%H:%M:%S", tz = "Europe/Budapest"),
                   limits = xlim_p1)
library(gridExtra)
library(grid)

locShift<- grid.arrange(distanceTime,accuracyTime,ncol=1)

#square
ggsave(locShift,filename = "../img/accuracyLocShiftSquare.png",device = "png",height = 6.5,width = 6.5, units = "cm")

#long
ggsave(locShift,filename = "../img/accuracyLocShift.png",device = "png",height = 15,width = 20, units = "cm")


# rolling distance from home, not very interesting

all %>%
  select(time,lon,lat,accuracy) %>%
  as_tbl_time(index = time) %>%
  time_filter(2017-02-15 ~ 2017-02-15)%>%
  mutate( distanceHome = sp::spDistsN1(pts=matrix(c(lon,lat),ncol = 2),
                                       pt = home,
                                       longlat = T),
          euclon = lon-home[1],
          euclat = lat-home[2],
          distancePrev = c(0,
                           sp::spDists(x=matrix(c(lon,lat),ncol = 2), segments = T)),
          rDistPrev = rolmean(distancePrev)) %>%
  ggplot( aes(x = time, y=distanceHome))+
  geom_line()+theme_tufte()+
  xlab("Time")+ylab("Rolling Mean Distance")+
  scale_x_datetime(breaks = date_breaks("4 hour"),
                   minor_breaks=date_breaks("2 hour"),
                   labels=date_format("%H:%M:%S", tz = "Europe/Budapest"),
                   limits = xlim_p1)


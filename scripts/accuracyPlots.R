## plots on accuracy
# missing days with padr
library(dplyr)
library(padr)
library(ggplot2)
library(tibbletime)
# read data
all<-readRDS("../../data/peter/myLocationHistory.rds")
all<-readRDS("../../data/boaz/myLocationHistory.rds")

all$time<- lubridate::with_tz(all$time, "Europe/Budapest")

accData <- all %>% select(accuracy) %>% filter (accuracy < 20000)
accuracy <- data.frame(accuracy = accData$accuracy,
                       group = ifelse(accData$accuracy < 800,"High",
                                      ifelse(accData$accuracy < 5000, "Middle",
                                             "Low")))

accuracy$group <- factor(accuracy$group, levels = c("High", "Middle", "Low"))

accuracyPlot<- ggplot(accuracy, aes(x = accuracy, fill = group)) + 
  geom_histogram() + 
  facet_grid(group ~ ., scales="free") + 
  theme_tufte() +
  theme(
    legend.position = "none",
    strip.placement = "outside",
    strip.background = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)
  ) +
  labs(
    x = "Accuracy in metres",
    y = "Count",
    title = "Location Log Accuracy"
  )+scale_fill_manual(values=c(muted("green"),"grey50",muted("red")))


ggsave(accuracyPlot,filename = "img/accuracyPeter.png",device = "png",height = 6.5, units = "cm")

#similar to the plot with distance from home and accuracy from palmius
library(leaflet)

lns<- all %>%
  select(time,lat,lon,accuracy) %>%
  filter(accuracy <400) %>% 
  as_tbl_time(index = time) %>%
  time_filter(2017-02-15 + 00:00:00 ~ 2017-02-15+ 12:00:00)%>%
  select(lon, lat) %>%
  sp::Line()
  

all %>%
  select(time,lat,lon,accuracy) %>%
  filter(accuracy <400) %>% 
  as_tbl_time(index = time) %>%
  time_filter(2017-02-15 + 00:00:00 ~ 2017-02-15+ 12:00:00) %>%
  leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addTiles() %>%
  addPolylines(data = lns,color = "#DF2935") %>%
  addCircles(lng = ~lon, lat = ~lat,
             radius = ~accuracy, fillOpacity = 0.02,color = "#DF2935")%>%
  addProviderTiles(providers$CartoDB.Positron)

# extract home
#52.10421 5.113919 
home<- c(5.113919,52.10421)
#calculate distance from home

boazday0218<- all %>%
  select(time,lon,lat,accuracy) %>%
  as_tbl_time(index = time) %>%
  time_filter(2017-02-15 ~ 2017-02-15)%>%
  mutate( distanceHome = sp::spDistsN1(pts=matrix(c(lon,lat),ncol = 2),
          pt = home,
          longlat = T),
          euclon = lon-home[1],
          euclat = lat-home[2],
          time2 = lubridate::force_tz(time, "Asia/Singapore"))

ggplot(boazday0218, aes(x = time, y=distanceHome, colour= accuracy))+
  geom_point()+theme_tufte()

ggplot(boazday0218,aes(x = euclon,y = euclat, colour = accuracy))+geom_point()+theme_tufte()

# missing: number of inaccurate measurements visualised effectively
# missing: arrows showing movmeent


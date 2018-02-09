# accuracy and time of day plot

# new scatterplot

all %>%
  select(time,lon,lat,accuracy) %>%
  as_tbl_time(index = time) %>%
  time_filter(2016-10-02 ~ 2016-10-07)%>%
  mutate( time2 = lubridate::force_tz(time, "Asia/Singapore"),
          distancePrev = c(0,
                           sp::spDists(x=matrix(c(lon,lat),ncol = 2), segments = T)),
          hour = as.POSIXct(
            paste0("2014-01-22 ",
                   strftime(time,format = "%H:%M", tz = "Europe/Budapest")))) %>%
  ggplot( aes(x = hour, y=accuracy))+
  geom_point()+theme_tufte()
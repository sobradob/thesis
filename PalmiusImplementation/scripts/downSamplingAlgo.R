# down sampling palmius algo

# preceded by the filtering algo

# divide in 1 hour epochs
library(padr)

# if lattitude and longitude sd both less than 0.01km
# then replace with mean
deg2rad <- function(deg) {(deg * pi) / (180)}

#something fishy,figure out what it is

d2<- d %>% thicken('hour') %>% 
  group_by(time_hour) %>%
  summarise(sdLat = sd(lat , na.rm = T),
            sdLon = sd(lon , na.rm = T),
            meanLat = mean(lat),
            meanLon = mean(lon)) %>%
  mutate(sdLatkm = sdLat*110.574,
         sdLonkm = 111.320*cos(deg2rad(meanLat))
         ) %>% 
  pad() %>% 
  fill_by_value( "NA") %>% 
  filter(sdLatkm < 0.01 & sdLonkm < 0.01)

summary(d2$sdLatkm)

# Latitude: 1 deg = 110.574 km
# Longitude: 1 deg = 111.320*cos(latitude) km


#alternative version using pythagoras and distance via haversine forumla
#in meters
sqrt(10^2+10^2)

d3<- d %>% thicken('hour') %>% 
  group_by(time_hour) %>%
  summarise(distancesMeterSd = sd(distancesMeter , na.rm = T),
            meanLat = mean(lat),
            meanLon = mean(lon)) %>% 
  pad() %>% 
  fill_by_value( "NA") %>%
  filter(distancesMeterSd < 14.14) %>%
  select(time_hour,meanLat,meanLon)

dF<- d %>% thicken('hour') %>%
  left_join(d3) %>%
  mutate(
    lonF1 = case_when(
      is.na(meanLon) == F ~ meanLon
    ),
    latF1 = case_when(
      is.na(meanLat) == F ~ meanLat
    )
  )

dF2<- dF %>% thicken(by = "time","5 min",colname= "time_5min") %>% 
  group_by(time_5min) %>% 
  summarise(lonF2 = median(lon),
            latF2 = median(lat)
            )
  
d.filt <- dF %>% thicken(by = "time","5 min",colname= "time_5min") %>% 
  left_join(dF2, by = "time_5min") %>%
  select(time, time_5min, lat,lon, accuracy, latF1,lonF1,latF2,lonF2) %>% 
  mutate(
    lonF = case_when(
    is.na(lonF1) ==F ~ lonF1,
    is.na(lonF1) == T ~lonF2),
    latF = case_when(
      is.na(lonF1 ) == F ~ latF1,
      is.na(lonF1 ) == T ~ latF2
    )
  )

#summerise and filter and plot and shit
raw<- d.filt %>% 
  select(time_5min, lonF,latF)%>%
  distinct() %>%
  as_tbl_time(time_5min) %>% 
  filter_time(~ "2017-02-15")

d.f1<- downSamplingPalmius(d) %>%
  as_tbl_time(time_5min) %>% 
  filter_time(~ "2017-02-15")

d.f1 %>% 
  leaflet() %>%
  addTiles() %>%
  addCircles(lng = ~lonF, lat = ~latF)%>%
  addProviderTiles(providers$CartoDB.Positron)

d.filt %>% 
  leaflet() %>%
  addTiles() %>%
  addCircles(lng = ~lonF, lat = ~latF)%>%
  addProviderTiles(providers$CartoDB.Positron)

# rolling 5 min filter
test <- d %>% as_tbl_time(time) %>% 
  filter_time(~ "2017-02-15")

for(i in 1: nrow(test)){
  timePoint <- test$time[i]
  latlon<- test %>% filter(time > timePoint - 60*2.5 &  time < timePoint + 60*2.5) %>%
    select(time,lat,lon) %>%
    summarise(lat = median(lat),
              lon = median(lon))
  test[i,c("lat","lon")] <- latlon
}

 
leaflet() %>%
  addTiles() %>%
  addCircles(lng = ~lon, lat = ~lat)%>%
  addProviderTiles(providers$CartoDB.Positron)

filtered <- d.filt %>% as_tbl_time(time) %>% 
  filter_time(~ "2017-02-15")

raw <- dataExample %>% as_tbl_time(time) %>% 
  filter_time(~ "2017-02-15")

saveRDS(filtered,"filtered.rds")
saveRDS(raw,"raw.rds")

 
leaflet() %>%
  addTiles() %>%
  addCircles(lng = ~lonF, lat = ~latF, data = raw, color = "#18206F", group = "Raw")%>%
  addCircles(lng = ~lon, lat = ~lat, data=filtered,color = "#DF2935",group = "Filtered")%>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addLayersControl(
    overlayGroups = c("Raw", "Filtered"),
    options = layersControlOptions(collapsed = FALSE)
  )

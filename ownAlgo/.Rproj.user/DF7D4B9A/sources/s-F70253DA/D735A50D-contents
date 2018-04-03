#check clustering of measurements

day <- fin %>% as_tbl_time(time)%>%
  filter_time((~ "2017-03-16"))

left_join(day,allPoints, by = "clust") %>% 
leaflet() %>% 
  addTiles() %>% 
  addCircles(lng = ~lon.y, lat = ~lat.y, color = "red", radius = 10,
             label = ~as.character(clust)) %>% 
  addCircles(lng = ~lon.x, lat = ~lat.x, color = "green", radius = 10,
             label = ~as.character(clust))


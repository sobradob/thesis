# results for evalutating binning
# read in allpoints and end results of binning if not already loaded

library(ggplot2)
library(ggthemes)

# calculate percentage within accuracy
fin %>%
  summarise(mean(accuracy >= distClustMin))

# calculate minimum deviance
fin %>%
  select(distClustMin) %>% summary(d)

#generate plots
fin %>% 
  ggplot(aes(x = accuracy, y = distClustMin))+
  geom_point()+
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0,
              na.rm = FALSE, show.legend = NA, colour = "grey")+
  theme_tufte()+
  ggtitle("Deviance vs Accuracy",subtitle = "Within 300km Utrecht")+
  xlab("Accuracy")+ylab("Deviance")

# calculate percentage March
fin %>%
  as_tbl_time(time) %>% 
  filter_time(~"2017-03") %>% 
  summarise(mean(accuracy >= distClustMin))

# calculate deviance March
fin %>%
  as_tbl_time(time) %>% 
  filter_time(~"2017-03") %>% 
  select(distClustMin) %>% summary(d)

fin %>%
  as_tbl_time(time) %>% 
  filter_time(~"2017-03") %>% 
  ggplot(aes(x = accuracy, y = distClustMin))+
  geom_point()+
  geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0,
              na.rm = FALSE, show.legend = NA, colour = "grey")+
  theme_tufte()+
  ggtitle("Deviance vs Accuracy",subtitle = "March 2017")+
  xlab("Accuracy")+ylab("Deviance")

# Create a plot compared to Palmius
plotBin<- left_join(fin %>% as_tbl_time(time) %>% filter_time(~"2017-03-01"),allPoints,by="clust", suffix=c(".measured",".cluster"))

plotBin <- left_join(plotBin %>% thicken("5 min"),
          downsampled,
          by =c("time_5_min" = "time_5min"))

plotBin %>% 
leaflet() %>% 
  addCircles(lng = ~jitter(lon.measured), lat = ~jitter(lat.measured), color = "#18206F") %>% 
  addCircles(lng = ~lonF,lat = ~latF, color = "yellow",label = ~as.character(time_5_min)) %>% 
  addCircles(lng = ~lon.cluster,lat = ~lat.cluster, color = "#DF2935",label = ~as.character(time_5_min))%>%
  addProviderTiles(providers$CartoDB.Positron)
  

# ggplot with lines

ggplot(plotBin, aes(x = lon.cluster, y = lat.cluster))+
  geom_path()+
  geom_path(aes(x = lonF, y = latF, colour = "red"))+
  coord_fixed()+
  theme_tufte()+
  xlab("Longitude")+
  ylab("Latitude")+
  geom_point(aes(x = lon.measured, y = lat.measured), shape = 1, colour = "blue")+
  theme(legend.position="bottom ") +
  geom_point( data = filter(plotBin, accuracy >800), aes(x = lon.measured, y = lat.measured,color = "green"))+
  ggtitle("Binning Method Comparison",subtitle = "01-03-2017")



# missing days with padr
library(dplyr)
library(padr)
library(ggplot2)
library(tibbletime)
# read data
all<-readRDS("../data/peter/myLocationHistory.rds")
attr(all$time, "tzone") <- "Europe/Paris"


mHour<- all %>%
  select(time) %>%
  as_tbl_time(index = time) %>%
  time_filter(2000-01 ~ 2017-06) %>% 
  thicken('hour') %>%
  group_by(time_hour) %>%
  summarise(measurements = n()) %>%
  pad() %>%
  fill_by_value(value = 0) %>%
  thicken("day") %>%
  mutate(
    type = case_when(
      measurements== 0 ~ 1,
      measurements > 0 ~ 0)) %>%
  group_by(time_hour_day) %>%
  summarise(hoursMissing = sum(type),
            measurements = sum(measurements))

library(scales)
missingDay<- ggplot(mHour, aes(x = time_hour_day, y=measurements, colour= hoursMissing))+
  geom_point()+theme_tufte()+
  xlab("")+
  ylab("Measurements")+
  ggtitle("Amount of daily measurements over time")+
  scale_colour_gradient2(low = muted("green"), mid = "grey50",
                         high = muted("red"), midpoint = 12, space = "Lab",
                         na.value = "grey50", guide = "colourbar")+
  labs(x = NULL, colour = "Missing Hours")

ggsave(missingDay,filename = "img/missingdayPeter.png",device = "png",height = 6.5, units = "cm")

accuracyData<- all %>%
  as_tbl_time(time) %>%
  select(time, accuracy)

hist(log(accuracyData$accuracy))
## thing about this later, now do a dayplot of accuracy


accuracyData<- all %>%
  as_tbl_time(time) %>%
  select(time, accuracy)%>%
  time_filter(2016-02-05~2016-02-05)

ggplot(accuracyData, aes(x = time, y=accuracy))+
  geom_point()+theme_tufte()+
  xlab("")+
  ylab("Accuracy in Meters")+
  ggtitle("Accuracy of measures in a day")


  scale_colour_gradient2(low = muted("green"), mid = "grey50",
                         high = muted("red"), midpoint = 12, space = "Lab",
                         na.value = "grey50", guide = "colourbar")+
  labs(x = NULL, colour = "Missing Hours")

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

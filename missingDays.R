##Graphs for determining where missing data is
library(ggthemes)
library(dplyr)

# would be a lot easier to do with padr

# read data
loc<- readRDS("..\\all.rds")


pointsdaily<-loc %>% group_by(Day) %>% summarise(count = n())
pointshourly<-loc %>% group_by(Day, lubridate::hour(time)) %>%
  summarise(count=n())
rm(loc)

colnames(pointshourly) <- c("Day","Hour","count")

#generate dates
x<-as.Date(pointsdaily$Day[1]):as.Date(pointsdaily$Day[nrow(pointsdaily)])
x<-as.Date.numeric(x = x,origin="1970-01-01")
h<- sort(rep(0:23,length(x)))
allDayHour <- data.frame(Day = rep(x,24), Hour = h, count = 0)

#dates with zero measurements
noMeasureDay<-as.Date(setdiff(x,pointsdaily$Day), origin = "1970-01-01")
# alternative approach

u<- unique(rbind(pointshourly[,-3],allDayHour[,-3]))
allDayHourPasted<- paste0(allDayHour$Day,"$",allDayHour$Hour)
measuredDayHourPasted <- paste0(pointshourly$Day,"$",pointshourly$Hour)
noMeasureHour<- setdiff(allDayHourPasted,measuredDayHourPasted)
noMeasureHour<-strsplit(noMeasureHour,split = "\\$")
#hour with zero measurements

noMeasureHour <- setNames(do.call(rbind.data.frame, noMeasureHour), c("Day", "Hour"))
noMeasureHour$count <- 0
noMeasureHour$Day <- as.Date(noMeasureHour$Day)
noMeasureHour$Hour <- as.numeric(noMeasureHour$Hour)

t<- data.frame(table(noMeasureHour$Day))
nm<- filter(t,Freq != 24 & Freq>=6) %>% select(Var1)
nm<- as.Date(as.character(nm$Var1))

ps<-rbind(pointsdaily,data.frame(Day = noMeasureDay,count = 0))
ps$type <- "More than 6 hours"
ps[which(ps$Day%in%nm),"type"] <- "6 hours or less"
ps[which(ps$Day%in%noMeasureDay),"type"] <- "No Measure"
saveRDS(object = ps,file = "missingDataAnalysisBoaz.rds")

pal<- c("#4228A8", "#0D0101","#E04AC2")

ggplot(ps, aes(x = Day, y=count, fill = type, colour= type))+geom_point()+
  theme_tufte()+
  xlab("")+
  ylab("Measurements")+
  ggtitle("Missing data over time")+
  scale_colour_manual(values=pal)+
  theme(legend.position = c(0.8, 0.8))+
  theme(legend.title=element_blank())



ggplot(ps, aes(x = Day, y=count))+geom_point()+
  geom_point(data=ps[which(ps$Day%in%noMeasureDay),], aes(x=Day, y=count), colour="red")+
  geom_point(data=ps[which(ps$Day%in%nm),], aes(x=Day, y=count), colour="blue")+
  theme_tufte()+
  xlab("")+
  ylab("Measurements")+
  ggtitle("Missing data over time")

intersect(ps$day,noMeasure)
ps[,]


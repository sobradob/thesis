# animations
library(gganimate)

p<- ggplot(boazday0218,aes(x = euclon,y = euclat, colour = accuracy, frame = time))+geom_point()+theme_tufte()

gganimate(p,"output.html",interval = .2)

box<- c(left = min(boazday0218$lon)-0.01,
        bottom = min(boazday0218$lat)-0.01,
        right = max(boazday0218$lon)+0.01,
        top = max(boazday0218$lat)+0.01)
library(ggmap)
map<- get_stamenmap(bbox=box,zoom = 15, maptype = "toner-lite")
q<- ggmap(map)+
  geom_point(aes(lon,lat,frame = time2,colour = accuracy),data = boazday0218)+xlab("")+ylab("")

gganimate(q,"output.gif",interval = .2,ani.width=1000, ani.height=800)
library(magick)

gganimate(q,"output.mp4",interval = .1,ani.width=500, ani.height=400, ani.res = 300)


install.packages("installr")
library(installr); install.ImageMagick(URL = "http://www.imagemagick.org/script/download.php")


Sys.setenv(PATH = paste("C:/Program Files/ImageMagick/bin",
                        Sys.getenv("PATH"), sep = ";"))

devtools::install_github("yihui/animation")
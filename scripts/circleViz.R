# Lets make some data
circles <- data.frame(
  x0 = rep(1:3, 3),
  y0 =  rep(1:3, each=3),
  r = seq(0.1, 1, length.out = 9)
)

install.packages("ggforce")
library(ggforce)
# Behold the some circles
ggplot() + geom_circle(aes(x0=x0, y0=y0, r=r), data=circles)

# Use coord_fixed to ensure true circularity
ggplot() + geom_circle(aes(x0=x0, y0=y0, r=r, fill=r), data=circles) +
  coord_fixed()

#each degree is this much km
40075/360

#each degree is this much m
metersPerDeg <- (40075/360*1000)


ggplot(boazday0218,aes(x = euclon,y = euclat, colour = accuracy))+
  geom_point()+
  theme_tufte()+
  geom_circle(aes(x0=euclon, y0=euclat, r= accuracy/metersPerDeg))
  

              
      
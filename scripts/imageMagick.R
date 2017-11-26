#image magick
library(magick)
inac <- image_read("../img/journeyTillMiddayBoaz.png")
ac <- image_read("../img/accuracyLocShift.png")

im2 <- c(inac,ac)
x<- image_append(image_scale(im2,"x500"))
image_write(x,"../img/accPlots.png")
# doesnt; work


image_mosaic(ac, inac)


bigdata <- image_read('https://jeroen.github.io/images/bigdata.jpg')
frink <- image_read("https://jeroen.github.io/images/frink.png")
logo <- image_read("https://www.r-project.org/logo/Rlogo.png")
img <- c(bigdata, logo, frink)
img <- image_scale(img, "300x300")
image_mosaic(img)
image_flatten(img)

image_append(image_scale(img,"x200"))

left_to_right <- image_append(image_scale(img, "x200"))
image_background(left_to_right, "white", flatten = TRUE)

top_to_bottom <- image_append(image_scale(img, "100"), stack = TRUE)
image_background(top_to_bottom, "white", flatten = TRUE)

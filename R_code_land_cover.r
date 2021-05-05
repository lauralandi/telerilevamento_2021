### LAND COVER

setwd("C:/lab/")

library(raster)
library(RStoolbox)
library(ggplot2) 
library(gridExtra)

defor1 <- brick("defor1.jpg") 
defor2 <- brick("defor2.jpg")

# defor1.1 = NIR
# defor1.2 = red
# defor1.3 = green

### GGPLOT2

par(mfrow=c(2,1))
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

## multiframe con ggplot con gridExtra
p1<-ggRGB(defor1, r=1, g=2, b=3, stretch="Lin") #ggRGB da ggplot2
p2<-ggRGB(defor2, r=1, g=2, b=3, stretch="Lin")
grid.arrange(p1,p2,nrow=2) #grid.arrange da gridExtra


##########

d1c <- unsuperClass(defor1, nClasses=2)
cl <- colorRampPalette(c('black','green'))(100)
cl2 <- colorRampPalette(c('green','black'))(100)
par(mfrow=c(2,1))
plot(d1c$map, col=cl)
plot(d1c$map, col=cl2)

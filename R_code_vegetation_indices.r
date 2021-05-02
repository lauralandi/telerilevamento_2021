### INDICI DI VEGETAZIONE

library(raster)
library(RStoolbox)

setwd("C:/lab/")

defor1<-brick("defor1.jpg")
defor2<-brick("defor2.jpg")

### B1=NIR B2=RED B3=GREEN

par(mfrow=c(2,1))
plotRGB(defor1, r=1, g=2, b=3, stretch='lin')
plotRGB(defor2, r=1, g=2, b=3, stretch='lin')

#le bande dentro defor1 si chiamano defor1.1, defor 1.2, defor1.3
cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100) # specifying a color scheme
DVI1<-defor1$defor1.1 - defor1$defor1.2 #calcolo DVI sottraendo B1-B2 (NIR-RED)
plot(DVI1, col=cl, main="DVI at time 1")

DVI2<-defor2$defor2.1 - defor2$defor2.2 #calcolo DVI sottraendo B1-B2 (NIR-RED)
plot(DVI2, col=cl, main="DVI at time 2")

par(mfrow=c(2,1))
plot(DVI1, col=cl, main="DVI at time 1")
plot(DVI2, col=cl, main="DVI at time 2")

#sottraiamo il secondo DVI al primo per capire di quanto è calato nel tempo
deltaDVI<- DVI1 - DVI2
cld <- colorRampPalette(c('blue','white','red'))(100) 
plot(deltaDVI, col=cld, main="differenza DVI")

## calcolo del NDVI
## NDVI= (NIR-RED)/(NIR+RED)

NDVI1<-(defor1$defor1.1-defor1$defor1.2)/(defor1$defor1.1+defor1$defor1.2)
NDVI2<-(defor2$defor2.1-defor2$defor2.2)/(defor2$defor2.1+defor2$defor2.2)
par(mfrow=c(2,1))
plot(NDVI1, col=cl, main="NDVI at time 1")
plot(NDVI2, col=cl, main="NDVI at time 2")

deltaNDVI<- NDVI1 - NDVI2
plot(deltaNDVI, col=cld, main="differenza DVI")


## RStoolbox:: spectralIndices

vi1<-spectralIndices(defor1, green=3, red=2, nir=1)
plot(vi1, col=cl)

vi2 <- spectralIndices(defor2, green = 3, red = 2, nir = 1)
plot(vi2, col=cl)

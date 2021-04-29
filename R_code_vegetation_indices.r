### INDICI DI VEGETAZIONE

library(raster)

setwd("C:/lab/")

defor1<-brick("defor1.jpg")
defor2<-brick("defor2.jpg")

### B1=NIR B2=RED B3=GREEN

par(mfrow=c(2,1))
plotRGB(defor1, r=1, g=2, b=3, stretch='lin')
plotRGB(defor2, r=1, g=2, b=3, stretch='lin')

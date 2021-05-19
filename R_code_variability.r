### VARIABILITA' SPAZIALE

library(raster)
library(RStoolbox)
library(ggplot2)
library(viridis)
library(gridExtra)

setwd("C:/lab/")

sentinel<-brick("sentinel.png")
###  1= NIR 2=RED 3=GREEN

plotRGB(sentinel, r=1, g=2, b=3, stretch="lin")
plotRGB(sentinel, r=2, g=1, b=3, stretch="lin")

### calcolo NDVI per ridurre il raster a un solo layer e poter usare la finestra mobile

NIR<-sentinel$sentinel.1
RED<-sentinel$sentinel.2
GREEN<-sentinel$sentinel.3
NDVI=(NIR-RED)/(NIR+RED)
plot(NDVI)

cl <- colorRampPalette(c('black','white','red','magenta','green'))(100)
plot(NDVI, col=cl)

### usare la funzione focal (da pacchetto raster) per analisi statistica utilizzando la moving window
# con w definisco le dimensioni della moving window, con fun la variabile statistica da calcolare

#deviazione standard
NDVI_sd3<-focal(NDVI, w=matrix(1/9,nrow=3,ncol=3), fun=sd)
cl2 <- colorRampPalette(c('blue','green','pink','magenta','orange','brown','red','yellow'))(100)
plot(NDVI_sd3, col=cl2)

#media
NDVI_sd3<-focal(NDVI, w=matrix(1/9,nrow=3,ncol=3), fun=mean)
plot(NDVI_sd3, col=cl2)

#moving window a dimensione diversa
NDVI_sd3<-focal(NDVI, w=matrix(1/81,nrow=9,ncol=9), fun=sd)
plot(NDVI_sd3, col=cl2)

NDVI_sd3<-focal(NDVI, w=matrix(1/25,nrow=5,ncol=5), fun=sd)
plot(NDVI_sd3, col=cl2)

#### ricalcolare statistica ma su PCA
sentinel_pca<-rasterPCA(sentinel)
plot(sentinel_pca$map)
summary(sentinel_pca$model)
# la PC1 spiega il 67,3% della variabilità totale



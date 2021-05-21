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
NDVI_m3<-focal(NDVI, w=matrix(1/9,nrow=3,ncol=3), fun=mean)
plot(NDVI_m3, col=cl2)

#moving window a dimensione diversa
NDVI_sd9<-focal(NDVI, w=matrix(1/81,nrow=9,ncol=9), fun=sd)
plot(NDVI_sd9, col=cl2)

NDVI_sd5<-focal(NDVI, w=matrix(1/25,nrow=5,ncol=5), fun=sd)
plot(NDVI_sd5, col=cl2)

#### ricalcolare statistica ma su PCA
sentinel_pca<-rasterPCA(sentinel)
plot(sentinel_pca$map)
summary(sentinel_pca$model)
# la PC1 spiega il 67,3% della variabilità totale

PC1<-sentinel_pca$map$PC1
PC1_sd3<-focal(PC1, w=matrix(1/9,nrow=3,ncol=3), fun=sd)
plot(PC1_sd3, col=cl2)

PC1_sd5<-focal(PC1, w=matrix(1/25,nrow=5,ncol=5), fun=sd)
plot(PC1_sd5, col=cl2, main="PC1 standard deviation")

PC1_sd7<-focal(PC1, w=matrix(1/49,nrow=7,ncol=7), fun=sd)
plot(PC1_sd7, col=cl2, main="PC1 standard deviation")

###############
# plot con ggplot e viridis
# The package viridis contains eight color scales: “viridis”, the primary choice, and five alternatives with similar properties 
# - “magma”, “plasma”, “inferno”, “cividis”, “mako”, and “rocket” -, and a rainbow color map - “turbo”

p0 <- ggplot() + geom_raster(PC1_sd5, mapping = aes(x = x, y = y, fill = layer)) + scale_fill_viridis() +  ggtitle("viridis palette")

p1 <- ggplot() + geom_raster(PC1_sd5, mapping = aes(x = x, y = y, fill = layer)) + scale_fill_viridis(option="magma") +  ggtitle("magma palette")

p2 <- ggplot() + geom_raster(PC1_sd5, mapping = aes(x = x, y = y, fill = layer)) + scale_fill_viridis(option="plasma") +  ggtitle("plasma palette")

p3 <- ggplot() + geom_raster(PC1_sd5, mapping = aes(x = x, y = y, fill = layer)) + scale_fill_viridis(option="inferno") +  ggtitle("inferno palette")

p4 <- ggplot() + geom_raster(PC1_sd5, mapping = aes(x = x, y = y, fill = layer)) + scale_fill_viridis(option="cividis") +  ggtitle("cividis palette")

p5 <- ggplot() + geom_raster(PC1_sd5, mapping = aes(x = x, y = y, fill = layer)) + scale_fill_viridis(option="mako") +  ggtitle("mako palette")

p6 <- ggplot() + geom_raster(PC1_sd5, mapping = aes(x = x, y = y, fill = layer)) + scale_fill_viridis(option="rocket") + ggtitle("rocket palette")

p7 <- ggplot() + geom_raster(PC1_sd5, mapping = aes(x = x, y = y, fill = layer)) + scale_fill_viridis(option="turbo") + ggtitle("turbo palette")

grid.arrange(p0, p1, p2, p3, p4, p5, p6, p7, nrow = 2)



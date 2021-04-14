### VISUALIZZAZIONE DATI COPERNICUS

install.packages("ncdf4")
library(raster)
library(ncdf4)
library(RStoolbox) #richiamo il pacchetto RStoolbox
library(ggplot2) #richiamo il pacchetto ggplot2
library(rasterVis)

setwd("C:/lab/")

SWI<-raster("c_gls_SWI1km_202104071200_CEURO_SCATSAR_V1.0.1.nc")

cl <- colorRampPalette(c("red","pink","light blue","blue"))(100)
cl2 <- colorRampPalette(c("red","yellow","pink","white"))(100)
#plot(SWI,col=cl)
levelplot(SWI,col.regions=cl2)

## resampling
SWIres<-aggregate(SWI,fact=10)
plot(SWIres,col=cl,main="Soil Water Index")

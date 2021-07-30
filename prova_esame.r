PROVA ESAME

library(raster) #richiamo il pacchetto raster
library(RStoolbox) #richiamo il pacchetto RStoolbox
library(ggplot2) #richiamo il pacchetto ggplot2
library(gridExtra)
library(multipanelfigure)
library(rgdal)
#install.packages("multipanelfigure")

setwd("C:/lab/esame/")

rlist12<-list.files(pattern="luglio12") #creo una lista di file e associo alla variabile rlist
import12<-lapply(rlist12,raster) #applico la funzione raster su tutti i file della lista e associo alla variabile import (in questo modo li importo tutti insieme)
luglio12<-stack(import12) # creo un univo file che contiene tutti quelli della lista importata

rlist18<-list.files(pattern="luglio18") #creo una lista di file e associo alla variabile rlist
import18<-lapply(rlist18,raster) #applico la funzione raster su tutti i file della lista e associo alla variabile import (in questo modo li importo tutti insieme)
luglio18<-stack(import18) # creo un univo file che contiene tutti quelli della lista importata

rlistS2<-list.files(pattern="S2") #creo una lista di file e associo alla variabile rlist
importS2<-lapply(rlistS2,raster) #applico la funzione raster su tutti i file della lista e associo alla variabile import (in questo modo li importo tutti insieme)
S2<-stack(importS2) # creo un univo file che contiene tutti quelli della lista importata

#pdf("prova1")
par(mfrow=c(2,1))
plotRGB(luglio12, 4, 5, 2, stretch="lin")
plotRGB(luglio18, 4, 5, 2, stretch="lin")
#dev.off()

cl5<-colorRampPalette(c('black','white'))(100)

#pdf("provareno")
par(mfrow=c(2,1), mar=c(2,2,2,2))
plot(luglio12$luglio12_S1_IW_DV_VV_dB, col=cl5, main="12 Luglio")
plot(luglio18$luglio18_S1_IW_DV_VV_dB, col=cl5, main="18 Luglio")
#dev.off()

set.seed(42)
luglio12_c2 <- unsuperClass(luglio12, nClasses=2)
luglio18_c2 <- unsuperClass(luglio12, nClasses=2)

cl6 <- colorRampPalette(c('red','green'))(100)
plot(luglio12_c2$map, col=cl6)
plot(luglio18_c2$map, col=cl6)

pdf("prova2")
par(mfrow=c(2,1))
plotRGB(luglio12, 4, 5, 2, stretch="hist")
plotRGB(luglio18, 4, 5, 2, stretch="hist")
dev.off()

par(mfrow=c(2,1))
plotRGB(S2, 4, 3, 2, stretch="lin")
plotRGB(S2, 4, 3, 2, stretch="hist")

plotRGB(S2, 8, 4, 3, stretch="hist")


###################################à

setwd("C:/lab/esame2/")

rlist<-list.files(pattern=".tiff") #creo una lista di file e associo alla variabile rlist
import<-lapply(rlist,raster) #applico la funzione raster su tutti i file della lista e associo alla variabile import (in questo modo li importo tutti insieme)
S2<-stack(import) # creo un univo file che contiene tutti quelli della lista importata

setMinMax(S2$B04)
maxValue(S2$B04)

m <- raster(ncol=2500, nrow=1202, xmn=616993.4, xmx=733024.8, ymn=6715299, ymx=6771098) #creo maschera come raster delle stesse dimesioni
m[m > 0.363925] <- NA #rendo nulli i valori superiori al 25% del max di B04

maskedB04 <- mask(S2$B04, m, filename="maskedB04", maskvalue=NA)
  

###################################à

setwd("C:/lab/esame3/")

rlist20<-list.files(pattern="july20") #creo una lista di file e associo alla variabile rlist
import20<-lapply(rlist20,raster) #applico la funzione raster su tutti i file della lista e associo alla variabile import (in questo modo li importo tutti insieme)
july20<-stack(import20) # creo un unico file che contiene tutti quelli della lista importata

rlist27<-list.files(pattern="july27") #creo una lista di file e associo alla variabile rlist
import27<-lapply(rlist27,raster) #applico la funzione raster su tutti i file della lista e associo alla variabile import (in questo modo li importo tutti insieme)
july27<-stack(import27) # creo un unico file che contiene tutti quelli della lista importata

plotRGB(july20, 4, 3, 2, stretch="lin")
plotRGB(july27, 4, 3, 2, stretch="lin")

### PLOT IN FALSI COLORI

pdf("sardegna")
par(mfrow=c(2,1))
plotRGB(july20, 8, 3, 2, stretch="lin")
plotRGB(july27, 8, 3, 2, stretch="lin")
dev.off()

cldmsk<-cloudMask(july27, threshold = 0.03, blue = 2, tir = 12, plot = TRUE)
masked_july27_TC <- mask(july27$july27_TC, cldmsk, inverse =TRUE, maskvalue=NA)
masked_july27_B04 <- mask(july27$july27_B04, cldmsk, inverse =TRUE, maskvalue=NA)
masked_july27_B03 <- mask(july27$july27_B03, cldmsk, inverse =TRUE, maskvalue=NA)
masked_july27_B02 <- mask(july27$july27_B02, cldmsk, inverse =TRUE, maskvalue=NA)
masked_july27_B08 <- mask(july27$july27_B08, cldmsk, inverse =TRUE, maskvalue=NA)

masked_july27<-stack(masked_july27_B02$layer.1, masked_july27_B03$layer.1, masked_july27_B04$layer.1, masked_july27_B08$layer.1, masked_july27_TC$layer.1)

plotRGB(masked_july27, 4, 3, 2, stretch="lin")

masked_july20_TC <- mask(july20$july20_TC, cldmsk, inverse =TRUE, maskvalue=NA)
masked_july20_B04 <- mask(july20$july20_B04, cldmsk, inverse =TRUE, maskvalue=NA)
masked_july20_B03 <- mask(july20$july20_B03, cldmsk, inverse =TRUE, maskvalue=NA)
masked_july20_B02 <- mask(july20$july20_B02, cldmsk, inverse =TRUE, maskvalue=NA)
masked_july20_B08 <- mask(july20$july20_B08, cldmsk, inverse =TRUE, maskvalue=NA)

masked_july20<-stack(masked_july20_B02$layer.1, masked_july20_B03$layer.1, masked_july20_B04$layer.1, masked_july20_B08$layer.1, masked_july20_TC$layer.1)

par(mfrow=c(2,1))
plotRGB(masked_july20, 4, 3, 2, stretch="lin")
plotRGB(masked_july27, 4, 3, 2, stretch="lin")

set.seed(42)
july20_c4 <- unsuperClass(masked_july20, nClasses=4)
july27_c4 <- unsuperClass(masked_july27, nClasses=4)

cl1 <- colorRampPalette(c('blue4','green', "dark green", "blue"))(100)
cl2 <- colorRampPalette(c('green','blue', "red", "dark green"))(100)

par(mfrow=c(2,1))
plot(july20_c4$map, col=cl1)
plot(july27_c4$map, col=cl2)

set.seed(42)
july20_c2 <- unsuperClass(masked_july20, nClasses=2)
july27_c2 <- unsuperClass(masked_july27, nClasses=2)

cl <- colorRampPalette(c("dark green", "red"))(100)

par(mfrow=c(2,1))
plot(july20_c2$map, col=cl, main="20 Luglio")
plot(july27_c2$map, col=cl, main="27 Luglio")

#####################

p1 <- ggRGB(july20,4,3,2, stretch="lin")
p2 <- ggRGB(july27,4,3,2, stretch="hist")

grid.arrange(p1, p2, nrow = 2)



###################à

#caricamento immagini prima e dopo
# classificazione e calcolo frequenza classi sulla foto bruciata per calcolare percentuale area incendiata
# creare dataframe e grafico
# calcolo ndvi e confronto prima e dopo
# calcolo firm spettrale ?


##########################à

#   PROVA SARDEGNA

setwd("C:/lab/esame_sardegna/")

rlist10<-list.files(pattern="july10") #creo una lista di file e associo alla variabile rlist
import10<-lapply(rlist10,raster) #applico la funzione raster su tutti i file della lista e associo alla variabile import (in questo modo li importo tutti insieme)
july10<-stack(import10) # creo un unico file che contiene tutti quelli della lista importata

rlist25<-list.files(pattern="july25") #creo una lista di file e associo alla variabile rlist
import25<-lapply(rlist25,raster) #applico la funzione raster su tutti i file della lista e associo alla variabile import (in questo modo li importo tutti insieme)
july25<-stack(import25) # creo un unico file che contiene tutti quelli della lista importata

par(mfrow=c(2,1))
plotRGB(july10, 4, 3, 2, stretch="lin")
plotRGB(july25, 4, 3, 2, stretch="lin")


## PLOT IN FALSI COLORI
plotRGB(july25, 11, 12, 4, stretch="lin")

par(mfrow=c(2,1))
plotRGB(july10, 4, 3, 2, stretch="lin", main="10 Luglio RGB")
plotRGB(july25, 11, 12, 4, stretch="lin", main="25 Luglio falsi colori")

e <- drawExtent()

july25_crop<- crop(july25, e)
july10_crop<- crop(july10, e)

par(mfrow=c(2,1))
plotRGB(july10_crop, 4, 8, 2, stretch="lin")
plotRGB(july25_crop, 11, 12, 4, stretch="lin")


set.seed(42)
july10_c3 <- unsuperClass(july10_crop, nClasses=3)
july25_c3 <- unsuperClass(july25_crop, nClasses=3)

cl <- colorRampPalette(c('red','green', "blue"))(100)


par(mfrow=c(2,1))
plot(july10_c3$map, col=cl)
plot(july25_c3$map, col=cl)

### L'area della foto croppata è di 925380 celle di risoluzone 23,2 metri quindi:
### area cella= 23,2*23,2= 538.24
### AREA = 538.24*925380 = 498076531 m2 = 498 km2


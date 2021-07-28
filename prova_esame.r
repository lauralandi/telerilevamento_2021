PROVA ESAME

library(raster) #richiamo il pacchetto raster
library(RStoolbox) #richiamo il pacchetto RStoolbox
library(ggplot2) #richiamo il pacchetto ggplot2

setwd("C:/lab/esame/")

rlist12<-list.files(pattern="luglio12") #creo una lista di file e associo alla variabile rlist
import12<-lapply(rlist12,raster) #applico la funzione raster su tutti i file della lista e associo alla variabile import (in questo modo li importo tutti insieme)
luglio12<-stack(import12) # creo un univo file che contiene tutti quelli della lista importata

rlist18<-list.files(pattern="luglio18") #creo una lista di file e associo alla variabile rlist
import18<-lapply(rlist18,raster) #applico la funzione raster su tutti i file della lista e associo alla variabile import (in questo modo li importo tutti insieme)
luglio18<-stack(import18) # creo un univo file che contiene tutti quelli della lista importata

rlistsent2<-list.files(pattern="Sentinel-1") #creo una lista di file e associo alla variabile rlist
importsent2<-lapply(rlistsent2,raster) #applico la funzione raster su tutti i file della lista e associo alla variabile import (in questo modo li importo tutti insieme)
sent2<-stack(importsent2) # creo un univo file che contiene tutti quelli della lista importata

#pdf("prova1")
par(mfrow=c(2,1))
plotRGB(luglio12, 4, 5, 2, stretch="lin")
plotRGB(luglio18, 4, 5, 2, stretch="lin")
#dev.off()

pdf("prova2")
par(mfrow=c(2,1))
plotRGB(luglio12, 4, 5, 2, stretch="hist")
plotRGB(luglio18, 4, 5, 2, stretch="hist")
dev.off()

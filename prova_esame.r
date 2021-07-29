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

rlistS2<-list.files(pattern="S2") #creo una lista di file e associo alla variabile rlist
importS2<-lapply(rlistS2,raster) #applico la funzione raster su tutti i file della lista e associo alla variabile import (in questo modo li importo tutti insieme)
S2<-stack(importS2) # creo un univo file che contiene tutti quelli della lista importata

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
  
raster(x, xmn=0, xmx=1, ymn=0, ymx=1, crs="", template=NULL)

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


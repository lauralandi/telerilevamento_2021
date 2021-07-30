PROVA ESAME

library(raster) #richiamo il pacchetto raster
library(RStoolbox) #richiamo il pacchetto RStoolbox
library(ggplot2) #richiamo il pacchetto ggplot2
library(gridExtra)
library(rgdal)

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

library(raster) #richiamo il pacchetto raster
library(RStoolbox) #richiamo il pacchetto RStoolbox
library(ggplot2) #richiamo il pacchetto ggplot2
library(gridExtra)
library(rgdal)

setwd("C:/lab/esame_sardegna/")

#############################à

## Le immagini utilizzate sono da Sentinel-2, le cui bande sono:
##     BANDA                  LUNGHEZZA D'ONDA (micrometri)      RISOLUZIONE SPAZIALE (metri)
## B01= coastal aerosol                 0.443                           60
## B02= blue                            0.490                           10
## B03= green                           0.560                           10
## B04= red                             0.665                           10
## B05= vegetation red edge             0.705                           20
## B06= vegetation red edge             0.740                           20
## B07= vegetation red edge             0.783                           20
## B08= NIR                             0.842                           10
## B08A= vegetation red edge            0.865                           20
## B09= water vapour                    0.945                           60
## B11= SWIR                            1.610                           20
## B12= SWIR                            2.190                           20

#############################
# PRIMO STEP: CARICO LE FOTO DELLE DIVERSE BANDE E LE UNISCO IN UN RASTER PACK PER JULY10 E UNO PER JULY25

rlist10<-list.files(pattern="july10") #creo una lista di file e associo alla variabile rlist
import10<-lapply(rlist10,raster) #applico la funzione raster su tutti i file della lista e associo alla variabile import (in questo modo li importo tutti insieme)
july10<-stack(import10) # creo un unico file che contiene tutti quelli della lista importata

rlist25<-list.files(pattern="july25") #creo una lista di file e associo alla variabile rlist
import25<-lapply(rlist25,raster) #applico la funzione raster su tutti i file della lista e associo alla variabile import (in questo modo li importo tutti insieme)
july25<-stack(import25) # creo un unico file che contiene tutti quelli della lista importata

########################
# SECONDO STEP OSSERVIAMO LE IMMAGINI


#### PRIMO PLOT ENTRAMBI IN RGB (Sì)

p1<-ggRGB(july10, 4, 3, 2, stretch="lin", quantiles = c(0.0001, 0.9999)) # modifico i quantili a tentativi per regolare lo stretch della foto e renderlo più simile e confrontabile 
                                                                          # con la seconda
p2<-ggRGB(july25, 4, 3, 2, stretch="lin")
grid.arrange(p1, p2, nrow = 2)

#### SECONDO PLOT ENTRAMBE IN FALSI COLORI CON TERMICO SU BANDA ROSSA (Sì)

p4<-ggRGB(july10, 11, 10, 4, stretch="lin", quantiles = c(0.0001, 0.9999))
p5<-ggRGB(july25, 11, 10, 4, stretch="lin")
grid.arrange(p4, p5, nrow = 2)

##################################
# TERZO STEP CROPPO SULL'AREA DEGLI INCENDI PER ANALISI

plotRGB(july25, 11, 10, 4, stretch="lin")
e <- drawExtent(show=TRUE, col="red") # funzione drawExtent per disegnare un riquadro sul plot e generare un oggetto extent da usare dopo nel crop
e
# class      : Extent 
# xmin       : 943072.7 
# xmax       : 965034.5 
# ymin       : 4882167 
# ymax       : 4905410

july25_crop<- crop(july25, e)
july10_crop<- crop(july10, e)

#### PLOT DEI DUE CROP IN FALSI COLORI (Sì)

p6<-ggRGB(july10_crop, 11, 10, 4, stretch="lin", quantiles = c(0.0001, 0.9999))
p7<-ggRGB(july25_crop, 11, 10, 4, stretch="lin")
grid.arrange(p6, p7, ncol = 2)

#####################################
# QUARTO STEP, CALCOLO NDVI E NBR DEI DUE GIORNI

## NDVI= (NIR-RED)/(NIR+RED)
## NBR=  (NIR-SWIR)/(NIR+SWIR)

# NIR=B08
# RED=B04
# SWIR=B12

## per l'NBR serve SWIR co lamba tra 2080 e 2350, quindi per sentinel è B12

###########
#NDVI

NDVI_july10<-(july10_crop$july10_B08-july10_crop$july10_B04)/(july10_crop$july10_B08+july10_crop$july10_B04)
NDVI_july25<-(july25_crop$july25_B08-july25_crop$july25_B04)/(july25_crop$july25_B08+july25_crop$july25_B04)

#### PLOT DEI DUE NDVI
cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100)
par(mfrow=c(1,2))
plot(NDVI_july10, col=cl, main="NDVI 10 Luglio")
plot(NDVI_july25, col=cl, main="NDVI 25 Luglio")

#### CALCOLO LA DIFFERENZA PER STIMARE IL CALO DI NDVI DOPO L'INCENDIO
deltaNDVI<- NDVI_july10 - NDVI_july25

### PLOT DEL DELTA NDVI, IN ROSSO IL CALO MAGGIORE, IN BLU VICINO A ZERO
cld <- colorRampPalette(c('blue','white','red'))(100) 
plot(deltaNDVI, col=cld, main="differenza NDVI") ## <-- controllare come forzare a zero il limite della legenda


###########
#NBR

NBR_july10<-(july10_crop$july10_B12-july10_crop$july10_B08)/(july10_crop$july10_B08+july10_crop$july10_B12)
NBR_july25<-(july25_crop$july25_B12-july25_crop$july25_B08)/(july25_crop$july25_B08+july25_crop$july25_B12)

#### PLOT DEI DUE NBR
cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100)
par(mfrow=c(1,2))
plot(NBR_july10, col=cl, main="NBR 10 Luglio")
plot(NBR_july25, col=cl, main="NBR 25 Luglio")

#### CALCOLO LA DIFFERENZA PER STIMARE L'AUMENTO DI NBR DOPO L'INCENDIO
deltaNBR<- NBR_july25 - NBR_july10

### PLOT DEL DELTA NBR, IN ROSSO L'AUMENTO MAGGIORE, IN BLU VICINO A ZERO
cld <- colorRampPalette(c('blue','white','red'))(100) 
plot(deltaNBR, col=cld, main="differenza NBR") ## <-- controllare come forzare a zero il limite della legenda

###############
#CLASSIFICARE I VALORI DI NBR !!!
# FARE GRAFICO E SE TROVO COME FARE PLOT












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


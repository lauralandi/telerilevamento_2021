PROVA ESAME

library(raster) #richiamo il pacchetto raster
library(RStoolbox) #richiamo il pacchetto RStoolbox
library(ggplot2) #richiamo il pacchetto ggplot2
library(gridExtra)
library(rgdal)

setwd("C:/lab/esame/")


#####################################################################################
#   e<-extent(943072.7, 965034.5, 4882167, 4905410)  # CANCELLARE PRIMA DI ESAME   #
#####################################################################################


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

##############################################################################################################################################################

##############à PROVE PER SARDEGNA ##############

H2<-hist(deltaNBR,
     main = "Distribution of raster cell values in the NBR difference data",
     xlab = "deltaNBR", ylab = "Number of Pixels",
     breaks = 4,
     col = "springgreen")

H3<-hist(deltaNBR,
     main = "Distribution of raster cell values in the NBR difference data",
     xlab = "deltaNBR", ylab = "Number of Pixels",
     breaks = c(-1.55, -1, -0.5, 0, 0.75),
     col = "springgreen")

# H3$breaks
# [1] -1.55 -1.00 -0.50  0.00  0.75

# H3$counts
# [1]   1068  89236 580991 276597

### -1.55 - -1 -> danno maggiore 3
### -1 - -0.5 -> danno intermedio 2
### -0.5 - 0 -> danno minore 1
###  0 - 0.75 -> no danno NA

# creo un oggetto con gli elementi che descrivono la classificazione
reclass<- c( -1.55,-1,3,
            -1,-0.5,2,
            -0.5,0,1,
            0,0.75, NA)

#trasformo l'oggetto reclass in una matrice con righe e colonne
reclass_m<-matrix(reclass,
                ncol = 3,
                byrow = TRUE)

#utilizzando la funzione reclassify e la matrice di classificaizone realizzata classifico il raster deltaNBR
deltaNBR_classified <- reclassify(deltaNBR,
                     reclass_m)

barplot(deltaNBR_classified)

clc<- c("red", "orange", "yellow")
plot(deltaNBR_classified, col=clc)
            

###############
#CLASSIFICARE I VALORI DI deltaNBR !!!
# FARE GRAFICO E SE TROVO COME FARE PLOT

## Analizzo il raster
summary(deltaNBR)
#             layer
# Min.    -1.5155392
# 1st Qu. -0.2412478
# Median  -0.1062936
# 3rd Qu.  0.0200322
# Max.     0.7329894
# NA's     0.0000000

#H1<-hist(deltaNBR,
 #    main = "Distribution of raster cell values in the NBR difference data",
  #   xlab = "deltaNBR", ylab = "Number of Pixels",
   #  col = "springgreen")

#dai dati di distribuzione di frequenza creo una classificazione con classi di ampiezza pari alla deviazione standard
### usare la funzione focal (da pacchetto raster) per analisi statistica utilizzando la moving window
# con w definisco le dimensioni della moving window, con fun la variabile statistica da calcolare

cld<-wes_palette("Darjeeling1", 100, type = c("continuous"))
clM<-wes_palette("Moonrise3", 100, type = c("continuous"))
clr<-wes_palette("Royal1", 100, type = c("continuous"))

#SD3_dNBR<-focal(deltaNBR, w=matrix(1/9,nrow=3,ncol=3), fun=sd)
#plot(SD3_dNBR, col=cld)

#SD5_dNBR<-focal(deltaNBR, w=matrix(1/25,nrow=5,ncol=5), fun=sd)
#plot(SD5_dNBR, col=cld)

#SD7_dNBR<-focal(deltaNBR, w=matrix(1/49,nrow=7,ncol=7), fun=sd)
#plot(SD7_dNBR, col=clr)

clA<-colorRampPalette(c('blue','green','yellow'))(100)
set.seed(42)
dNBR_c3<-unsuperClass(deltaNBR, nClasses=3)
plot(dNBR_c3$map, col=clA)





### L'area della foto croppata è di 925380 celle di risoluzone 23,2 metri quindi:
### area cella= 23,2*23,2= 538.24
### AREA = 538.24*925380 = 498076531 m2 = 498 km2




###########################################################################################################################################################################
###########################################################################################################################################################################
###################################################################  PROVA SARDEGNA UFFICIALE  #############################################################################
###########################################################################################################################################################################
###########################################################################################################################################################################


###################################################################################################
#   STEP 1  -  Richiamare tutte le library necessarie al codice e definire la working directiory  #
###################################################################################################

library(raster) #richiamo il pacchetto raster
library(RStoolbox) #richiamo il pacchetto RStoolbox
library(ggplot2) #richiamo il pacchetto ggplot2
library(gridExtra)
library(rgdal)
library(wesanderson)

setwd("C:/lab/esame_sardegna/")

###################################################################
#   STEP 2  -  Scegliere le immagini su cui effettuare l'analisi  #
###################################################################

## Le due immagini utilizzate derivano da Sentinel-2 e rappresentano la zona del Montiferru in provincia di 
## Oristano il 10 e il 25 Luglio 2021, ovvero prima e dopo gli incendi che hanno colpito l'area in quel mese.

## Le diverse bande in cui il satellite Sentinel-2 fornisce le immagini sono:

##       BANDA                  LUNGHEZZA D'ONDA (micrometri)      RISOLUZIONE SPAZIALE (metri)
##   B01= coastal aerosol                 0.443                           60
##   B02= blue                            0.490                           10
##   B03= green                           0.560                           10
##   B04= red                             0.665                           10
##   B05= vegetation red edge             0.705                           20
##   B06= vegetation red edge             0.740                           20
##   B07= vegetation red edge             0.783                           20
##   B08= NIR                             0.842                           10
##   B08A= vegetation red edge            0.865                           20
##   B09= water vapour                    0.945                           60
##   B11= SWIR                            1.610                           20
##   B12= SWIR                            2.190                           20


## Nello specifico per questa analisi sono state sfruttate le bande
##   B02= blue 
##   B03= green
##   B04= red  
##   B08= NIR
##   B11= SWIR
##   B12= SWIR


# Le diverse bande sono state scaricate separatamente, quindi per caricarle creo due liste di file con pattern "july10" e "july25" rispettivamente per
# la prima e la seconda immagine. Dopodichè la funzione lapply mi permette di applicare all'intera lista la funzione raster. Infine con la funzione stack unisco
# tutti i raster in un unico RasterStack, che conterrà tutte le bande (il procedimento è ripetuto per entrambe le immagini)

rlist10<-list.files(pattern="july10") #creo una lista di file delle diverse bande con pattern "july10" e associo alla variabile rlist10
import10<-lapply(rlist10,raster) #applico la funzione raster su tutti i file della lista e associo alla variabile import10
july10<-stack(import10) # creo un unico oggetto RasterStack che contiene come layer tutte le bande importate

rlist25<-list.files(pattern="july25") #creo una lista di file delle diverse bande con pattern "july25" e associo alla variabile rlist25
import25<-lapply(rlist25,raster) #applico la funzione raster su tutti i file della lista e associo alla variabile import25
july25<-stack(import25) # creo un unico oggetto RasterStack che contiene come layer tutte le bande importate

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

#### PLOT IN FALSI COLORI CON NIR IN BANDA ROSSA

p6<-ggRGB(july10, 8, 3, 2, stretch="lin", quantiles = c(0.0001, 0.9999))
p7<-ggRGB(july25, 8, 3, 2, stretch="lin")
grid.arrange(p6, p7, nrow = 2)

##################################
# TERZO STEP CROPPO SULL'AREA DEGLI INCENDI PER ANALISI

plotRGB(july25, 11, 10, 4, stretch="lin")
# e <- drawExtent(show=TRUE, col="red") # funzione drawExtent per disegnare un riquadro sul plot e generare un oggetto extent da usare dopo nel crop
# e
# class      : Extent 
# xmin       : 943072.7 
# xmax       : 965034.5 
# ymin       : 4882167 
# ymax       : 4905410
e<-extent(943072.7, 965034.5, 4882167, 4905410)

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
clz<-wes_palette("Zissou1", 100, type = c("continuous"))
#cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100)
par(mfrow=c(1,2))
plot(NDVI_july10, col=clz, main="NDVI 10 Luglio")
plot(NDVI_july25, col=clz, main="NDVI 25 Luglio")

#### CALCOLO LA DIFFERENZA PER STIMARE IL CALO DI NDVI DOPO L'INCENDIO
deltaNDVI<- NDVI_july10 - NDVI_july25

### PLOT DEL DELTA NDVI, IN ROSSO IL CALO MAGGIORE, IN BLU VICINO A ZERO
clm<-wes_palette("Moonrise1", 100, type = c("continuous"))
cld <- colorRampPalette(c('blue','white','red'))(100) 
plot(deltaNDVI, col=cld, main="differenza NDVI") ## <-- controllare come forzare a zero il limite della legenda


###########
#NBR

NBR_july10<-(july10_crop$july10_B08-july10_crop$july10_B12)/(july10_crop$july10_B08+july10_crop$july10_B12)
NBR_july25<-(july25_crop$july25_B08-july25_crop$july25_B12)/(july25_crop$july25_B08+july25_crop$july25_B12)

#### PLOT DEI DUE NBR
par(mfrow=c(1,2))
plot(NBR_july10, col=clz, main="NBR 10 Luglio")
plot(NBR_july25, col=clz, main="NBR 25 Luglio")

#### CALCOLO LA DIFFERENZA PER STIMARE L'AUMENTO DI NBR DOPO L'INCENDIO
deltaNBR<- NBR_july25 - NBR_july10

### PLOT DEL DELTA NBR, IN ROSSO L'AUMENTO MAGGIORE, IN BLU VICINO A ZERO
cld2 <- colorRampPalette(c('red','white','blue'))(100)
plot(deltaNBR, col=cld2, main="differenza NBR") ## <-- controllare come forzare a zero il limite della legenda


#################
# PLOT DEI DUE DELTA
par(mfrow=c(1,2))
plot(deltaNDVI, col=cld, main="differenza NDVI")
plot(deltaNBR, col=cld2, main="differenza NBR")


#############
# CLASSIFICAZIONE DEL DELTANBR IN 4 CLASSI (DUE NO DANNI, ALTRE DUE DANNI PIù E MENO GRAVI)
clB<-colorRampPalette(c('blue','green', 'red','yellow'))(100)

set.seed(42)
dNBR_c4<-unsuperClass(deltaNBR, nClasses=4)
plot(dNBR_c4$map, col=clB)

set.seed(42)
dNBR_c3<-unsuperClass(deltaNBR, nClasses=3)
plot(dNBR_c3$map, col=clB)

clD<-colorRampPalette(c('blue','red'))(100)
set.seed(42)
dNBR_c2<-unsuperClass(deltaNBR, nClasses=2)
plot(dNBR_c2$map, col=clD)

colors <- colorRampPalette(c('red','green','yellow','darkgreen'))(100) 
colors2 <-colorRampPalette(c('darkgreen','red','yellow'))(100)

HCLASS<-hist(dNBR_c4$map,
     main = "Distribution of raster cell values in the NBR difference data",
     breaks= c(0,1, 2, 3, 4),
     xlab = "classi", ylab = "Number of Pixels",
     col = colors)

dNBR_c4$model

##### PLOT CLASSIFICAZIONE A 4 CLASSI
plot(dNBR_c4$map, col = colors, legend = FALSE, axes = FALSE, box = FALSE)
legend(965031,4905419, legend = paste0("C",1:4), fill = colors,
      title = "Classi", horiz = FALSE,  bty = "n")
                           

## Estensione raster:
#xmin 943077.6
#xmax 965030.7
#ymin 4882162
#ymax 4905419

## Posizionamento legenda:
#x 965031
#y 4905419

####################
## PLOT DI CONFRONTO DELTANBR E CLASSIFICAZIONE

par(mfrow=c(1,2))
plot(deltaNBR, col=cld2, main="differenza NBR")
plot(dNBR_c4$map, col = colors, legend = FALSE, axes = FALSE, box = FALSE, main="Classificazione deltaNBR")
legend(965031,4905419, legend = paste0("C",1:4), fill = colors,
      title = "Classi", horiz = FALSE,  bty = "n")

####################
## PLOT DI CONFRONTO FALSI COLORI E CLASSIFICAZIONE

par(mfrow=c(1,2))
plotRGB(july25_crop, 11, 10, 4, stretch="lin")
plot(dNBR_c4$map, col = colors, legend = FALSE, axes = FALSE, box = FALSE, main="Classificazione deltaNBR")
legend(965031,4905419, legend = paste0("C",1:4), fill = colors,
      title = "Classi", horiz = FALSE,  bty = "n")

#################
## CALCOLARE PERCENTUALE DELLE CLASSI

freq(dNBR_c4$map)
#      value   count
#  [1,]   1   210375
#  [2,]   2   359710
#  [3,]   3    99897
#  [4,]   4   277910

somma<- 210375 + 359710 + 99897 + 277910
perc<-freq(dNBR_c4$map)/somma
#            value        count
# [1,]    1.054973e-06   0.2219398
# [2,]    2.109945e-06   0.3794842
# [3,]    3.164918e-06   0.1053886
# [4,]    4.219890e-06   0.2931874

# C1: 22%
# C2: 38%
# C3: 11%
# C4: 29%

#######################
## GENERARE UN DATASET (DATAFRAME)

# La risoluzione è 
# x: 23.20628 m
# y: 23.21091 m
# ncell: 947892

# L'area di un pixel quindi è 
# 23.20628 * 23.21091 = 538.6389 m2
# L'area totale è
# 538.6389 * somma = 510571504 m2 = 510.571504 km2

Classi<-c("C1","C2", "C3", "C4")
Danno<- c( "Intermedio", "Nullo", "Alto", "Nullo")
Area_percentuale<-c(0.22, 0.38, 0.11, 0.29)
Area_km2<-Area_percentuale*510.571504

percent<-data.frame(Classi, Danno, Area_percentuale, Area_km2)
percent

#PLOT DEL GRAFICO AREA DANNO DA DATAFRAME
g5<-ggplot(percent, aes(x=Danno,y=Area_km2,color=Classi)) + geom_bar(stat="identity", width=0.2, (aes(fill = Classi)))

##############################################################################################################################################################################


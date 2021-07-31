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

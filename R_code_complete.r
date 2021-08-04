# R code complete
# TELERILEVAMENTO GEO-ECOLOGICO 2021

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#---------  SUMMARY: -----------#

# 1. Remote sensing - First Code
# 2. R Code - Time series
# 3. R Code - Copernicus
# 4. R Code - Knitr
# 5. R Code - Multivariate analysis
# 6. R Code - Classification
# 7. R Code - ggplot2
# 8. R Code - Vegetation Indices
# 9. R Code - Land Cover
# 10. R Code - Variability
# 11. R Code - Spectral Signatures

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1. Remote sensing - First Code

# Primo codice in R per telerilevamento

# install.packages("raster") # con installl.packages installo i pacchetti che mi interessano (è sufficiente farlo una volta)
# install.packages("RStoolbox")
# install.packages("ggplot2")
library(raster) #richiamo il pacchetto raster  # una volta installati i pacchetti uso library per richiamare quelli che servono nel codice e poterli utilizzare
library(RStoolbox) #richiamo il pacchetto RStoolbox
library(ggplot2) #richiamo il pacchetto ggplot2

setwd("C:/lab/") #imposto la working directory

### BANDE LANDSAT:
#B1= blu
#B2= verde
#B3= rosso
#B4= near infrared
#B5= middle infrared
#B6= infrarosso termico
#B7= infrarosso medio (altro sensore)

p224r63_2011 <- brick("p224r63_2011_masked.grd") # con la funzione brick estraggo i dati raster e li associo a una variabile
p224r63_2011 # leggo le informazioni raster
plot(p224r63_2011) # plotto l'immagine con la scala colori di default

cl1<-colorRampPalette(c("blue","green","orange","yellow")) (100) # definisco una nuova scala colore con la funzione colorRampPalette
plot(p224r63_2011,col=cl1) # plotto l'immagine con la nuova scala colore

dev.off() # pulisco la finestra grafica
plot(p224r63_2011$B1_sre, col=cl1) # plotto una sola banda a scelta usando $ per definire il layer specifico dell'oggetto RasterBrick che mi interessa

# con la funzione par definisco la visualizzazione di più grafici insieme in righe e colonne a mia scelta:
par(mfrow=c(2,1)) # 2 righe e 1 colonna
plot(p224r63_2011$B1_sre,col=cl1)
plot(p224r63_2011$B2_sre,col=cl1)

par(mfrow=c(4,1)) # 4 righe e 1 colonna
plot(p224r63_2011$B1_sre,col=cl1)
plot(p224r63_2011$B2_sre,col=cl1)
plot(p224r63_2011$B3_sre,col=cl1)
plot(p224r63_2011$B4_sre,col=cl1)

par(mfrow=c(2,2)) # 2 righe e 2 colonne
plot(p224r63_2011$B1_sre,col=cl1)
plot(p224r63_2011$B2_sre,col=cl1)
plot(p224r63_2011$B3_sre,col=cl1)
plot(p224r63_2011$B4_sre,col=cl1)

# associo una scala colore diversa ad ogni banda e le plotto in un quadrato 2x2
par(mfrow=c(2,2))
clb<- colorRampPalette(c("darkblue", "blue", "lightblue")) (100)
clv<- colorRampPalette(c("darkgreen", "green", "lightgreen")) (100)
clr<- colorRampPalette(c("darkred", "red", "orange")) (100)
clnir<- colorRampPalette(c("pink", "salmon","magenta")) (100)
plot(p224r63_2011$B1_sre,col=clb)
plot(p224r63_2011$B2_sre,col=clv)
plot(p224r63_2011$B3_sre,col=clr)
plot(p224r63_2011$B4_sre,col=clnir)


### PLOT IN RGB
# con la funzione plotRGB posso montare sui canali RGB le bande in diverse combinazioni per ottenere visualizzazioni più efficaci delgi elementi che mi interessano
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin") # immagine a colori naturali, R=RED G=GREEN B=BLUE
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") # immagine in falsi colori R=NIR G=RED B=GREEN
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin") # immagine in falsi colori R=RED G=NIR B=GREEN
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin") # immagine in falsi colori R=RED G=GREEN B=NIR
# la funzione stretch modifica la distribuzione dei valori di riflettanza tra il minimo e il massimo per migliorare il contrasto e la visibilità dell'immagine

plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="hist") # le due funzioni stretch di default sono linear e histogram


pdf("pdf1") # salvo il risultato come pdf nella working directory
par(mfrow=c(2,2)) # plotto le immagini in un grafico 2x2
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin") # immagine a colori naturali, R=RED G=GREEN B=BLUE
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") # immagine in falsi colori R=NIR G=RED B=GREEN
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin") # immagine in falsi colori R=RED G=NIR B=GREEN
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin") # immagine in falsi colori R=RED G=GREEN B=NIR
dev.off() # pulisco la finestra grafica dopo il procedimento per salvare correttamente il pdf

#confronto le immagin a colori naturali e in falsi colori (nir in verde) con diversi stretch (lin e his)
par(mfrow=c(3,1))
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin") # immagine a colori naturali, R=RED G=GREEN B=BLUE con stretch lin
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin") # immagine in falsi colori R=RED G=NIR B=GREEN con stretch lin
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="hist") # immagine in falsi colori R=RED G=NIR B=GREEN con stretch his


### OSSERVARE CAMBIAMENTI NEL TEMPO DELLA STESSA ZONA

p224r63_1988 <- brick("p224r63_1988_masked.grd") # con la funzione brick importol'immagine del 1988 come oggetto RasterBrick
plot(p224r63_1988) # plotto tutte le bande separate
plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin") # plotto l'immagine a colori naturali

# multitemporal set con stretch lineare: plotto su due righe l'immagine del 1988 e del 2011 per confrontarle
par(mfrow=c(2,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin") # immagine 1988
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") # immagine 2011

# multitemporal set con stretch histogram: plotto su due righe l'immagine del 1988 e del 2011 per confrontarle
par(mfrow=c(2,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="hist") #immagine 1988
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="hist") #immagine 2011

#pdf con set 4x4 delle immagini 1988 e 2011 in stretch lineare e hist
pdf("multitemporal_set_1988_2011_lin_hist") # imposto la creazione del pdf
par(mfrow=c(2,2)) # imposto righe e colonne 2x2 dove plottare le immagini
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin") # plotto immagine 1988
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") # plotto immagine 2011
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="hist") #plotto immagine 1988
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="hist") #plotto immagine 2011
dev.off() # pulisco la finestra grafica dopo il procedimento per salvare correttamente il pdf

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 2. R Code - Time series

### TIME SERIES ANALYSIS
### GREENLAND INCREASE OF TEMPERATURE
### Data and code from Emanuela Cosma

# install.packages("raster")
library(raster)
# install.packages("RStoolbox")
library(RStoolbox)
# install.packages("rasterVis")
library(rasterVis)
# install.packages("knitr")
library(knitr)

setwd("C:/lab/greenland/")
lst_2000<-raster("lst_2000.tif") #importo un layer raster
lst_2005<-raster("lst_2005.tif")
lst_2010<-raster("lst_2010.tif")
lst_2015<-raster("lst_2015.tif")

#creare multipanel con le 4 immagini
par(mfrow=c(2,2))
plot(lst_2000)
plot(lst_2005)
plot(lst_2010)
plot(lst_2015)

rlist<-list.files(pattern="lst") #creo una lista di file e associo alla variabile rlist
import<-lapply(rlist,raster) #applico la funzione raster su tutti i file della lista e associo alla variabile import (in questo modo li importo tutti insieme)
TGr<-stack(import) # creo un univo file che contiene tutti quelli della lista importata

plotRGB(TGr, 1, 2, 3, stretch="lin")

levelplot(TGr) #plot dell'immagine
levelplot(TGr) #plot del singolo layer dell'immagine con grafici della medie colonne e righe
cl <- colorRampPalette(c("blue","light blue","pink","red"))(100)
levelplot(TGr,col.regions=cl,main="Summer land surface temperature",names.attr=c("July 2000","July 2005", "July 2010", "July 2015"))

### Melt Data 
melt_list<-list.files(pattern="melt")
melt_import<-lapply(melt_list,raster)
melt_stack<-stack(melt_import)
levelplot(melt_stack,col.regions=cl,main="Annual Melt Data",names.attr=c("1979","1980", "1981", "1982", "1983", "1984", "1985", "1986", 
                                                                          "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", 
                                                                          "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", 
                                                                          "2003", "2004", "2005", "2007"))

#sottrazione tra matrici 2007-1979
melt_amount<- melt_stack$X2007annual_melt - melt_stack$X1979annual_melt
cl2<-colorRampPalette(c("blue","white","red"))(100)
levelplot(melt_amount,col.regions=cl2)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 3. R Code - Copernicus

### VISUALIZZAZIONE DATI COPERNICUS

#install.packages("ncdf4")
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

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 4. R Code - Knitr

### REPORT IN KNITR

setwd("C:/lab/")
library(knitr)
stitch("R_code_greenland.r", template=system.file("misc", "knitr-template.Rnw", package="knitr"))

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 5. R Code - Multivariate analysis

### ANALISI MULTIVARIATA

library(raster)
library(RStoolbox) #richiamo il pacchetto RStoolbox

setwd("C:/lab/")

p224r63_2011 <- brick("p224r63_2011_masked.grd")

plot(p224r63_2011)

plot(p224r63_2011$B1_sre,p224r63_2011$B2_sre,col="red", pch=19, cex=2)
plot(p224r63_2011$B2_sre,p224r63_2011$B1_sre,col="red", pch=19, cex=2)

pairs(p224r63_2011)

### resampling (ricampionamento)
p224r63_2011res <- aggregate(p224r63_2011, fact=10)  #riduco il numero di pixel (la risoluzione) con la funzione aggregate

par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch='lin')
plotRGB(p224r63_2011res, r=4, g=3, b=2, stretch='lin')

p224r63_2011res_PCA<-rasterPCA(p224r63_2011res) #contiene al suo interno mappa e informazioni sul modello
summary(p224r63_2011res_PCA$model) #fa un sommario delle info del modello generato dalla funzione rasterPCA
plot(p224r63_2011res_PCA$map)
de.off()
plotRGB(p224r63_2011res_PCA$map, r=1, g=2, b=3, stretch='lin')
str(p224r63_2011res_PCA) #struttura dell'oggetto

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 6. R Code - Classification
  
### CLASSIFICAZIONE IMMAGINI 
### DATI SOLAR ORBITER

library(raster)

library(RStoolbox) #richiamo il pacchetto RStoolbox
library(ggplot2) #richiamo il pacchetto ggplot2


setwd("C:/lab/")

so <- brick("Solar_Orbiter_s_first_views_of_the_Sun_pillars.jpg")
so #vedo le info della immagine
plot(so) #visualizzo le tre bande separate
plotRGB(so, 1,2,3, stretch="lin") #visualizzo le tre bande montate in RGB

set.seed(42) # fa in modo che il risultato sia sempre lo stesso nei diversi run utilizzando sempre lo stesso set di pixel che altrimenti sarebbe sempre randomica
so_c3 <- unsuperClass(so, nClasses=3) #classificazone non supervisionata che produce una mappa e il model con le info
so_c20 <- unsuperClass(so, nClasses=20) #classificazione con 20 classi
plot(so_c3$map) # plotto la mappa prodotta dalla funzione unsuperClass
plot(so_c20$map)

#carico una seconda immagine
so2 <- brick("Solar_Orbiter_spots_campfires_on_the_Sun_annotated.jpg")
plotRGB(so2, 1, 2, 3, stretch="lin")

so2_c3 <- unsuperClass(so2, nClasses=3) 
plot(so2_c3$map)
so2_c20 <- unsuperClass(so2, nClasses=20)
plot(so2_c20$map)

### PROVA CON FOTO
fiume<-brick("photo6034998005653942920.jpg")
fiume_c3 <- unsuperClass(fiume, nClasses=3) 
plot(fiume_c3$map)
fiume_c20 <- unsuperClass(fiume, nClasses=20)
plot(fiume_c20$map)


### DATI GRAND CANYON

gc<-brick("dolansprings_oli_2013088_canyon_lrg.jpg")
plotRGB(gc,r=1,g=2,b=3,stretch="lin")

gc_c2 <- unsuperClass(gc, nClasses=2)
cl <- colorRampPalette(c('blue','yellow'))(100)
plot(gc_c2$map, col=cl)

gc_c4 <- unsuperClass(gc, nClasses=4)
cl2 <- colorRampPalette(c('blue','green', 'orange','yellow'))(100)
cl3 <- colorRampPalette(c('blue','green', 'pink','orange'))(100)
plot(gc_c4$map, col=cl2)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 7. R Code - ggplot2

##### GGPLOT2

library(raster)
library(RStoolbox)
library(ggplot2)
library(gridExtra)

setwd("~/lab/")

p224r63 <- brick("p224r63_2011_masked.grd")

ggRGB(p224r63,3,2,1, stretch="lin")
ggRGB(p224r63,4,3,2, stretch="lin")

p1 <- ggRGB(p224r63,3,2,1, stretch="lin")
p2 <- ggRGB(p224r63,4,3,2, stretch="lin")

grid.arrange(p1, p2, nrow = 2) # this needs gridExtra

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 8. R Code - Vegetation Indices

### INDICI DI VEGETAZIONE

library(raster)
library(RStoolbox)
library(rasterdiv) # wordlwide NDVI
library(rasterVis)

setwd("C:/lab/")

defor1<-brick("defor1.jpg")
defor2<-brick("defor2.jpg")

### B1=NIR B2=RED B3=GREEN

par(mfrow=c(2,1))
plotRGB(defor1, r=1, g=2, b=3, stretch='lin')
plotRGB(defor2, r=1, g=2, b=3, stretch='lin')

#le bande dentro defor1 si chiamano defor1.1, defor 1.2, defor1.3
cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100) # specifying a color scheme
DVI1<-defor1$defor1.1 - defor1$defor1.2 #calcolo DVI sottraendo B1-B2 (NIR-RED)
plot(DVI1, col=cl, main="DVI at time 1")

DVI2<-defor2$defor2.1 - defor2$defor2.2 #calcolo DVI sottraendo B1-B2 (NIR-RED)
plot(DVI2, col=cl, main="DVI at time 2")

par(mfrow=c(2,1))
plot(DVI1, col=cl, main="DVI at time 1")
plot(DVI2, col=cl, main="DVI at time 2")

#sottraiamo il secondo DVI al primo per capire di quanto è calato nel tempo
deltaDVI<- DVI1 - DVI2
cld <- colorRampPalette(c('blue','white','red'))(100) 
plot(deltaDVI, col=cld, main="differenza DVI")

## calcolo del NDVI
## NDVI= (NIR-RED)/(NIR+RED)

NDVI1<-(defor1$defor1.1-defor1$defor1.2)/(defor1$defor1.1+defor1$defor1.2)
NDVI2<-(defor2$defor2.1-defor2$defor2.2)/(defor2$defor2.1+defor2$defor2.2)
par(mfrow=c(2,1))
plot(NDVI1, col=cl, main="NDVI at time 1")
plot(NDVI2, col=cl, main="NDVI at time 2")

deltaNDVI<- NDVI1 - NDVI2
plot(deltaNDVI, col=cld, main="differenza DVI")


## RStoolbox:: spectralIndices

vi1<-spectralIndices(defor1, green=3, red=2, nir=1)
plot(vi1, col=cl)

vi2 <- spectralIndices(defor2, green = 3, red = 2, nir = 1)
plot(vi2, col=cl)

### worldwide NDVI
plot(copNDVI) #deriva da pacchetto rasterdiv
copNDVI<-raster::reclassify(copNDVI, cbind(253:255,NA))
#cbind è funzione che trasforma in NA (non valori) pixel scelti, in questo caso l'acqua

levelplot(copNDVI)     

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 9. R Code - Land Cover

### LAND COVER

library(raster)
library(RStoolbox)
library(ggplot2) 
library(gridExtra)

setwd("C:/lab/")

defor1 <- brick("defor1.jpg") 
defor2 <- brick("defor2.jpg")

# defor1.1 = NIR
# defor1.2 = red
# defor1.3 = green

### GGPLOT2

par(mfrow=c(2,1))
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

## multiframe con ggplot con gridExtra
p1<-ggRGB(defor1, r=1, g=2, b=3, stretch="Lin") #ggRGB da ggplot2
p2<-ggRGB(defor2, r=1, g=2, b=3, stretch="Lin")
grid.arrange(p1,p2,nrow=2) #grid.arrange da gridExtra


##### CLASSIFICAZIONE

#set.seed() permette di ottenere sempre lo stesso risultato

d1c <- unsuperClass(defor1, nClasses=2)
cl <- colorRampPalette(c('black','green'))(100)
cl2 <- colorRampPalette(c('green','black'))(100)
par(mfrow=c(2,1))
plot(d1c$map, col=cl)
plot(d1c$map, col=cl2)

d2c<- unsuperClass(defor2, nClasses=2)
par(mfrow=c(2,1))
plot(d2c$map, col=cl)
plot(d2c$map, col=cl2)

d1c3 <- unsuperClass(defor1, nClasses=3)
d2c3 <- unsuperClass(defor2, nClasses=3)
par(mfrow=c(2,1))
plot(d1c3$map)
plot(d2c3$map)

####### CALCOLARE LE AREE DI FORESTA PERSE
#       (Calcolare la frequenza dei pixel di una certa classe)

freq(d1c$map)
#     value  count
# [1,]     1 305922
# [2,]     2  35370
somma1<- 305922+35370
prop1<-freq(d1c$map)/somma1 #proporzioni delle due classi nella immagine
#       value     count
# [1,] 2.930042e-06 0.8963644
# [2,] 5.860085e-06 0.1036356

# classe 1 = 89,6%
# classe 2 = 10,3%


freq(d2c$map)
#      value  count
# [1,]     1 178312
# [2,]     2 164414

somma2<-178312+164414
prop2<-freq(d2c$map)/somma2
#            value     count
# [1,] 2.917783e-06 0.5202757
# [2,] 5.835565e-06 0.4797243

# classe 1 = 52%
# classe 2 = 47,9%


########## GENERARE UN DATASET (DATAFRAME)
cover<-c("Forest","Agricolture")
percent_1992<-c(89.64,10.36)
percent_2006<-c(52.03,47.97)
percentages<-data.frame(cover,percent_1992,percent_2006)
percentages

## usiamo ggplot2 per creare un grafico

g1<-ggplot(percentages, aes(x=cover,y=percent_1992,color=cover)) + geom_bar(stat="identity", fill="white")   
g2<-ggplot(percentages, aes(x=cover,y=percent_2006,color=cover)) + geom_bar(stat="identity", fill="white") 
# aes indica l'estetica del grafico
# geom indica il tipo di geometria per il dato (esempio punto o linea)
# color indica a quali oggetti facciamo discriminare le classi
# stat="identity" indica che usiamo i dati del nostro dataset

grid.arrange(g1,g2,nrow=2)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 10. R Code - Variability

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

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 11. R Code - Spectral Signatures

## FIRMA SPETTRALE

library(raster)
library(rgdal)
library(ggplot2)

setwd("C:/lab/")

defor2<-brick("defor2.jpg")

# defor2.1 defor2.2 defor 2.3
# NIR       red      green

plotRGB(defor2, r=1, g=2, b=3, stretch="lin")

click(defor2, id=T, xy=T, cell=T, type= "p", pch=16, col="yellow")
    # xy TRUE perchè lavoriamo su coordinate spaziali
# type è tipo di click (p=puntuale)
# pch è il simbolo (cerca su google immagini)
# nell'usare la funzione click il plot deve rimanere aperto

#     x     y   cell defor2.1 defor2.2 defor2.3
# 1 345.5 223.5 182464      202        3        8
#      x     y   cell defor2.1 defor2.2 defor2.3
# 1 191.5 178.5 214575        0       54       63

## creo un dataframe
# prima definisco le colonne (in questo caso tre):
band<-c(1,2,3)
forest<-c(202,3,8)
water<-c(0,54,63)
spect_sign<-data.frame(band,forest,water)

# plottiamo un grafico con questi dati
ggplot(spect_sign, aes(x=band)) +
              geom_line(aes(y=forest), color="green") +
              geom_line(aes(y=water), color="blue") +
              labs(x="bande",y="riflettanza")

######
# ANALISI MULTITEMPORALE

defor1<-brick("defor1.jpg")

              
# spectral signature defor1
plotRGB(defor1, r=1, g=2, b=3, stretch="lin")
click(defor1, id=T, xy=T, cell=T, type= "p", pch=16, col="yellow")    

#  x     y   cell defor1.1 defor1.2 defor1.3
# 1 80.5 325.5 108609      218       12       32
#      x     y   cell defor1.1 defor1.2 defor1.3
# 1 104.5 306.5 122199      207       10       27
#      x     y   cell defor1.1 defor1.2 defor1.3
# 1 105.5 330.5 105064      217        6       25
#     x     y   cell defor1.1 defor1.2 defor1.3
# 1 50.5 320.5 112149      205       18       35
#     x     y  cell defor1.1 defor1.2 defor1.3
# 1 90.5 381.5 68635      215       23       38
#     x     y  cell defor1.1 defor1.2 defor1.3
# 1 60.5 402.5 53611      206       20       44


# spectral signature defor2
plotRGB(defor2, r=1, g=2, b=3, stretch="lin")
click(defor2, id=T, xy=T, cell=T, type= "p", pch=16, col="yellow")

#  x     y   cell defor2.1 defor2.2 defor2.3
# 1 54.5 321.5 111907      186      108      108
#     x     y   cell defor2.1 defor2.2 defor2.3
# 1 31.5 320.5 112601      177       10       18
#     x     y  cell defor2.1 defor2.2 defor2.3
# 1 47.5 338.5 99711      174       97      103
#     x     y  cell defor2.1 defor2.2 defor2.3
# 1 64.5 375.5 73199      180      100      101
#     x     y  cell defor2.1 defor2.2 defor2.3
# 1 35.5 381.5 68868      214      177      168
#     x     y  cell defor2.1 defor2.2 defor2.3
# 1 25.5 349.5 91802      189      163      166

## creo dataset
band<-c(1,2,3)
defor1_1<-c(218,12,32)
defor2_1<-c(186,108,108)
defor1_2<-c(207,10,27)
defor2_2<-c(177,10,18)
multitemp<-data.frame(band,defor1_1,defor2_1, defor1_2,defor2_2)

# plotto il grafico
ggplot(multitemp, aes(x=band)) +
              geom_line(aes(y=defor1_1), color="green") +
              geom_line(aes(y=defor2_1), color="blue") +
              geom_line(aes(y=defor1_2), color="green") +
               geom_line(aes(y=defor2_2), color="blue") +
              labs(x="bande",y="riflettanza")



### prova con altra immagine
loza<-brick("loza.jpg")
plotRGB(loza, r=1, g=2, b=3, stretch="hist")
click(loza, id=T, xy=T, cell=T, type= "p", pch=16, col="yellow")

# x      y    cell loza.1 loza.2 loza.3
# 1 3869.5 2975.5 4101918     43     85     75
#       x      y     cell loza.1 loza.2 loza.3
# 1 2389.5 1838.5 10431254    248    223    183
#      x      y    cell loza.1 loza.2 loza.3
# 1 796.5 2329.5 7695773      0     19     20
#       x      y     cell loza.1 loza.2 loza.3
# 1 3040.5 1330.5 13260449    150     89     68

band<-c(1,2,3)
zona1<-c(43,85,75)
zona2<-c(248,223,183)
zona3<-c(0,19,20)
zona4<-c(150,89,68)
tab<-data.frame(band,zona1,zona2,zona3,zona4)

ggplot(multitemp, aes(x=band)) +
              geom_line(aes(y=zona1), color="green") +
              geom_line(aes(y=zona2), color="blue") +
              geom_line(aes(y=zona3), color="red") +
               geom_line(aes(y=zona4), color="magenta") +
              labs(x="bande",y="riflettanza")



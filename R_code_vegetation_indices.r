### INDICI DI VEGETAZIONE

# uso library per richiamare i pacchetti che servono nel codice e poterli utilizzare
library(raster) # pacchetto con funzioni per elaborare file raster
library(RStoolbox)  # pacchetto con funzioni per processare le immagini (tra cui unsuperClass)
library(rasterdiv) # pacchetto che permette di calcolare diversi indici (tra cui il wordlwide NDVI)
library(rasterVis)  # pacchetto con funzioni aggiuntive per elaborare oggetti raster

setwd("C:/lab/")  # definisco la wd

# importo le due immagini come oggetti RasterBrick:
defor1<-brick("defor1.jpg")
defor2<-brick("defor2.jpg")

# Le bande dell'immagino sono:
# B1=NIR B2=RED B3=GREEN

par(mfrow=c(2,1))  # creo un multipanel di due righe dove plottare le immagini
plotRGB(defor1, r=1, g=2, b=3, stretch='lin')  # plotto le immagini all'interno del multipanel in veri colori
plotRGB(defor2, r=1, g=2, b=3, stretch='lin')

# Calcolo del DVI = (NIR-RED)
# I layer del RasteBrick defor1 si chiamano defor1.1, defor 1.2, defor1.3 e corrispondono alle tre bande
cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100) # definisco una scala colori
DVI1<-defor1$defor1.1 - defor1$defor1.2 # calcolo il DVI della prima immagine applicando la sottrazione tra matrici B1-B2 (NIR-RED) utilizzando $ per specificare il layer che mi interessa
plot(DVI1, col=cl, main="DVI at time 1") # plotto il risultato dell'operazione con la scala colori scelta e un titolo

DVI2<-defor2$defor2.1 - defor2$defor2.2 # calcolo il DVI della seconda immagine sottraendo B1-B2 (NIR-RED) 
plot(DVI2, col=cl, main="DVI at time 2") # plotto il DVI ottenuto

par(mfrow=c(2,1)) # creo un multipanel con due righe dove plottare i due DVI
plot(DVI1, col=cl, main="DVI at time 1")
plot(DVI2, col=cl, main="DVI at time 2")

# sottraggo il secondo DVI al primo per capire di quanto è calato nel tempo e plotto il risultato con una scala di colori scelta
deltaDVI<- DVI1 - DVI2
cld <- colorRampPalette(c('blue','white','red'))(100) 
plot(deltaDVI, col=cld, main="differenza DVI")

## calcolo del NDVI = (NIR-RED)/(NIR+RED)
# (normalizzo il DVI per la somma delle due bande)
NDVI1<-(defor1$defor1.1-defor1$defor1.2)/(defor1$defor1.1+defor1$defor1.2) # applico l'operazione tra matrici specificando i layer corrispondenti alle bande con $
NDVI2<-(defor2$defor2.1-defor2$defor2.2)/(defor2$defor2.1+defor2$defor2.2)
par(mfrow=c(2,1))  # creo un multipanel con due righe dove plottare i due NDVI calcolati
plot(NDVI1, col=cl, main="NDVI at time 1")
plot(NDVI2, col=cl, main="NDVI at time 2")

deltaNDVI<- NDVI1 - NDVI2  # calcolo la differenza tra i due NDVI per valutare quanto è calato nel tempo
plot(deltaNDVI, col=cld, main="differenza DVI")  # plotto il risultato del deltaNDVI


## RStoolbox:: spectralIndices

vi1<-spectralIndices(defor1, green=3, red=2, nir=1)  # la funzione spectraIndices del pacchetto RStoolbox produce in output una serie di indici multispettrali,
                                                      # tra cui anche NDVI, calcolati sul raster di input
plot(vi1, col=cl) # plotto gli indici calcolati 

vi2 <- spectralIndices(defor2, green = 3, red = 2, nir = 1) # applico la funzione spectralIndices anche sulla seconda immagine per ottenere gli indici
plot(vi2, col=cl)  # plotto il secondo risultato

### worldwide NDVI
plot(copNDVI) # copNDVI è un RasterLayer che fa parte del pacchetto rasterdiv e rappresenta l'NDVI a scala globale
copNDVI<-raster::reclassify(copNDVI, cbind(253:255,NA))  # riclassifico l'immagine trasformando in NA (non valori) i pixel scelti, in questo caso l'acqua, 
                                                         # tramite la funzione cbind

levelplot(copNDVI) # plotto il risultato con i pixel acqua eliminati

### VARIABILITA' SPAZIALE

# uso library per richiamare i pacchetti che servono nel codice e poterli utilizzare
library(raster)  # pacchetto con funzioni per elaborare file raster
library(RStoolbox)  # pacchetto con funzioni per processare le immagini (tra cui unsuperClass)
library(ggplot2)  # pacchetto con diverse funzioni per creare e modificare grafici
library(viridis)  # pacchetto con diverse palette di colori
library(gridExtra) # pacchetto con funzioni per lavorare con grafici (tra cui grid.arrange)

setwd("C:/lab/")  # definisco la working directory

sentinel<-brick("sentinel.png")  # importo l'immagine come oggetto RasterBrick

# Le layer presenti in questa immagine corrispondono alle bande:
#  sentinel.1= NIR , sentinel.2=RED , sentinel.3=GREEN

plotRGB(sentinel, r=1, g=2, b=3, stretch="lin")  # plotto l'immagine in falsi colori con r=nir g=red b=green
plotRGB(sentinel, r=2, g=1, b=3, stretch="lin")  # plotto l'immagine in falsi colori con r=red g=nir b=green

# calcolo l'indice NDVI 

# per separare le bande associo alle variabili NIR RED e GREEN i layer del RasterBrick corrispondenti a quelle bande
NIR<-sentinel$sentinel.1  # il primo layer è la banda NIR
RED<-sentinel$sentinel.2  # il secondo layer è la banda RED
GREEN<-sentinel$sentinel.3  # il secondo layer è la banda GREEN
NDVI=(NIR-RED)/(NIR+RED)  # calcolo NDVI secondo la sua formula 
plot(NDVI)  # visualizzo il risultato del calcolo di NDVI

cl <- colorRampPalette(c('black','white','red','magenta','green'))(100)  # definisco una color palette
plot(NDVI, col=cl)  # plotto il risultato del NDVI con la nuova color palette

### usare la funzione focal (da pacchetto raster) per analisi statistica utilizzando la moving window
# con w definisco le dimensioni della moving window, con fun la variabile statistica da calcolare

# Calcolo della deviazione standard

NDVI_sd3<-focal(NDVI, w=matrix(1/9,nrow=3,ncol=3), fun=sd) 
 # la funzione focal utilizza la tecnica della finestra mobile per calcolare diverse variabili statistiche in questo caso definiamo come variabile statistica di 
# interesse la deviazione standard (fun=sd) e definiamo la dimensione della finestra mobile 3x3 (1/9,nrow=3, ncol=3).  
# La finestra mobile calcola nel suo pixel centrale la variabile statistica delle celle circostanti, poi ripete il calcolo scorrendo lungo l'immagine
# Associo il risultato di questo calcolo alla funzione NDVI_sd3

cl2 <- colorRampPalette(c('blue','green','pink','magenta','orange','brown','red','yellow'))(100)  # definisco una color palette
plot(NDVI_sd3, col=cl2)  # plotto il risultato della deviazione standard con la scala colori scelta

# applico la funzione focal per calcolare la media
NDVI_m3<-focal(NDVI, w=matrix(1/9,nrow=3,ncol=3), fun=mean)
plot(NDVI_m3, col=cl2)  # visualizzo il risultato

# applico la funzione focal per calcolare la deviazione standar ma con una finestra mobile di dimensione 9x9
NDVI_sd9<-focal(NDVI, w=matrix(1/81,nrow=9,ncol=9), fun=sd)
plot(NDVI_sd9, col=cl2)  # visualizzo il risultato

# applico la funzione focal per calcolare la deviazione standar ma con una finestra mobile di dimensione 5x5
NDVI_sd5<-focal(NDVI, w=matrix(1/25,nrow=5,ncol=5), fun=sd)
plot(NDVI_sd5, col=cl2)  # visualizzo il risultato

# più piccola è la finestra mobile è maggiore è il dettaglio del risultato. A seconda del tipo di analisi che dobbiamo effettuare può esserci utile
# un grado maggiore o minore di dettaglio

sentinel_pca<-rasterPCA(sentinel)  # Applico un'analisi PCA all'immagine originale
plot(sentinel_pca$map)  # visualizzo la mappa risultate dall'analisi PCA
summary(sentinel_pca$model)  # visualizzo il sommario delle informazioni del modello dell'analisi PCA
# la PC1 spiega il 67,3% della variabilità totale

PC1<-sentinel_pca$map$PC1  # associo alla variabile PC1 il layer corrispondente all'interno della mappa prodotta dalla PCA
PC1_sd3<-focal(PC1, w=matrix(1/9,nrow=3,ncol=3), fun=sd)  # con la funzione focal calcolo la deviazione standard della PC1 con una finestra mobile 3x3
plot(PC1_sd3, col=cl2)  # visualizzo il risultato ottenuto

PC1_sd5<-focal(PC1, w=matrix(1/25,nrow=5,ncol=5), fun=sd)  # con la funzione focal calcolo la deviazione standard della PC1 con una finestra mobile 5x5
plot(PC1_sd5, col=cl2, main="PC1 standard deviation")  # visualizzo il risultato ottenuto

PC1_sd7<-focal(PC1, w=matrix(1/49,nrow=7,ncol=7), fun=sd)  # con la funzione focal calcolo la deviazione standard della PC1 con una finestra mobile 7x7
plot(PC1_sd7, col=cl2, main="PC1 standard deviation")  # visualizzo il risultato ottenuto

###############
# con ggplot creo un grafico dove visualizzo il risultato della deviazione standard calcolata con finestra mobile 5x5 con le diverse scale di colore fornite dal pacchetto viridis
# il pacchetto viridis contiene otto scale di colori:“viridis”, “magma”, “plasma”, “inferno”, “cividis”, “mako”, “rocket” ,“turbo”

# geom_raster mi permette di plottare un oggetto raster
# scale_fill_viridis() mi permette di scegliere la palette dal pacchetto viridis
# con ggtitle aggiungo un titolo

p0 <- ggplot() + geom_raster(PC1_sd5, mapping = aes(x = x, y = y, fill = layer)) + scale_fill_viridis() +  ggtitle("viridis palette")

p1 <- ggplot() + geom_raster(PC1_sd5, mapping = aes(x = x, y = y, fill = layer)) + scale_fill_viridis(option="magma") +  ggtitle("magma palette")

p2 <- ggplot() + geom_raster(PC1_sd5, mapping = aes(x = x, y = y, fill = layer)) + scale_fill_viridis(option="plasma") +  ggtitle("plasma palette")

p3 <- ggplot() + geom_raster(PC1_sd5, mapping = aes(x = x, y = y, fill = layer)) + scale_fill_viridis(option="inferno") +  ggtitle("inferno palette")

p4 <- ggplot() + geom_raster(PC1_sd5, mapping = aes(x = x, y = y, fill = layer)) + scale_fill_viridis(option="cividis") +  ggtitle("cividis palette")

p5 <- ggplot() + geom_raster(PC1_sd5, mapping = aes(x = x, y = y, fill = layer)) + scale_fill_viridis(option="mako") +  ggtitle("mako palette")

p6 <- ggplot() + geom_raster(PC1_sd5, mapping = aes(x = x, y = y, fill = layer)) + scale_fill_viridis(option="rocket") + ggtitle("rocket palette")

p7 <- ggplot() + geom_raster(PC1_sd5, mapping = aes(x = x, y = y, fill = layer)) + scale_fill_viridis(option="turbo") + ggtitle("turbo palette")

grid.arrange(p0, p1, p2, p3, p4, p5, p6, p7, nrow = 2)  # con grid.arrange creo un grafico multipanel con due righe dove plotto 
                                                         #  tutte le mappe che ho associato alle variabili p0, p1, p2, p3, p4, p5, p6, p7



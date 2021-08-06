# ESERCITAZIONE SULLA DIFFERENZA DI NO2 IN ATMOSFERA TRA GENNAIO E MARZO 2020  

# 1. set working directory su EN
# 2. import the first image (single band)
# 3. plot the first image with color ramp palette
# 4. import the last image and plot it
# 5. make the difference between the two images and plot it
# 6. plot everything altogether
# 7. import the all set
# 8. replicate plot images 1 and 13 using the stack
# 9. compute principle component analysis
# 10. compute of local variability (standard deviation) of PC1


library(raster)  # pacchetto con funzioni per elaborare file raster


#----------- 1. set working directory su EN  -----------#

setwd("C:/lab/EN") # definisco la working directory


#-----------  2. import the first image (single band)  -----------#

EN01<-raster("EN_0001.png")  # importo la prima immagine (NO2 a Gennaio) come oggetto raster


#-----------  3. plot the first image with color ramp palette  -----------#

cl<-colorRampPalette(c("blue","green","yellow")) (100)  # definisco una color palette
plot(EN01, col=cl)  # visualizzo l'immagine importata con la color palette scelta


#----------- 4. import the last image and plot it  -----------#

EN13<-raster("EN_0013.png")  # importo la seconda immagine (NO2 a Marzo) come oggetto raster
plot(EN13, col=cl)  # visualizzo l'immagine importata con la color palette scelta


#----------- 5. make the difference between the two images and plot it  -----------#

deltaEN<-EN01-EN13  # calcolo la differenza tra i valori di NO2 di Gennaio (EN01) e Marzo (EN13)
plot(deltaEN, col=cl)  # visualizzo il risultato ottenuto

#----------- 6.  plot everything altogether  -----------#

par(mfrow=c(3,1)) # imposto un multipanel su 3 righe dove plottare le immagini
plot(EN01,col=cl, main="NO2 in January")  # mappa di NO2 a Gennaio
plot(EN13,col=cl, main="NO2 in March")  # mappa di NO2 a Marzo
plot(deltaEN, col=cl, main="Difference between January and march") # mappa della differenza di NO2 tra Gennaio e Marzo

#----------- 7. import the all set  -----------#

# per importare tuti i file in una stessa variabile seguo il procedimento:
rlist<-list.files(pattern="EN") #creo una lista di file con pattern "EN" e associo alla variabile rlist
import<-lapply(rlist,raster)  # con lapply applico la funzione raster su tutti i file della lista e li importo associati alla variabile import
EN<-stack(import) # con stack creo un unico oggetto RasterStack che contiene come layer tutti i file importati


#----------- 8. replicate plot images 1 and 13 using the stack -----------#

par(mfrow=c(2,1)) # imposto un multipanel su 2 righe dove plottare le immagini
plot(EN$EN_0001,col=cl, main="NO2 in January") # mappa di NO2 a Gennaio presa dal layer corrispondente del RasterStack tramite $
plot(EN$EN_0013,col=cl, main="NO2 in March")  # mappa di NO2 a Marzo presa dal layer corrispondente del RasterStack tramite $


#----------- 9. compute principle component analysis  -----------#

EN_PCA<-rasterPCA(EN) # la funzione rasterPCA applica un'analisi PCA e produce un oggetto che contiene la mappa e le informazioni sul modello
summary(EN_PCA$model) # la funzione summary fa un sommario delle info del modello generato dall'analisi di rasterPCA, con le percentuali di varianza spiegate da ogni PC
plotRGB(EN_PCA$map, r=1, g=2, b=3, stretch='lin') # plotto in RGB le prime tre componenti principali generate dall'analisi PCA 


#----------- 10. compute of local variability (standard deviation) of PC1  -----------#

PC1<-EN_PCA$map$PC1 # associo alla variabile PC1 il layer corrispondente all'interno della mappa prodotta dalla PCA
PC1_sd3<-focal(PC1, w=matrix(1/9,nrow=3,ncol=3), fun=sd) # con la funzione focal calcolo la deviazione standard della PC1 con una finestra mobile 3x3
plot(PC1_sd3, col=cl) # visualizzo il risultato ottenuto










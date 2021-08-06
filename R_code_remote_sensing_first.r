# Primo codice in R per telerilevamento

# con installl.packages installo i pacchetti che mi interessano (è sufficiente farlo una volta)
# install.packages("raster") 
# install.packages("RStoolbox")
# install.packages("ggplot2")
# una volta installati i pacchetti uso library per richiamare quelli che servono nel codice e poterli utilizzare
library(raster) # pacchetto con funzioni per elaborare file raster  
library(RStoolbox) # pacchetto con funzioni per processare le immagini (tra cui unsuperClass)
library(ggplot2) # pacchetto con diverse funzioni per creare e modificare grafici

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

# con la funzione par definisco la visualizzazione di più grafici insieme in un multipanel con righe e colonne a mia scelta:
par(mfrow=c(2,1)) # 2 righe e 1 colonna
plot(p224r63_2011$B1_sre,col=cl1) 
plot(p224r63_2011$B2_sre,col=cl1)

par(mfrow=c(4,1)) # 4 righe e 1 colonna
plot(p224r63_2011$B1_sre,col=cl1) # plotto le immagini da inserire nel multipanel
plot(p224r63_2011$B2_sre,col=cl1)
plot(p224r63_2011$B3_sre,col=cl1)
plot(p224r63_2011$B4_sre,col=cl1)

par(mfrow=c(2,2)) # 2 righe e 2 colonne
plot(p224r63_2011$B1_sre,col=cl1) # plotto le immagini da inserire nel multipanel
plot(p224r63_2011$B2_sre,col=cl1)
plot(p224r63_2011$B3_sre,col=cl1)
plot(p224r63_2011$B4_sre,col=cl1)

# associo una scala colore diversa ad ogni banda e le plotto in un quadrato 2x2
par(mfrow=c(2,2)) # 2 righe 2 colonne
clb<- colorRampPalette(c("darkblue", "blue", "lightblue")) (100)  # definisco le scale colori
clv<- colorRampPalette(c("darkgreen", "green", "lightgreen")) (100)
clr<- colorRampPalette(c("darkred", "red", "orange")) (100)
clnir<- colorRampPalette(c("pink", "salmon","magenta")) (100)
plot(p224r63_2011$B1_sre,col=clb)  # plotto le immagini all'interno del multipanel con le nuove scale colori
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
par(mfrow=c(2,2)) # imposto un multipanel 2x2 dove plottare le immagini
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin") # immagine a colori naturali, R=RED G=GREEN B=BLUE
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") # immagine in falsi colori R=NIR G=RED B=GREEN
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin") # immagine in falsi colori R=RED G=NIR B=GREEN
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin") # immagine in falsi colori R=RED G=GREEN B=NIR
dev.off() # pulisco la finestra grafica dopo il procedimento per salvare correttamente il pdf

#confronto le immagin a colori naturali e in falsi colori (nir in verde) con diversi stretch (lin e his)
par(mfrow=c(3,1))  # imposto un multipanel 1x3 dove plottare le immagini
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin") # immagine a colori naturali, R=RED G=GREEN B=BLUE con stretch lin
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin") # immagine in falsi colori R=RED G=NIR B=GREEN con stretch lin
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="hist") # immagine in falsi colori R=RED G=NIR B=GREEN con stretch his


### OSSERVARE CAMBIAMENTI NEL TEMPO DELLA STESSA ZONA

p224r63_1988 <- brick("p224r63_1988_masked.grd") # con la funzione brick importo l'immagine del 1988 come oggetto RasterBrick
plot(p224r63_1988) # plotto tutte le bande separate
plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin") # plotto l'immagine a colori naturali

# multitemporal set con stretch lineare: plotto su due righe l'immagine del 1988 e del 2011 per confrontarle
par(mfrow=c(2,1)) # multipanel 1x2
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin") # immagine 1988
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") # immagine 2011

# multitemporal set con stretch histogram: plotto su due righe l'immagine del 1988 e del 2011 per confrontarle
par(mfrow=c(2,1)) # multipanel 1x2
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="hist") #immagine 1988
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="hist") #immagine 2011

# creo un pdf con un multipanel 4x4 delle immagini 1988 e 2011 in stretch lineare e hist
pdf("multitemporal_set_1988_2011_lin_hist") # imposto la creazione del pdf
par(mfrow=c(2,2)) # imposto righe e colonne 2x2 dove plottare le immagini
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin") # plotto immagine 1988
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") # plotto immagine 2011
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="hist") #plotto immagine 1988
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="hist") #plotto immagine 2011
dev.off() # pulisco la finestra grafica dopo il procedimento per salvare correttamente il pdf

# Primo codice in R per telerilevamento
# install.packages("raster")
# install.packages("RStoolbox")
library(raster) #richiamo il pacchetto raster
library(RStoolbox) #richiamo il pacchetto RStoolbox

setwd("C:/lab/") #imposto la working directory

### BANDE LANDSAT:
#B1= blu
#B2= verde
#B3= rosso
#B4= near infrared
#B5= middle infrared
#B6= infrarosso termico
#B7= infrarosso medio (altro sensore)

p224r63_2011 <- brick("p224r63_2011_masked.grd") # con la funzione brick estraggo i dati raster e li associo alla variabile
p224r63_2011 #leggo le informazioni raster
plot(p224r63_2011) #plotto l'immagine con la scala colori di default

cl1<-colorRampPalette(c("blue","green","orange","yellow")) (100) #definisco una nuova scala colore
plot(p224r63_2011,col=cl1) #plotto l'immagine con la nuova scala colore

dev.off() #pulisco la finestra grafica
plot(p224r63_2011$B1_sre, col=cl1) #plotto una sola banda a scelta usando $

#con la funzione par definisco la visualizzazione dei grafici in righe e colonne a mia scelta:
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

#associo una scala colore diversa ad ogni banda e le plotto in un quadrato 2x2
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

plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin") # immagine a colori naturali
plotRGB(p224r63_2011, r=4, g3=3, b=2, stretch="Lin") # immagine in falsi colori con infrarosso in rosso
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin") # immagine in falsi colori con infrarosso in verde
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin") # immagine in falsi colori con infrarosso in blu

#altre funzioni per stretch:
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="hist") #stretch histogram

# plottare in un 2x2 le 4 immagine precedenti
pdf("pdf1") #salvo il risultato come pdf nella wd
par(mfrow=c(2,2))
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin") # immagine a colori naturali
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") # immagine in falsi colori con infrarosso in rosso
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin") # immagine in falsi colori con infrarosso in verde
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin") # immagine in falsi colori con infrarosso in blu
dev.off()

#plot di immagine a colori naturali e in falsi colori (nir in verde) con stretch lin e his
par(mfrow=c(3,1))
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin") # immagine a colori naturali
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin") # immagine in falsi colori con infrarosso in verde
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="hist") # immagine in falsi colori con infrarosso in verde con stretch his




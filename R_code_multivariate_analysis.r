### ANALISI MULTIVARIATA

# con installl.packages installo i pacchetti che mi interessano (è sufficiente farlo una volta)
# una volta installati i pacchetti uso library per richiamare quelli che servono nel codice e poterli utilizzare

library(raster)  # pacchetto con funzioni per elaborare file raster
library(RStoolbox) # pacchetto con funzioni per processare le immagini (tra cui unsuperClass)

setwd("C:/lab/")  #definisco la working directory

p224r63_2011 <- brick("p224r63_2011_masked.grd")  # importo il file come oggetto RasterBrick, ovvero comporto da più layer

plot(p224r63_2011)  # plotto tutti i layer del raster importato

plot(p224r63_2011$B1_sre,p224r63_2011$B2_sre,col="red", pch=19, cex=2)  # plotto una grafico a dispersione (scatterplot) che mette in relazione i valori di due layer ponendoli sugli assi x e y

pairs(p224r63_2011) # con la funzione pairs creo una matrice di scatterplot che mettono in relazione i diversi layer scon tutte le combinazioni

### resampling (ricampionamento)
p224r63_2011res <- aggregate(p224r63_2011, fact=10)  # ricampiono il raster con un fattore 10 (in uscita ho 1 pixel ogni 100 del raster di partenza)

par(mfrow=c(2,1))  # imposto un multipanel con due righe dove plottare le due immagini
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch='lin') # immagine originale in RGB
plotRGB(p224r63_2011res, r=4, g=3, b=2, stretch='lin')  # immagine ricampionata in RGB

# L'analisi multivariata delle componenti principali PCA (Principal Component Analysis) permette la riduzione del numero di variabili senza perdere troppe informazioni.
# La PCA consiste nel proiettare le variabili originarie in un nuovo sistema cartesiano nel quale le variabili vengono ordinate in ordine decrescente di varianza: 
# la variabile con maggiore varianza viene proiettata sul primo asse (PC1), la seconda sul secondo asse (PC2) e così via. La riduzione della complessità avviene limitandosi 
# ad analizzare solo le componenti principali, ovvero quelle che spiegano maggiore varianza.

p224r63_2011res_PCA<-rasterPCA(p224r63_2011res) # la funzione rasterPCA applica un'analisi PCA e produce un oggetto che contiene la mappa e le informazioni sul modello
summary(p224r63_2011res_PCA$model) # la funzione summary fa un sommario delle info del modello generato dall'analisi di rasterPCA, con le percentuali di varianza spiegate da ogni PC
plot(p224r63_2011res_PCA$map)  # plotto tutte le componenti principali generate dall'analisi PCA

plotRGB(p224r63_2011res_PCA$map, r=1, g=2, b=3, stretch='lin')  # plotto in RGB le prime tre PC (quelle che spiegano maggiore varianza)
str(p224r63_2011res_PCA) # descrive le struttura dell'oggetto prodotto dall'analisi

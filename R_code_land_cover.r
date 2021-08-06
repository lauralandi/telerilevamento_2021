### LAND COVER

# uso library per richiamare i pacchetti che servono nel codice e poterli utilizzare
library(raster)  # pacchetto con funzioni per elaborare file raster
library(RStoolbox)  # pacchetto con funzioni per processare le immagini (tra cui unsuperClass)
library(ggplot2)   # pacchetto con diverse funzioni per creare e modificare grafici
library(gridExtra)  # pacchetto con funzioni per lavorare con grafici (tra cui grid.arrange)

setwd("C:/lab/")  # definisco la working directory

# importo le due immagini come RasterBrick:
defor1 <- brick("defor1.jpg") # immagine più vecchia
defor2 <- brick("defor2.jpg")  # immagine più recente

# I layer e le corrispondenti bande sono:
# defor1.1 = NIR
# defor1.2 = red
# defor1.3 = green


par(mfrow=c(2,1)) # creo un multipanel di due righe dove plottare le immagini importate
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")  # plotto la prima immagine in falsi colori r=nir g=red b=green
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin") # plotto la seconda immagine in falsi colori r=nir g=red b=green

# plotto le due immagini in falsi colori con la funzione ggRGB e le associo a due variabili p1 e p2
p1<-ggRGB(defor1, r=1, g=2, b=3, stretch="Lin") 
p2<-ggRGB(defor2, r=1, g=2, b=3, stretch="Lin")
grid.arrange(p1,p2,nrow=2) # con grid.arrange plotto le due immagini insieme su due righe

# applico una classificazione alla prima immagine, più vecchia:
set.seed(42)  # la funzione set.seed permette di ottenere sempre lo stesso risultato
d1c <- unsuperClass(defor1, nClasses=2)  # applico una classificazione a due classi
cl <- colorRampPalette(c('black','green'))(100) # definisco una prima color palette
cl2 <- colorRampPalette(c('green','black'))(100) # definisco una seconda color palette
par(mfrow=c(2,1)) # creo un multipanel di due righe dove plottare la mappa di classificazione con le due diverse colorpalette
plot(d1c$map, col=cl)  # mappa di classificazione con la prima color palette
plot(d1c$map, col=cl2)  # mappa di classificazione con la seconda color palette

# applico lo stesso procedimento di classificaizone sulla seconda foto, più recente:
set.seed(42)  # la funzione set.seed permette di ottenere sempre lo stesso risultato
d2c<- unsuperClass(defor2, nClasses=2)   # applico una classificazione a due classi
par(mfrow=c(2,1))  # creo un multipanel di due righe dove plottare la mappa di classificazione con le due diverse colorpalette
plot(d2c$map, col=cl) # mappa di classificazione con la prima color palette
plot(d2c$map, col=cl2)  # mappa di classificazione con la seconda color palette


# applico una classifazione a 3 classi su entrambe le immagini
d1c3 <- unsuperClass(defor1, nClasses=3) # applico una classificazione a 3 classi sulla prima foto
d2c3 <- unsuperClass(defor2, nClasses=3)  # applico una classificazione a 3 classi sulla seconda foto
par(mfrow=c(2,1)) # creo un multipanel a due righe dove plottare le due mappe di classificazione ottenute
plot(d1c3$map) # mappa di classificazione prima immagine
plot(d2c3$map) # mappa di classificazione seconda immagine


#### CALCOLARE LE AREE DI FORESTA PERSE

# Ottenuta la classificazione posso calcolare la frequenza dei pixel nelle diverse classi:
freq(d1c$map)  # la funzione freq restituisce il numero di pixel che ricade in ogni classe
#     value  count
# [1,]     1 305922
# [2,]     2  35370

somma1<- 305922+35370 # sommo i pixel di ogni classe per conoscere il totale
prop1<-freq(d1c$map)/somma1 # divido i valori di pixel appartenenti ad ogni classe per il totale in modo da ottenere i valori proporzionali delle classi 
#       value     count
# [1,] 2.930042e-06 0.8963644
# [2,] 5.860085e-06 0.1036356

# dai valori proporzionali definisco le percentuali di ogni classe:
# classe 1 = 89,6% 
# classe 2 = 10,3%

# applicp lo stesso procedimento per la classificazione della seconda foto:
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


### GENERARE UN DATASET (DATAFRAME)

# per creare un dataset definisco le variabili che corrisponderanno alle colonne:
cover<-c("Forest","Agricolture") # applico alla variabile cover le etichette che voglio associare alle due classi
percent_1992<-c(89.64,10.36) # applico alla variabile percent_1992 i valori percentuali calcolati dalla classificazione della prima immagine
percent_2006<-c(52.03,47.97) # applico alla variabile percent_2006 i valori percentuali calcolati dalla classificazione della seconda immagine
percentages<-data.frame(cover,percent_1992,percent_2006)  # con la funzione data.frame creo un dataset contenente le variabili precedentemente definite
percentages  # richiamando la variabile mi restituisce il dataset costruito

# creo due grafici con i valori delle due classificazioni all'interno del dataser
g1<-ggplot(percentages, aes(x=cover,y=percent_1992,color=cover)) +  # con ggplot creo un grafico con i dati del dataset impostando i dati del 1992 sull'asse y 
                                                                                                                     # e le classi (le loro etichette) sull'asse x
            geom_bar(stat="identity", fill="white")   # geom_bar permette di relizzare un grafico a barre

g2<-ggplot(percentages, aes(x=cover,y=percent_2006,color=cover)) + geom_bar(stat="identity", fill="white") # creo un secondo grafico a barre con i dati del 2006 sull'asse y

# aes indica l'estetica del grafico
# geom indica il tipo di geometria con cui visualizzare il dato
# color indica a quali oggetti facciamo discriminare le classi
# stat="identity" indica che usiamo i dati del nostro dataset

grid.arrange(g1,g2,nrow=2)  # con grid.arrange plotto i due grafici su due righe

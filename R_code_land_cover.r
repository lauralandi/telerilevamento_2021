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

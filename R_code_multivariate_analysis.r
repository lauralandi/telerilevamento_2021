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

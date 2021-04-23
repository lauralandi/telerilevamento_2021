### ANALISI MULTIVARIATA

library(raster)
library(RStoolbox) #richiamo il pacchetto RStoolbox

setwd("C:/lab/")

p224r63_2011 <- brick("p224r63_2011_masked.grd")

plot(p224r63_2011$B1_sre,p224r63_2011$B2_sre,col="red", pch=19, cex=2)
pairs(p224r63_2011)

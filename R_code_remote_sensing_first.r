# Primo codice in R per telerilevamento
# install.packages("raster")
library(raster)

setwd("C:/lab/")

# p224r63_2011 <- brick("p224r63_2011_masked.grd")
# p224r63_2011
# plot(p224r63_2011)

#cambio colore
cl1<-colorRampPalette(c("black","grey","light grey")) (100)
#plot(p224r63_2011,col=cl1)

cl2<-colorRampPalette(c("blue","green","yellow","pink")) (100)
#plot(p224r63_2011,col=cl1)

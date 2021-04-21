  
### CLASSIFICAZIONE IMMAGINI 
### DATI SOLAR ORBITER

library(raster)

library(RStoolbox) #richiamo il pacchetto RStoolbox
library(ggplot2) #richiamo il pacchetto ggplot2


setwd("C:/lab/")

so <- brick("Solar_Orbiter_s_first_views_of_the_Sun_pillars.jpg")
so

plotRGB(so, 1,2,3, stretch="lin") #visualizzo le tre bande montate in RGB

set.seed(42) # fa in modo che il risultato sia sempre lo stesso nei diversi run utilizzando sempre lo stesso set di pixel che altrimenti sarebbe sempre randomica
so_c <- unsuperClass(so, nClasses=3) #classificazone non supervisionata
so_c20 <- unsuperClass(so, nClasses=20) #classificazione con 20 classi

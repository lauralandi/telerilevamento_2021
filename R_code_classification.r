  
### CLASSIFICAZIONE IMMAGINI 
### DATI SOLAR ORBITER

library(raster)

library(RStoolbox) #richiamo il pacchetto RStoolbox
library(ggplot2) #richiamo il pacchetto ggplot2


setwd("C:/lab/")

so <- brick("Solar_Orbiter_s_first_views_of_the_Sun_pillars.jpg")
so #vedo le info della immagine
plot(so) #visualizzo le tre bande separate
plotRGB(so, 1,2,3, stretch="lin") #visualizzo le tre bande montate in RGB

set.seed(42) # fa in modo che il risultato sia sempre lo stesso nei diversi run utilizzando sempre lo stesso set di pixel che altrimenti sarebbe sempre randomica
so_c3 <- unsuperClass(so, nClasses=3) #classificazone non supervisionata che produce una mappa e il model con le info
so_c20 <- unsuperClass(so, nClasses=20) #classificazione con 20 classi
plot(so_c3$map) # plotto la mappa prodotta dalla funzione unsuperClass
plot(so_c20$map)

#carico una seconda immagine
so2 <- brick("Solar_Orbiter_spots_campfires_on_the_Sun_annotated.jpg")
plotRGB(so2, 1, 2, 3, stretch="lin")

so2_c3 <- unsuperClass(so2, nClasses=3) 
plot(so2_c3$map)
so2_c20 <- unsuperClass(so2, nClasses=20)
plot(so2_c20$map)

fiume<-brick("photo6034998005653942920.jpg")
fiume_c3 <- unsuperClass(fiume, nClasses=3) 
plot(fiume_c3$map)
fiume_c20 <- unsuperClass(fiume, nClasses=20)
plot(fiume_c20$map)



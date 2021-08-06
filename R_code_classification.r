  
### CLASSIFICAZIONE IMMAGINI 
### DATI SOLAR ORBITER

# uso library per richiamare i pacchetti che servono nel codice e poterli utilizzare
library(raster)  # pacchetto con funzioni per elaborare file raster
library(RStoolbox) # pacchetto con funzioni per processare le immagini (tra cui unsuperClass)
library(ggplot2) # pacchetto con diverse funzioni per creare e modificare grafici

setwd("C:/lab/") # definisco la wd

so <- brick("Solar_Orbiter_s_first_views_of_the_Sun_pillars.jpg")  # importo il file come un oggetto RasterBrick, ovvero costituito da più RasterLayer
so # vedo le informazioni dell'immagine importata
plot(so) # visualizzo le tre bande separate
plotRGB(so, 1,2,3, stretch="lin") # visualizzo le tre bande montate in RGB

set.seed(42) # la funzione set.seed fa in modo che il risultato sia sempre lo stesso nei diversi run utilizzando sempre lo stesso set di pixel che altrimenti sarebbe ogni volta randomico
so_c3 <- unsuperClass(so, nClasses=3) # classificazone non supervisionata che produce un oggetto costiruio dalla mappa e dal modello con le informazioni
so_c20 <- unsuperClass(so, nClasses=20) #classificazione con 20 classi
plot(so_c3$map) # plotto la mappa prodotta dalla funzione unsuperClass, richiamandola con $ all'interno dell'oggetto output della classificazione
plot(so_c20$map)

so2 <- brick("Solar_Orbiter_spots_campfires_on_the_Sun_annotated.jpg") # # importo una seconda immagine come un oggetto RasterBrick
plotRGB(so2, 1, 2, 3, stretch="lin")  # plotto l'immagine in RGB

# Applico classificazioni con diverso numero di classi
so2_c3 <- unsuperClass(so2, nClasses=3) # classificazione con 3 classi
plot(so2_c3$map) # visualizzo la mappa prodotta con 3 classi
so2_c20 <- unsuperClass(so2, nClasses=20) # calssificazione con 20 classi
plot(so2_c20$map) # visualizzo la mappa prodotta con 20 classi


### DATI GRAND CANYON

gc<-brick("dolansprings_oli_2013088_canyon_lrg.jpg") # importo l'immagine del grandcanyon come RasterBrick
plotRGB(gc,r=1,g=2,b=3,stretch="lin") # plotto l'immagine in RGB

gc_c2 <- unsuperClass(gc, nClasses=2)  # applico una classificazione con 2 classi
cl <- colorRampPalette(c('blue','yellow'))(100) # definisco una color palette
plot(gc_c2$map, col=cl) # plotto la mappa di classificazione con la scala di colori scelta

# applico lo stesso procedimento per una seconda classificazione con 4 classi:
gc_c4 <- unsuperClass(gc, nClasses=4) # classificazione con 4 classi
cl2 <- colorRampPalette(c('blue','green', 'orange','yellow'))(100) # definisco una color palette
plot(gc_c4$map, col=cl2)  # plotto la mappa di classificazione con la scala di colori scelta

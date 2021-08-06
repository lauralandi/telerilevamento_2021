### VISUALIZZAZIONE DATI COPERNICUS

# con installl.packages installo i pacchetti che mi interessano (è sufficiente farlo una volta)
# una volta installati i pacchetti uso library per richiamare quelli che servono nel codice e poterli utilizzare

#install.packages("ncdf4")
library(raster) # pacchetto con funzioni per elaborare file raster
library(ncdf4)  # pacchetto che permette di importare ed elaborare file in formato .nc
library(RStoolbox) # pacchetto con funzioni per processare le immagini (tra cui unsuperClass)
library(ggplot2) # pacchetto con diverse funzioni per creare e modificare grafici
library(rasterVis) # pacchetto con funzioni aggiuntive per elaborare oggetti raster

setwd("C:/lab/")  # definisco la working directory

SWI<-raster("c_gls_SWI1km_202104071200_CEURO_SCATSAR_V1.0.1.nc")  # importo il file come oggetto raster

cl <- colorRampPalette(c("red","pink","light blue","blue"))(100)
cl2 <- colorRampPalette(c("red","yellow","pink","white"))(100)
levelplot(SWI,col.regions=cl2)  # plotto l'immagine con la scala colori scelta

## resampling
SWIres<-aggregate(SWI,fact=10) # con la funzione aggregate posso ricampionare il raster, ovvero ridurre il numero di pixel
                               # con fact=10 calcola per ogni gruppo 10x10 di pixel una media producendo per ognuna di queste un solo pixel più grande in uscita
plot(SWIres,col=cl,main="Soil Water Index") # plotto il risultato del ricampionamento

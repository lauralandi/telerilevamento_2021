### TIME SERIES ANALYSIS
### GREENLAND INCREASE OF TEMPERATURE
### Data and code from Emanuela Cosma

# con installl.packages installo i pacchetti che mi interessano (è sufficiente farlo una volta)
# una volta installati i pacchetti uso library per richiamare quelli che servono nel codice e poterli utilizzare
# install.packages("raster")
library(raster)  # pacchetto con funzioni per elaborare oggetti raster
# install.packages("RStoolbox")
library(RStoolbox) # pacchetto con funzioni per processare le immagini (tra cui unsuperClass)
# install.packages("rasterVis")
library(rasterVis)  # pacchetto con funzioni aggiuntive per elaborare oggetti raster


setwd("C:/lab/greenland/")  # definisco la working directory

lst_2000<-raster("lst_2000.tif") # con la funzione raster importo un file come oggetto raster
lst_2005<-raster("lst_2005.tif")
lst_2010<-raster("lst_2010.tif")
lst_2015<-raster("lst_2015.tif")

# con la funzione par creo un multipanel 2x2 con le 4 immagini importate
par(mfrow=c(2,2))
plot(lst_2000)
plot(lst_2005)
plot(lst_2010)
plot(lst_2015)

rlist<-list.files(pattern="lst") # creo una lista di file dalla wd secondo un pattern (tutti i file che contengono lst nel nome) e la associo alla variabile rlist
import<-lapply(rlist,raster) # con lapply applico la funzione raster su tutti i file della lista: associo quindi alla variabile import una lista di oggetti raster
TGr<-stack(import) # con la funzione stack creo un unico oggetto di tipo RasterStack che contiene come layer i raster importati dalla lista

plotRGB(TGr, 1, 2, 3, stretch="lin") # plotto l'immagine montando i layer in RGB

levelplot(TGr$lst_2000) # con la funzione levelplot il singolo layer è plottato insieme ai grafici che, lungo la direzione x e y, rappresentano la media rispettivament di ogni colonna e riga
cl <- colorRampPalette(c("blue","light blue","pink","red"))(100)  
levelplot(TGr,col.regions=cl,main="Summer land surface temperature",       # la funzione levelplot permette di arricchire il grafico la color palette scelta,
          names.attr=c("July 2000","July 2005", "July 2010", "July 2015"))                                 # i titoli dei singoli layer e un titolo generale
                                                                           
                                                                                                                                 

### Melt Data 
# I dati sullo scioglimento sono un file per ogni anno, per importarli in un singolo oggetto seguo il procedimento:
melt_list<-list.files(pattern="melt")  # creo una lista con tutti i file che contengono melt nel nome
melt_import<-lapply(melt_list,raster)  # importo la lista di file come oggetti raster
melt_stack<-stack(melt_import)  # creo un oggetto RasterStack che unisce tutti i layer importati
levelplot(melt_stack,col.regions=cl,main="Annual Melt Data",names.attr=c("1979","1980", "1981", "1982", "1983", "1984", "1985", "1986", 
                                                                          "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", 
                                                                          "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", 
                                                                          "2003", "2004", "2005", "2007"))
# uso levelplot per plottare tutti i layer insieme

# Per calcolare la variazione dello scioglimento tra il 1979 e il 2007 applico una sottrazione tra matrici (ovvero i corrispondenti layer)
melt_amount<- melt_stack$X2007annual_melt - melt_stack$X1979annual_melt # alla variabile melt_amount associo il risultato della differenza tra il layer del 2007
                                                                        # e quello del 1979 (che definisco tramite $)
cl2<-colorRampPalette(c("blue","white","red"))(100) # scelgo una scala di colori che metta in risalto le differenze positive e quelle negative
levelplot(melt_amount,col.regions=cl2) # plotto il risultato della differenza con la scala di colori scelta

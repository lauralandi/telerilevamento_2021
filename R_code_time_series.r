### TIME SERIES ANALYSIS
### GREENLAND INCREASE OF TEMPERATURE
### Data and code from Emanuela Cosma

# install.packages("raster")
library(raster)
# install.packages("RStoolbox")
library(RStoolbox)
# install.packages("rasterVis")
library(rasterVis)
# install.packages("knitr")
library(knitr)

setwd("C:/lab/greenland/")
lst_2000<-raster("lst_2000.tif") #importo un layer raster
lst_2005<-raster("lst_2005.tif")
lst_2010<-raster("lst_2010.tif")
lst_2015<-raster("lst_2015.tif")

#creare multipanel con le 4 immagini
par(mfrow=c(2,2))
plot(lst_2000)
plot(lst_2005)
plot(lst_2010)
plot(lst_2015)

rlist<-list.files(pattern="lst") #creo una lista di file e associo alla variabile rlist
import<-lapply(rlist,raster) #applico la funzione raster su tutti i file della lista e associo alla variabile import (in questo modo li importo tutti insieme)
TGr<-stack(import) # creo un univo file che contiene tutti quelli della lista importata

plotRGB(TGr, 1, 2, 3, stretch="lin")

levelplot(TGr) #plot dell'immagine
levelplot(TGr) #plot del singolo layer dell'immagine con grafici della medie colonne e righe
cl <- colorRampPalette(c("blue","light blue","pink","red"))(100)
levelplot(TGr,col.regions=cl,main="Summer land surface temperature",names.attr=c("July 2000","July 2005", "July 2010", "July 2015"))

### Melt Data 
melt_list<-list.files(pattern="melt")
melt_import<-lapply(melt_list,raster)
melt_stack<-stack(melt_import)
levelplot(melt_stack,col.regions=cl,main="Annual Melt Data",names.attr=c("1979","1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", 
                                                                         "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", 
                                                                         "2004", "2005", "2007"))

#sottrazione tra matrici 2007-1979
melt_amount<- melt_stack$X2007annual_melt - melt_stack$X1979annual_melt
cl2<-colorRampPalette(c("blue","white","red"))(100)
levelplot(melt_amount,col.regions=cl2)

###########################################################################################################################################################################
###########################################################################################################################################################################
###################################################################  PROVA SARDEGNA UFFICIALE  #############################################################################
###########################################################################################################################################################################
###########################################################################################################################################################################


###################################################################################################
#   STEP 1  -  Richiamare tutte le library necessarie al codice e definire la working directiory  #
###################################################################################################

library(raster) #richiamo il pacchetto raster
library(RStoolbox) #richiamo il pacchetto RStoolbox
library(ggplot2) #richiamo il pacchetto ggplot2
library(gridExtra)
library(rgdal)
library(wesanderson)

setwd("C:/lab/esame_sardegna/")

################################################################################
#   STEP 2  -  Scegliere le immagini su cui effettuare l'analisi e importarle  #
################################################################################

## Le due immagini utilizzate derivano da Sentinel-2 e rappresentano la zona del Montiferru in provincia di 
## Oristano il 10 e il 25 Luglio 2021, ovvero prima e dopo gli incendi che hanno colpito l'area in quel mese.

## Le diverse bande in cui il satellite Sentinel-2 fornisce le immagini sono:

##       BANDA                  LUNGHEZZA D'ONDA (micrometri)      RISOLUZIONE SPAZIALE (metri)
##   B01= coastal aerosol                 0.443                           60
##   B02= blue                            0.490                           10
##   B03= green                           0.560                           10
##   B04= red                             0.665                           10
##   B05= vegetation red edge             0.705                           20
##   B06= vegetation red edge             0.740                           20
##   B07= vegetation red edge             0.783                           20
##   B08= NIR                             0.842                           10
##   B08A= vegetation red edge            0.865                           20
##   B09= water vapour                    0.945                           60
##   B11= SWIR                            1.610                           20
##   B12= SWIR                            2.190                           20


## Nello specifico per questa analisi sono state sfruttate le bande
##   B02= blue 
##   B03= green
##   B04= red  
##   B08= NIR
##   B11= SWIR
##   B12= SWIR


# Le diverse bande sono state scaricate separatamente, quindi per caricarle creo due liste di file con pattern rispettivamente "july10" e "july25". 
# Dopodichè la funzione lapply mi permette di applicare all'intera lista la funzione raster per importare gli elementi come oggetti RasterLayer. 
# Infine con la funzione stack unisco tutti i raster in un unico oggetto RasterStack, che conterrà tutte le bande (il procedimento è ripetuto per entrambe le immagini)

rlist10<-list.files(pattern="july10") #creo una lista di file con le diverse bande con pattern "july10" e associo alla variabile rlist10
import10<-lapply(rlist10,raster) # con lapply applico la funzione raster su tutti i file della lista e li importo associati alla variabile import10
july10<-stack(import10) # creo un unico oggetto RasterStack che contiene come layer tutte le bande importate

rlist25<-list.files(pattern="july25") #creo una lista di file con le diverse bande con pattern "july25" e associo alla variabile rlist25
import25<-lapply(rlist25,raster) # con lapply applico la funzione raster su tutti i file della lista e li importo associati alla variabile import25
july25<-stack(import25) # creo un unico oggetto RasterStack che contiene come layer tutte le bande importate

# Richiamando le variabili posso leggerne le informazioni e i nomi e la posizione dei layer
july10
july25
### AGGIUNGERE INFO IMMAGINI E ORDINE BANDE

############################################################
#   STEP 3  -  Elaborazione e osservazione delle immagini  #
############################################################

## PLOT1 - Le due immagini (10 e 25 Luglio) in RGB veri colori: r=red, g=green, b=blue
p1<-ggRGB(july10, 4, 3, 2, stretch="lin", quantiles = c(0.0001, 0.9999))  # con la funzione ggRGB monto le immagini in RGB (veri colori) e le associo alle variabili p1 e p2 (modificando i quantili regolo lo stretch della foto)
p2<-ggRGB(july25, 4, 3, 2, stretch="lin")
grid.arrange(p1, p2, nrow = 2)     # con la funzione grid.arrange plotto le due immagini insieme in un unico grafico

## PLOT2 - Le due immagini (10 e 25 Luglio) in falsi colori: r=NIR, g=green, b=blue
# Questa modalità di visualizzazione esalta in rosso la vegetazione, poichè essa ha un'alta riflettanza nella banda del NIR
p3<-ggRGB(july10, 8, 3, 2, stretch="lin", quantiles = c(0.0001, 0.9999))
p4<-ggRGB(july25, 8, 3, 2, stretch="lin")
grid.arrange(p3, p4, nrow = 2)
# il PLOT2 mostra come il 25 Luglio si sia formata una grossa area scura dove la vegetazione è scomparsa a causa degli incendi

## PLOT3 - Le due immagini (10 e 25 Luglio) in falsi colori: r=SWIR(B12), g=SWIR(B11), b=red
# Questa modalità di visualizzazione esalta in rosso le aree bruciate, poichè esse mostrano un'alta riflettanza nella banda del SWIR
p5<-ggRGB(july10, 11, 10, 4, stretch="lin", quantiles = c(0.0001, 0.9999))
p6<-ggRGB(july25, 11, 10, 4, stretch="lin")
grid.arrange(p5, p6, nrow = 2)
# il PLOT3 evidenzia l'area bruciata e le zone più colpite con una maggiore riflettanza nel SWIR


############################################################
#   STEP 4  -  Focus sull'area di interesse per l'analisi  #
############################################################

## Per analizzare meglio l'area bruciata dagli incendi mi focalizzo su un'area più stretta.
## In questo modo limito le interferenze che la presenza del mare e delle nuvole possono avere sull'analisi

plotRGB(july25, 11, 10, 4, stretch="lin")
# e <- drawExtent(show=TRUE, col="red") # con la funzione drawExtent disegno un riquadro sul plot appena aperto e genero un oggetto extent associato alla variabile e
e   # richiamando la variabile e ottengo le sue informazioni:
    # class      : Extent 
    # xmin       : 943072.7 
    # xmax       : 965034.5 
    # ymin       : 4882167 
    # ymax       : 4905410
e<-extent(943072.7, 965034.5, 4882167, 4905410)  # DA CANCELLARE PRIMA DI ESAME

july25_crop<- crop(july25, e)  # con la funzione crop ritaglio le immagini nelle dimensioni definite dalla variabile e
july10_crop<- crop(july10, e)

## PLOT4 - Le due immagini ritagliate, in falsi colori
p6<-ggRGB(july10_crop, 11, 10, 4, stretch="lin", quantiles = c(0.0001, 0.9999))
p7<-ggRGB(july25_crop, 11, 10, 4, stretch="lin")
grid.arrange(p6, p7, ncol = 2)

################################################
#   STEP 4  -  Calcolo degli indici NDVI e NBR #
################################################

# SPIEGARE BREVEMENTE I DUE INDICI

## NDVI= (NIR-RED)/(NIR+RED)
## NBR=  (NIR-SWIR)/(NIR+SWIR)

#  BANDA   NOME LAYER JULY10   NOME LAYER JULY25
# NIR=B08     july10_B08          july25_B08
# RED=B04     july10_B04          july25_B04
# SWIR=B12    july10_B12          july25_B12

## per l'NBR serve SWIR co lamba tra 2080 e 2350, quindi per sentinel è B12



## Calcolo del NDVI

NDVI_july10<-(july10_crop$july10_B08-july10_crop$july10_B04)/(july10_crop$july10_B08+july10_crop$july10_B04) # NDVI=(NIR-RED)/(NIR+RED)
NDVI_july25<-(july25_crop$july25_B08-july25_crop$july25_B04)/(july25_crop$july25_B08+july25_crop$july25_B04)

## PLOT5 - Confronto tra NDVI del 10 Luglio e NDVI del 25 luglio
clz<-wes_palette("Zissou1", 100, type = c("continuous"))  #associo alla variabile clz una palette di colori dal pacchetto wesanderson
par(mfrow=c(1,2))  # con la funzione par plotto in unico grafico le due immagini
plot(NDVI_july10, col=clz, main="NDVI 10 Luglio")  # plot del raster NDVI calcolato con la palette di colori scelta
plot(NDVI_july25, col=clz, main="NDVI 25 Luglio")

## Calcolando la differenza tra i due NDVI se ne può quantificare il calo dopo gli incendi
deltaNDVI<- NDVI_july10 - NDVI_july25  

## PLOT6 - Il deltaNDVI 
cld <- colorRampPalette(c('blue','white','red'))(100) # definisco una palette che associa al rosso i valori di differenza maggiori, ovvero il calo di NDVI maggiore
plot(deltaNDVI, col=cld, main="differenza NDVI") ## <-- controllare come forzare a zero il limite della legenda


## Calcolo del NBR

NBR_july10<-(july10_crop$july10_B08-july10_crop$july10_B12)/(july10_crop$july10_B08+july10_crop$july10_B12)  # NBR= (NIR-SWIR)/(NIR+SWIR)
NBR_july25<-(july25_crop$july25_B08-july25_crop$july25_B12)/(july25_crop$july25_B08+july25_crop$july25_B12)

## PLOT7 - Confronto tra NBR del 10 Luglio e NBR del 25 luglio
par(mfrow=c(1,2))
plot(NBR_july10, col=clz, main="NBR 10 Luglio")
plot(NBR_july25, col=clz, main="NBR 25 Luglio")

## Calcolando la differenza tra i due NBR se ne può quantificare il calo dopo gli incendi
deltaNBR<- NBR_july10 - NBR_july25

## PLOT8 - Il deltaNBR
#cld2 <- colorRampPalette(c('red','white','blue'))(100)  
plot(deltaNBR, col=cld, main="differenza NBR") ## <-- controllare come forzare a zero il limite della legenda


## PLOT9 - Confronto tra il deltNDVI e il deltaNBR
par(mfrow=c(1,2))
plot(deltaNDVI, col=cld, main="differenza NDVI")
plot(deltaNBR, col=cld2, main="differenza NBR")


#############################################
#  STEP 5  -  Classificazione del deltaNBR  #
#############################################

# Applico una classificazione dell'indice deltaNBR per riconoscere le aree più danneggiate dagli incendi

set.seed(42)  # la funzione set.seed mi permette di poter replicare più volte lo stesso processo che altrimenti sarebbe sempre randomico ???
dNBR_c4<-unsuperClass(deltaNBR, nClasses=4) # con la funzione unsuperClass applico una classificazione non suervisionata di 4 classi e la associo alla variabile dNBR_c4

## PLOT10 - La classificazione del deltaNBR
clc <- colorRampPalette(c('red','green','yellow','darkgreen'))(100) # definisco una palette con 4 colori per le classi
plot(dNBR_c4$map, col = clc, legend = FALSE, axes = FALSE, box = FALSE)
legend(965031,4905419, legend = paste0("C",1:4), fill = clc,
      title = "Classi", horiz = FALSE,  bty = "n")


HCLASS<-hist(dNBR_c4$map,
     main = "Distribution of raster cell values in the NBR difference data",
     breaks= c(0,1, 2, 3, 4),
     xlab = "classi", ylab = "Number of Pixels",
     col = colors)


dNBR_c4$map # richiamando la variabile della mappa di classificazione ottengo informazioni sulla sua estensione:                          
            # xmin 943077.6
            # xmax 965030.7
            # ymin 4882162
            # ymax 4905419
            # In base alle quali posiziono la legenda:
            # x 965031
            # y 4905419


## PLOT11 - Confronto tra il deltaNBR e la sua classificazione 
par(mfrow=c(1,2))
plot(deltaNBR, col=cld, main="differenza NBR")
plot(dNBR_c4$map, col = clc, legend = FALSE, axes = FALSE, box = FALSE, main="Classificazione deltaNBR")
legend(965031,4905419, legend = paste0("C",1:4), fill = clc,
      title = "Classi", horiz = FALSE,  bty = "n")


## PLOT12 - Confronto tra l'immagine infalsi colori e la classificaizone di deltaNBR
par(mfrow=c(1,2))
plotRGB(july25_crop, 11, 10, 4, stretch="lin")
plot(dNBR_c4$map, col = clc, legend = FALSE, axes = FALSE, box = FALSE, main="Classificazione deltaNBR")
legend(965031,4905419, legend = paste0("C",1:4), fill = clc,
      title = "Classi", horiz = FALSE,  bty = "n")


## Ottenuta la classificazione è possibile calcolare le percentuali di area che rientrano nelle diverse classi

freq(dNBR_c4$map)  # con la funzione freq ricavo in numero di pixel che ricade in ogni classe
#  value   count
#    1     210375
#    2     359710
#    3     99897
#    4     277910

pxtot<- 210375 + 359710 + 99897 + 277910
perc<-freq(dNBR_c4$map)/pxtot   # normalizzo il numero di pixel di ogni classe per il numero totale dei pixel per ottenere i valori percentuali
#  value   count
#   1    0.2219398
#   2    0.3794842
#   3    0.1053886
#   4    0.2931874

# Per cui approssimando si ha:
# C1: 22%
# C2: 38%
# C3: 11%
# C4: 29%

## Conoscendo la risoluzione dell'immagine è possibile calcolare le aree di territorio coinvolte dall'incendio

dNBR_c4$map # richiamando la variabile della mappa ottengo le informazioni sulla risoluzione lungo x e y    
            # x: 23.20628 m
            # y: 23.21091 m
            
# L'area di un pixel quindi è 
# 23.20628 * 23.21091 = 538.6389 m2
# L'area totale è
# 538.6389 * pxtot = 510571504 m2 = 510.571504 km2


##########################################
#  STEP 6  -  Costruzione di un dataset  #
##########################################

## Ottenuti i dati di interesse li inserisco in un dataset
Classi<-c("C1","C2", "C3", "C4") # alla variabile Classi associo i nomi delle classi ottenute
Danno<- c( "Intermedio", "Nullo", "Alto", "Nullo")  # alla variabile Danno associo una descrizione qualitativa del danno sulla base dei valori di deltaNBR (maggiori per danni maggiori)
Area_percentuale<-c(0.22, 0.38, 0.11, 0.29)  # alla variabile Area_percentuale associo i valori ricavati precedentemente
Area_km2<-Area_percentuale*510.571504  # moltiplicando l'area percentuale per l'area totale in km2 ottengo le aree in km2 che rientrano nelle 4 classi

percent<-data.frame(Classi, Danno, Area_percentuale, Area_km2)  # con la funzione data.frame costruisco il dataframe e lo associo alla variabile percent
percent 

## PLOT13 - Grafico a barre dei dati presenti nel dataframe
g5<-ggplot(percent, aes(x=Danno,y=Area_km2,color=Classi)) 
           + geom_bar(stat="identity", width=0.2, (aes(fill = Classi))) # con la funzione ggplot creo un grafico a barre che mostra 
                                                                        # le aree e le classi che rientrano nei danni

################################################################################################################################################################################
###############################################################                                            #####################################################################
###############################################################   ESAME DI TELERILEVAMENTO GEO-ECOLOGICO   #####################################################################
###############################################################               10 Agosto 2021               #####################################################################
###############################################################                Laura Landi                 #####################################################################
###############################################################                                            #####################################################################
################################################################################################################################################################################



#                             INDICE
#
#  STEP 1  -  Richiamare le library necessarie al codice e definire la working directory
#  STEP 2  -  Scelta delle immagini su cui effettuare l'analisi e importazione
#  STEP 3  -  Elaborazione e osservazione delle immagini
#  STEP 4  -  Focus sull'area di interesse per l'analisi
#  STEP 5 -   Classificazione della vegetazione pre-incendio
#  STEP 6  -  Calcolo degli indici NDVI e NBR
#  STEP 7  -  Classificazione del deltaNBR
#  STEP 8  -  Costruzione e plot di un dataset con i dati ottenuti



###################################################################################################
#   STEP 1  -  Richiamare le library necessarie al codice e definire la working directory  #
###################################################################################################

library(raster) # pacchetto con funzioni per elaborare file raster
library(RStoolbox) # pacchetto con funzioni per processare le immagini (tra cui unsuperClass)
library(ggplot2) # pacchetto con diverse funzioni per creare e modificare grafici
library(grid)  
library(gridExtra) # pacchetto con funzioni per lavorare con grafici (tra cui grid.arrange)
#library(rgdal)
library(wesanderson) # pacchetto con diverse palette di colori ispirate a Wes Anderson
#library(RColorBrewer)
library(ggpubr)

setwd("C:/lab/esame_sardegna/")  # definire la working directory



##################################################################################
#   STEP 2  -  Scelta delle immagini su cui effettuare l'analisi e importazione  #
##################################################################################

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

# Richiamando le variabili leggo i nomi e la posizione dei layer:
july10 
#names: july10_B01, july10_B02, july10_B03, july10_B04, july10_B05, july10_B06, july10_B07, july10_B08, july10_B09, july10_B11, july10_B12, july10_B8A, july10_TC 
july25
#names: july25_B01, july25_B02, july25_B03, july25_B04, july25_B05, july25_B06, july25_B07, july25_B08, july25_B09, july25_B11, july25_B12, july25_B8A, july25_TC



############################################################
#   STEP 3  -  Elaborazione e osservazione delle immagini  #
############################################################

## PLOT1 - Le due immagini (10 e 25 Luglio) in RGB veri colori: r=red, g=green, b=blue

p1<-ggRGB(july10, 4, 3, 2, stretch="lin", quantiles = c(0.001, 0.999)) + #monto le bande in RGB in veri colori e modificando i quantili regolo lo stretch della foto
    ggtitle("10 Luglio 2021") +     # titolo dell'immagine
    xlab("Long") + ylab("Lat") +    #titoli degli assi
    theme(panel.background = element_blank(), plot.title = element_text(size=11, face="bold", color="red"), 
          axis.title=element_text(size=10), axis.text= element_text(size=8))   # modifiche agli elementi del grafico (sfondo, titoli e valori degli assi)
        
p2<-ggRGB(july25, 4, 3, 2, stretch="hist") + 
    ggtitle("25 Luglio 2021") +     
    xlab("Long") + ylab("Lat") +
    theme(panel.background = element_blank(), plot.title = element_text(size=11, face="bold", color="red"), 
          axis.title=element_text(size=10), axis.text= element_text(size=8))

grid.arrange(p1, p2, nrow = 2, top=grid.text("Immagini in veri colori", gp=gpar(fontsize=15,font=2)))     
# con la funzione grid.arrange plotto le due immagini insieme in un unico grafico aggiungendo un titolo


## PLOT2 - Le due immagini (10 e 25 Luglio) in falsi colori: r=NIR, g=green, b=blue
# Questa modalità di visualizzazione esalta in rosso la vegetazione, che ha un'alta riflettanza nella banda del NIR, con valori maggiori per le aree boschive 
# e minori per le aree a prevalenza di vegetazione erbacea 

p3<-ggRGB(july10, 8, 3, 2, stretch="lin", quantiles = c(0.001, 0.999)) + #monto le bande in RGB in falsi colori e modificando i quantili regolo lo stretch della foto
    ggtitle("10 Luglio 2021") +     # titolo dell'immagine
    xlab("Long") + ylab("Lat") +    #titoli degli assi
    theme(panel.background = element_blank(), plot.title = element_text(size=11, face="bold", color="red"), 
          axis.title=element_text(size=10), axis.text= element_text(size=8))   # modifiche agli elementi del grafico (sfondo, titoli e valori degli assi)
        
p4<-ggRGB(july25, 8, 3, 2, stretch="hist") + 
    ggtitle("25 Luglio 2021") +     
    xlab("Long") + ylab("Lat") +
    theme(panel.background = element_blank(), plot.title = element_text(size=11, face="bold", color="red"), 
          axis.title=element_text(size=10), axis.text= element_text(size=8))

grid.arrange(p3, p4, nrow = 2, top=grid.text("Immagini in falsi colori", gp=gpar(fontsize=15,font=2))) 
# il PLOT2 mostra come il 25 Luglio si sia formata una grossa area scura dove la vegetazione è scomparsa a causa degli incendi


## PLOT3 - Le due immagini (10 e 25 Luglio) in falsi colori: r=SWIR(B12), g=SWIR(B11), b=red
# Questa modalità di visualizzazione esalta in rosso le aree bruciate, poichè esse hanno un'alta riflettanza nella banda del SWIR

p5<-ggRGB(july10, 11, 10, 4, stretch="lin", quantiles = c(0.001, 0.999)) + #monto le bande in RGB in falsi colori e modificando i quantili regolo lo stretch della foto
    ggtitle("10 Luglio 2021") +     # titolo dell'immagine
    xlab("Long") + ylab("Lat") +    #titoli degli assi
    theme(panel.background = element_blank(), plot.title = element_text(size=11, face="bold", color="red"), 
          axis.title=element_text(size=10), axis.text= element_text(size=8))   # modifiche agli elementi del grafico (sfondo, titoli e valori degli assi)
        
p6<-ggRGB(july25, 11, 10, 4, stretch="hist") + 
    ggtitle("25 Luglio 2021") +     
    xlab("Long") + ylab("Lat") +
    theme(panel.background = element_blank(), plot.title = element_text(size=11, face="bold", color="red"), 
          axis.title=element_text(size=10), axis.text= element_text(size=8))

grid.arrange(p5, p6, nrow = 2, top=grid.text("Immagini in falsi colori", gp=gpar(fontsize=15,font=2)))
# il PLOT3 evidenzia in rosso l'area bruciata e le zone in essa più colpite

############################################################
#   STEP 4  -  Focus sull'area di interesse per l'analisi  #
############################################################

## Per analizzare meglio l'area bruciata dagli incendi mi focalizzo su un'area più stretta.
## In questo modo limito le interferenze che la presenza del mare e delle nuvole possono avere sull'analisi

p6
# e <- drawExtent(show=TRUE, col="red") # con la funzione drawExtent disegno un riquadro sull'immagine p6 aperta e genero un oggetto extent associato alla variabile e
e   # richiamando la variabile e ottengo le sue informazioni:
    # class      : Extent 
    # xmin       : 943072.7 
    # xmax       : 965034.5 
    # ymin       : 4882167 
    # ymax       : 4905410
e<-extent(943072.7, 965034.5, 4882167, 4905410)  # CANCELLARE PRIMA DI ESAME

july25_crop<- crop(july25, e)  # con la funzione crop ritaglio le immagini nelle dimensioni definite dalla variabile e
july10_crop<- crop(july10, e)

## PLOT4 - Le due immagini ritagliate, in falsi colori

p7<-ggRGB(july10_crop, 11, 10, 4, stretch="lin", quantiles = c(0.001, 0.999)) + #monto le bande in RGB in falsi colori e modificando i quantili regolo lo stretch della foto
    ggtitle("10 Luglio 2021") +     # titolo dell'immagine
    xlab("Long") + ylab("Lat") +    #titoli degli assi
    theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", color="red"), 
          axis.title=element_text(size=10), axis.text= element_text(size=8))   # modifiche agli elementi del grafico (sfondo, titoli e valori degli assi)
        
p8<-ggRGB(july25_crop, 11, 10, 4, stretch="lin") + 
    ggtitle("25 Luglio 2021") +     
    xlab("Long") + ylab("Lat") +
    theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", color="red"), 
          axis.title=element_text(size=10), axis.text= element_text(size=8))

grid.arrange(p7, p8, ncol = 2, top=grid.text("Immagini in falsi colori", gp=gpar(fontsize=18,font=2)))


##############################################################
#   STEP 5 - Classificazione della vegetazione pre-incendio  #
##############################################################

# Applicando una classificazione sull'mmagine del 10 Luglio definisco due tipi di vegetazione, una con una maggiore riflettanza nel NIR (aree boschive) e
# una con minore riflettanza (aree coltivate o a vegetazione erbacea).

set.seed(60)  # la funzione set.seed mi permette di poter replicare più volte lo stesso processo che altrimenti sarebbe sempre randomico
july10_c2<-unsuperClass(july10_crop, nClasses=2)

## PLOT5 - La mappa di classificazione pre-incendio

p9 <-ggplot(july10_c2$map, aes(x,y)) +
     geom_raster(aes(fill=factor(layer))) +
     scale_fill_manual(values=c('green', 'darkgreen'), name=("Copertura"), labels=c("Coltivazioni", "Boschiva")) +
     ggtitle("Classificazione copertura vegetale pre-incendi") +     # titolo dell'immagine
     xlab("Long") + ylab("Lat") +    #titoli degli assi
     theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold",  hjust=0.5), 
           axis.title=element_text(size=10), axis.text= element_text(size=8),
           legend.title = element_text(size=10, face="bold"),
           legend.text = element_text(size = 10))
p9

# La mappa di classificazione mostra un'area prevalentemente boschiva (C2) nella zona SE e una dominata da aree coltivate e vegetazione erbacea (C1) nella zona W e NW

## Ottenuta la classificazione è possibile calcolare le percentuali di area che rientrano nelle diverse classi

freq(july10_c2$map)  # con la funzione freq ricavo in numero di pixel che ricade in ogni classe
#  value   count
#    1     428796
#    2     519096

pxtot<- 428796 + 519096
perc_c<-freq(july10_c2$map)/pxtot   # normalizzo il numero di pixel di ogni classe per il numero totale dei pixel per ottenere i valori percentuali
perc_c
#  value   count
#   1    0.452368
#   2    0.547632

# Per cui approssimando si ha:
# C1 (aree coltivate e vegetazione erbacea): 45.24%
# C2 (aree boschive): 54.76%

## Conoscendo la risoluzione dell'immagine è possibile quantificare le aree classificate

july10_c2$map # richiamando la variabile della mappa ottengo le informazioni sulla risoluzione lungo x e y    
            # x: 23.20628 m
            # y: 23.21091 m
            
# L'area di un pixel quindi è 
# 23.20628 * 23.21091 = 538.6389 m2
# L'area totale dell'immagine è
# 538.6389 * pxtot = 510571504 m2 = 510.571504 km2

# Inserisco questi dati in un dataset

Classi_<-c("C1","C2") # alla variabile Classi associo i nomi delle classi ottenute
Copertura<- c( "Coltivazioni", "Boschiva")  # alla variabile Danno associo una descrizione qualitativa del danno sulla base dei valori di deltaNBR (più alti per danni maggiori)
Area_perc_cop<-c(0.4524, 0.5476)  # alla variabile Area_percentuale associo i valori ricavati precedentemente
Area_km2_cop<-Area_perc_cop*510.571504  # moltiplicando l'area percentuale per l'area totale in km2 ottengo le aree in km2 che rientrano nelle 4 classi

perc_cop<-data.frame(Classi_, Copertura, Area_perc_cop, Area_km2_cop)  # con la funzione data.frame inserisco le variabili all'interno di un dataset che associo alla variabile percent
perc_cop

## PLOT6 - Grafico a barre dei dati presenti nel dataframe
g1<-ggplot(perc_cop, aes(x=Classi_, y=Area_km2_cop)) + 
    geom_bar(stat="identity", width=0.5, (aes(fill = Copertura))) +
    ggtitle("Aree di copertura vegetale pre-incendi") + 
    xlab("Classi") + ylab("Area (km2)") +
    theme(plot.title = element_text(size=16, face="bold",  hjust=0.5), 
           axis.title=element_text(size=10), axis.text= element_text(size=10),
           legend.title = element_text(size=12, face="bold"),
           legend.text = element_text(size = 10))
g1

# con la funzione ggplot creo un grafico a barre che mostra 
# le aree e le classi che rientrano nei danni



################################################
#   STEP 6  -  Calcolo degli indici NDVI e NBR #
################################################

# Per analizzare l'area incendiata è utile calcolare due indici di vegetazione: NDVI e NBR

# L'indice NDVI è utilizzato per descrivere lo stato fisiologico della vegetazione e si basa sulla firma spettrale dei vegetali, che quando
# sono in salute mostrano un picco di riflettanza nel NIR e assorbimento nel RED.
# La sua variazione spaziale permette di ricoscere aree con diversi tipi di vegetazione: valori maggiori per le aree boschive 
# e minori per quelle coltivate o a vegetazione prevalentemente erbacea.
# La sua variazione temporale permette invece, in questo caso specifico, di riconoscere il danno subito dalla vegetazione dopo l'incendio: 
# superfici maggiormente danneggiate avranno infatti un calo più drastico del valore di NDVI.
## NDVI= (NIR-RED)/(NIR+RED)

# L'indice NBR è utilizzato per mappare la severità delle aree incendiate e si basa anch'esso sulla caratteristica firma spettrale dei vegetali: una vegetazione 
# in normale stato di salute mostra un picco di riflettanza nel NIR e un valore invece debole nello SWIR, mentre a seguito di un incendio, e quindi di una 
# perdita del materiale fotosintetizzante, la riflettanza nel NIR cala drasticamente e si ha un netto aumento di riflettanza nello SWIR.
# Nel calcolo nel NBR si usa una banda di SWIR con lunghezze d'onda comprese tra  2080 e 2350, che nel caso di Sentinel.2 è la B12.
## NBR=  (NIR-SWIR)/(NIR+SWIR)

# Le bande necessarie a calcolare gli indici e i rispettivi layer nelle due immagini sono quindi:

#  BANDA   NOME LAYER JULY10   NOME LAYER JULY25
# NIR=B08     july10_B08          july25_B08
# RED=B04     july10_B04          july25_B04
# SWIR=B12    july10_B12          july25_B12


## Calcolo del NDVI

NDVI_july10<-(july10_crop$july10_B08-july10_crop$july10_B04)/(july10_crop$july10_B08+july10_crop$july10_B04) # NDVI=(NIR-RED)/(NIR+RED)
NDVI_july25<-(july25_crop$july25_B08-july25_crop$july25_B04)/(july25_crop$july25_B08+july25_crop$july25_B04) # con il $ scelgo il layer che mi serve all'interno del RasterStack

NDVI_july10
# -0.3337898, 0.9993358  (min, max)

NDVI_july25
#-0.3251454, 0.7381541  (min, max)

# Per definire una scala di colore comune da plottare sui due grafici impongo un range -0.3251454, 0.9993358 per entrambi e poi plotto una legenda comune

## PLOT7 - Confronto tra NDVI del 10 Luglio e NDVI del 25 luglio
clz<-wes_palette("Zissou1", 100, type = c("continuous"))

p10 <-ggplot(NDVI_july10, aes(x,y)) +
     geom_raster(aes(fill=layer)) + 
     scale_fill_gradientn(colors = clz, limits = c(-0.35, 1), 
                          breaks = c(0, 0.25, 0.5, 0.75, 1), labels=c(0, 0.25, 0.5, 0.75, 1))  +
     guides(fill = guide_colourbar(barwidth= 15)) +
     ggtitle("NDVI 10 Luglio 2021") +     # titolo dell'immagine
     xlab("Long") + ylab("Lat") +    #titoli degli assi
     theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", hjust=0.5), 
          axis.title=element_text(size=10), axis.text= element_text(size=8),
          legend.title = element_blank())
             # modifiche agli elementi del grafico (sfondo, titoli e valori degli assi)
        
p11 <-ggplot(NDVI_july25, aes(x,y)) +
     geom_raster(aes(fill=layer)) + 
     scale_fill_gradientn(colors = clz, limits = c(-0.35, 1), 
                          breaks = c(0, 0.25, 0.5, 0.75, 1), labels=c(0, 0.25, 0.5, 0.75, 1))  +
     guides(fill = guide_colourbar(barwidth= 15)) +
     ggtitle(" NDVI 25 Luglio 2021") +     # titolo dell'immagine
     xlab("Long") + ylab("Lat") +    #titoli degli assi
     theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", hjust=0.5), 
          axis.title=element_text(size=10), axis.text= element_text(size=8),
          legend.title = element_blank())
             # modifiche agli elementi del grafico (sfondo, titoli e valori degli assi)

ggarrange(p10, p11, ncol = 2, common.legend=TRUE, legend="bottom")
# Da questo plot si osserva bene il calo drastico di NDVI nella zona colpita dagli incendi


## Calcolando la differenza tra i due NDVI se ne può quantificare il calo
deltaNDVI<- NDVI_july10 - NDVI_july25  

## PLOT8 - Il deltaNDVI 

p12<-ggplot(deltaNDVI, aes(x,y)) +
     geom_raster(aes(fill=layer)) + 
     scale_fill_gradientn(colors = clz, limits = c(0, 1)) +   
     ggtitle("deltaNDVI") +     # titolo dell'immagine
     xlab("Long") + ylab("Lat") +    #titoli degli assi
     theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", hjust=0.5), 
          axis.title=element_text(size=10), axis.text= element_text(size=8),
          legend.title = element_blank())
p12
# Tralasciando le due zone a NW e NE dove vi è una interferenza con le nuvole, dal plot si osserva come all'interno dell'area incendiata il
# calo di NDVI più drastico sia nella porziona a sud, corrispondente alla zona boschiva
           


## Calcolo del NBR

NBR_july10<-(july10_crop$july10_B08-july10_crop$july10_B12)/(july10_crop$july10_B08+july10_crop$july10_B12)  # NBR= (NIR-SWIR)/(NIR+SWIR)
NBR_july25<-(july25_crop$july25_B08-july25_crop$july25_B12)/(july25_crop$july25_B08+july25_crop$july25_B12)

NBR_july10
#-0.4545455, 0.843633  (min, max)

NBR_july25
#-0.8563125, 0.7879109  (min, max)

# Per definire una scala di colore comune da plottare sui due grafici impongo un range -0.4545455, 0.843633 per entrambi e poi plotto una legenda comune

## PLOT9 - Confronto tra NBR del 10 Luglio e NBR del 25 luglio

p13 <-ggplot(NBR_july10, aes(x,y)) +
     geom_raster(aes(fill=layer)) + 
     scale_fill_gradientn(colors = clz, limits = c(-0.46, 0.85), 
                          breaks = c(-0.45, -0.2, 0.05, 0.3, 0.55, 0.8))  +
     guides(fill = guide_colourbar(barwidth= 15)) +
     ggtitle("NBR 10 Luglio 2021") +     # titolo dell'immagine
     xlab("Long") + ylab("Lat") +    #titoli degli assi
     theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", hjust=0.5), 
          axis.title=element_text(size=10), axis.text= element_text(size=8),
          legend.title = element_blank())
             # modifiche agli elementi del grafico (sfondo, titoli e valori degli assi)
        
p14 <-ggplot(NBR_july25, aes(x,y)) +
     geom_raster(aes(fill=layer)) + 
     scale_fill_gradientn(colors = clz, limits = c(-0.46, 0.85), 
                          breaks = c(-0.45, -0.2, 0.05, 0.3, 0.55, 0.8))  +
     guides(fill = guide_colourbar(barwidth= 15)) +
     ggtitle(" NBR 25 Luglio 2021") +     # titolo dell'immagine
     xlab("Long") + ylab("Lat") +    #titoli degli assi
     theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", hjust=0.5), 
          axis.title=element_text(size=10), axis.text= element_text(size=8),
          legend.title = element_blank())
             # modifiche agli elementi del grafico (sfondo, titoli e valori degli assi)

ggarrange(p13, p14, ncol = 2, common.legend=TRUE, legend="bottom")
# Anche in questo caso il calo di NBR nella seconda foto indica un aumento della riflettanza nello SWIR simultaneamente a un calo nel NIR dovuti all'incendio


## Calcolando la differenza tra i due NBR se ne può quantificare il calo dopo gli incendi
deltaNBR<- NBR_july10 - NBR_july25

## PLOT10 - Il deltaNBR

p15<-ggplot(deltaNBR, aes(x,y)) +
     geom_raster(aes(fill=layer)) + 
     scale_fill_gradientn(colors = clz, limits = c(0, 1)) +   
     ggtitle("deltaNBR") +     # titolo dell'immagine
     xlab("Long") + ylab("Lat") +    #titoli degli assi
     theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", hjust=0.5), 
          axis.title=element_text(size=10), axis.text= element_text(size=8),
          legend.title = element_blank())
p15
# Anche in questo caso i valori maggiori di delta NBR sono nella porsione sud dell'area incendiata, ovvero la zona boschiva, dove il danno è quindi stato maggiore

## PLOT11 - Confronto tra il deltNDVI e il deltaNBR
ggarrange(p12, p15, ncol = 2, common.legend=TRUE, legend="bottom")


#############################################
#  STEP 7  -  Classificazione del deltaNBR  #
#############################################

# Per riconoscere e quantificare le aree più danneggiate dagli incendi applico una classificazione dell'indice deltaNBR 

set.seed(60)  # la funzione set.seed mi permette di poter replicare più volte lo stesso processo che altrimenti sarebbe sempre randomico ???
dNBR_c4<-unsuperClass(deltaNBR, nClasses=4) # con la funzione unsuperClass applico una classificazione non supervisionata di 4 classi e la associo alla variabile dNBR_c4

## PLOT12 - La mappa di classificazione del deltaNBR
p16 <-ggplot(dNBR_c4$map, aes(x,y)) +
     geom_raster(aes(fill=factor(layer))) +
     scale_fill_manual(values=c('yellow','red','darkgreen','green'), name=("Severità di danno"), labels=c("Alta", "Intermedia", "Bassa o Nulla", "Bassa o Nulla")) +
     ggtitle("Mappa di classificazione del deltaBNR") +     # titolo dell'immagine
     xlab("Long") + ylab("Lat") +    #titoli degli assi
     theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold",  hjust=0.5), 
           axis.title=element_text(size=10), axis.text= element_text(size=8),
           legend.title = element_text(size=10, face="bold"),
           legend.text = element_text(size = 10))
p16

# La mappa di classificazione mette in evidenza due zone riconducibili alle aree non coinvolte dagli incendi (C3 e C4) e altre due alle zone invece colpite, con due 
# diversi gradi di severità (C1, con i maggiori valori di deltaNBR, e C2). Anche in questo caso bisogna tenere conto di alcune interferenze legate alla nuvolosità.

## PLOT13 - Confronto tra il deltaNBR e la sua classificazione 
grid.arrange(p15, p16, ncol = 2)

## PLOT14 - Confronto tra classificazione pre-incendio e classificazione deltaNBR
grid.arrange(p9, p16, ncol = 2)

# Confrontando le due mappe di classificazione si osserva come il grado di severità maggiore di danno (C1) sono prevalenti nella porzione corrispondente alle aree boschive, 
# dove il valore di riflettanza nel NIR di partenza era maggiore e quindi il calo di NBR è stato più drastico

## PLOT15 - Confronto tra l'immagine in falsi colori e la classificazione di deltaNBR
grid.arrange(p8, p16, ncol = 2)


## Ottenuta la classificazione è possibile calcolare le percentuali di area che rientrano nelle diverse classi

freq(dNBR_c4$map)  # con la funzione freq ricavo in numero di pixel che ricade in ogni classe
#  value   count
#    1     101603
#    2     236700
#    3     364891
#    4     244698

perc_d<-freq(dNBR_c4$map)/pxtot   # normalizzo il numero di pixel di ogni classe per il numero totale dei pixel per ottenere i valori percentuali
perc_d
#  value   count
#   1    0.1071884
#   2    0.2497120
#   3    0.3849500
#   4    0.2581497

# Per cui approssimando si ha:
# C1: 10.72%  --> Severità alta
# C2: 24.97%  --> Severità intermedia
# C3: 38.50%  --> Severità bassa o nulla
# C4: 25.81%  --> Severità bassa o nulla

## Conoscendo la risoluzione dell'immagine è possibile quantificare le aree di territorio coinvolte dall'incendio
# Area dell'immagine= 510.571504 km2 
# (Vedi procedimento per il calcolo dell'area alla riga 235)


#####################################################################
#  STEP 8  -  Costruzione e plot di un dataset con i dati ottenuti  #
#####################################################################

## Ottenuti i dati di interesse li inserisco in un dataset

Classi_deltaNBR<-c("C1","C2", "C3", "C4") # alla variabile Classi associo i nomi delle classi ottenute
Danno<- c( "Alto", "Intermedio", "Basso o Nullo", "Basso o Nullo")  # alla variabile Danno associo una descrizione qualitativa del danno sulla base dei valori di deltaNBR (più alti per danni maggiori)
Area_perc_dan<-c(0.1072, 0.2497, 0.3850, 0.2581)  # alla variabile Area_percentuale associo i valori ricavati precedentemente
Area_km2_dan<-Area_perc_dan*510.571504  # moltiplicando l'area percentuale per l'area totale in km2 ottengo le aree in km2 che rientrano nelle 4 classi

perc_dan<-data.frame(Classi_deltaNBR, Danno, Area_perc_dan, Area_km2_dan)  # con la funzione data.frame inserisco le variabili all'interno di un dataset che associo alla variabile percent
perc_dan

## PLOT16 - Grafico a barre dei dati presenti nel dataframe

g2<-ggplot(perc_dan, aes(x=factor(Danno,level = c("Alto", "Intermedio", "Basso o Nullo")), y=Area_km2_dan)) + 
    geom_bar(stat="identity", width=0.5, (aes(fill = Danno))) +
    ggtitle("Aree danneggiate dall'incendio") + 
    xlab("") + ylab("Area (km2)") +
    theme(plot.title = element_text(size=16, face="bold",  hjust=0.5), 
           axis.title=element_text(size=10), axis.text= element_text(size=10),
          axis.text.x=element_blank(),
           axis.ticks.x=element_blank(),
           legend.title = element_text(size=12, face="bold"),
           legend.text = element_text(size = 10))
g2

 # con la funzione ggplot creo un grafico a barre che mostra le aree risultano danneggiate dall'incendio





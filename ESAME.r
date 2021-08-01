################################################################################################################################################################################
###############################################################                                            #####################################################################
###############################################################   ESAME DI TELERILEVAMENTO GEO-ECOLOGICO   #####################################################################
###############################################################               10 Agosto 2021               #####################################################################
###############################################################                Laura Landi                 #####################################################################
###############################################################                                            #####################################################################
################################################################################################################################################################################



#                             INDICE
#
#  STEP 1  -  Richiamare tutte le library necessarie al codice e definire la working directiory
#  STEP 2  -  Scegliere le immagini su cui effettuare l'analisi e importarle
#  STEP 3  -  Elaborazione e osservazione delle immagini
#  STEP 4  -  Focus sull'area di interesse per l'analisi
#  STEP 5  -  Calcolo degli indici NDVI e NBR
#  STEP 6  -  Classificazione del deltaNBR
#  STEP 7  -  Costruzione di un dataset con i dati ottenuti



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

# Richiamando le variabili leggo i nomi e la posizione dei layer:
july10 
#names: july10_B01, july10_B02, july10_B03, july10_B04, july10_B05, july10_B06, july10_B07, july10_B08, july10_B09, july10_B11, july10_B12, july10_B8A, july10_TC 
july25
#names: july25_B01, july25_B02, july25_B03, july25_B04, july25_B05, july25_B06, july25_B07, july25_B08, july25_B09, july25_B11, july25_B12, july25_B8A, july25_TC



############################################################
#   STEP 3  -  Elaborazione e osservazione delle immagini  #
############################################################

## PLOT1 - Le due immagini (10 e 25 Luglio) in RGB veri colori: r=red, g=green, b=blue
p1<-ggRGB(july10, 4, 3, 2, stretch="lin", quantiles = c(0.001, 0.999))  # con la funzione ggRGB monto le bande in RGB (veri colori) e le associo alle variabili p1 e p2 (modificando i quantili regolo lo stretch della foto)
p2<-ggRGB(july25, 4, 3, 2, stretch="hist")
grid.arrange(p1, p2, nrow = 2)     # con la funzione grid.arrange plotto le due immagini insieme in un unico grafico

## PLOT2 - Le due immagini (10 e 25 Luglio) in falsi colori: r=NIR, g=green, b=blue
# Questa modalità di visualizzazione esalta in rosso la vegetazione, che ha un'alta riflettanza nella banda del NIR, con valori maggiori per le aree boschive 
# e minori per le aree a prevalenza di vegetazione erbacea 
p3<-ggRGB(july10, 8, 3, 2, stretch="lin", quantiles = c(0.001, 0.999))
p4<-ggRGB(july25, 8, 3, 2, stretch="hist")
grid.arrange(p3, p4, nrow = 2)
# il PLOT2 mostra come il 25 Luglio si sia formata una grossa area scura dove la vegetazione è scomparsa a causa degli incendi

## PLOT3 - Le due immagini (10 e 25 Luglio) in falsi colori: r=SWIR(B12), g=SWIR(B11), b=red
# Questa modalità di visualizzazione esalta in rosso le aree bruciate, poichè esse hanno un'alta riflettanza nella banda del SWIR
p5<-ggRGB(july10, 11, 10, 4, stretch="lin", quantiles = c(0.001, 0.999))
p6<-ggRGB(july25, 11, 10, 4, stretch="hist")
grid.arrange(p5, p6, nrow = 2)
# il PLOT3 evidenzia in rosso l'area bruciata e le zone in essa più colpite


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
p7<-ggRGB(july10_crop, 11, 10, 4, stretch="lin", quantiles = c(0.0001, 0.9999))
p8<-ggRGB(july25_crop, 11, 10, 4, stretch="lin")
grid.arrange(p6, p7, ncol = 2)



##############################################################
#   STEP 5 - Classificazione della vegetazione pre-incendio  #
##############################################################

# Applicando una classificazione sull'mmagine del 10 Luglio definisco due tipi di vegetazione, una con una maggiore riflettanza nel NIR (aree boschive) e
# una con minore riflettanza (aree coltivate o a vegetazione erbacea).

set.seed(60)  # la funzione set.seed mi permette di poter replicare più volte lo stesso processo che altrimenti sarebbe sempre randomico
july10_c2<-unsuperClass(july10_crop, nClasses=2)

## PLOT5 - La mappa di classificazione pre-incendio
clc2 <- colorRampPalette(c('green', 'darkgreen'))(2) # definisco una palette con 2 colori per le classi
plot(july10_c2$map, col = clc2, legend = FALSE, axes = FALSE, box = FALSE)
legend(965031,4905419, legend = paste0("C",1:2), fill = clc2,
       title = "Classi", horiz = FALSE,  bty = "n")

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

## PLOT13 - Grafico a barre dei dati presenti nel dataframe
g1<-ggplot(perc_cop, aes(x=Classi_, y=Area_km2_cop)) + geom_bar(stat="identity", width=0.2, (aes(fill = Copertura))) # con la funzione ggplot creo un grafico a barre che mostra 
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

## PLOT6 - Confronto tra NDVI del 10 Luglio e NDVI del 25 luglio
clz<-wes_palette("Zissou1", 100, type = c("continuous"))  #associo alla variabile clz una palette di colori dal pacchetto wesanderson
par(mfrow=c(1,2))  # con la funzione par plotto in unico grafico le due immagini
plot(NDVI_july10, col=clz, main="NDVI 10 Luglio", box=FALSE)  # plot del raster NDVI calcolato con la palette di colori scelta
plot(NDVI_july25, col=clz, main="NDVI 25 Luglio", box= FALSE)
# Da questo plot si osserva bene il calo drastico di NDVI nella zona colpita dagli incendi

## Calcolando la differenza tra i due NDVI se ne può quantificare il calo
deltaNDVI<- NDVI_july10 - NDVI_july25  

## PLOT7 - Il deltaNDVI 
cld <- colorRampPalette(c('blue','white','red'))(100) # definisco una palette che associa al rosso i valori di differenza maggiori, ovvero il calo di NDVI maggiore
plot(deltaNDVI, col=cld, main="differenza NDVI") ## <-- controllare come forzare a zero il limite della legenda
# Tralasciando le due zone a NW e NE dove vi è una interferenza con le nuvole, dal plot si osserva come all'interno dell'area incendiata il
# calo di NDVI più drastico sia nella porziona a sud, corrispondente alla zona boschiva


## Calcolo del NBR

NBR_july10<-(july10_crop$july10_B08-july10_crop$july10_B12)/(july10_crop$july10_B08+july10_crop$july10_B12)  # NBR= (NIR-SWIR)/(NIR+SWIR)
NBR_july25<-(july25_crop$july25_B08-july25_crop$july25_B12)/(july25_crop$july25_B08+july25_crop$july25_B12)

## PLOT8 - Confronto tra NBR del 10 Luglio e NBR del 25 luglio
par(mfrow=c(1,2))
plot(NBR_july10, col=clz, main="NBR 10 Luglio")
plot(NBR_july25, col=clz, main="NBR 25 Luglio")
# Anche in questo caso il calo di NBR nella seconda foto indica un aumento della riflettanza nello SWIR simultaneamente a un calo nel NIR dovuti all'incendio

## Calcolando la differenza tra i due NBR se ne può quantificare il calo dopo gli incendi
deltaNBR<- NBR_july10 - NBR_july25

## PLOT9 - Il deltaNBR
plot(deltaNBR, col=cld, main="differenza NBR") ## <-- controllare come forzare a zero il limite della legenda
# Anche in questo caso i valori maggiori di delta NBR sono nella porsione sud dell'area incendiata, ovvero la zona boschiva, dove il danno è quindi stato maggiore

## PLOT10 - Confronto tra il deltNDVI e il deltaNBR
par(mfrow=c(1,2))
plot(deltaNDVI, col=cld, main="differenza NDVI")
plot(deltaNBR, col=cld, main="differenza NBR")



#############################################
#  STEP 7  -  Classificazione del deltaNBR  #
#############################################

# Per riconoscere e quantificare le aree più danneggiate dagli incendi applico una classificazione dell'indice deltaNBR 

set.seed(60)  # la funzione set.seed mi permette di poter replicare più volte lo stesso processo che altrimenti sarebbe sempre randomico ???
dNBR_c4<-unsuperClass(deltaNBR, nClasses=4) # con la funzione unsuperClass applico una classificazione non supervisionata di 4 classi e la associo alla variabile dNBR_c4

## PLOT11 - La mappa di classificazione del deltaNBR
clc <- colorRampPalette(c('yellow','red','darkgreen','green'))(4) # definisco una palette con 4 colori per le classi
plot(dNBR_c4$map, col = clc, legend = FALSE, axes = FALSE, box = FALSE)
legend(965031,4905419, legend = paste0("C",1:4), fill = clc,
       title = "Classi", horiz = FALSE,  bty = "n")

# La mappa di classificazione mette in evidenza due zone riconducibili alle aree non coinvolte dagli incendi (C3 e C4) e altre due alle zone invece colpite, con due 
# diversi gradi di severità (C1, con i maggiori valori di deltaNBR, e C2). Anche in questo caso bisogna tenere conto di alcune interferenze legate alla nuvolosità.

## PLOT12 - Confronto tra il deltaNBR e la sua classificazione 
par(mfrow=c(1,2))
plot(deltaNBR, col=cld, main="differenza NBR")
plot(dNBR_c4$map, col = clc, legend = FALSE, axes = FALSE, box = FALSE, main="Classificazione deltaNBR")
legend(965031,4905419, legend = paste0("C",1:4), fill = clc,
      title = "Classi", horiz = FALSE,  bty = "n")

## PLOT13 - Confronto tra classificazione pre-incendio e classificazione deltaNBR
par(mfrow=c(1,2))
plot(july10_c2$map, col = clc2, legend = FALSE, axes = FALSE, box = FALSE, main="Classi di vegetazione pre-incendio")
legend(965031,4905419, legend = paste0("C",1:2), fill = clc2,
       title = "Classi", horiz = FALSE,  bty = "n")
plot(dNBR_c4$map, col = clc, legend = FALSE, axes = FALSE, box = FALSE, main="Zone di severità dei danni da incendio")
legend(965031,4905419, legend = paste0("C",1:4), fill = clc,
       title = "Classi", horiz = FALSE,  bty = "n")

# Confrontando le due mappe di classificazione si osserva come il grado di severità maggiore di danno (C1) sono prevalenti nella porzione corrispondente alle aree boschive, 
# dove il valore di riflettanza nel NIR di partenza era maggiore e quindi il calo di NBR è stato più drastico


dNBR_c4$map # richiamando la variabile della mappa di classificazione ottengo informazioni sulla sua estensione:                          
            # xmin 943077.6
            # xmax 965030.7
            # ymin 4882162
            # ymax 4905419
            # In base alle quali posiziono la legenda:
            # x 965031
            # y 4905419



## PLOT14 - Confronto tra l'immagine in falsi colori e la classificazione di deltaNBR
par(mfrow=c(1,2))
plotRGB(july25_crop, 11, 10, 4, stretch="lin")
plot(dNBR_c4$map, col = clc, legend = FALSE, axes = FALSE, box = FALSE, main="Aree di severità di danno")
legend(965031,4905419, legend = paste0("C",1:4), fill = clc,
      title = "Classi", horiz = FALSE,  bty = "n")


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
# C1: 10.72%
# C2: 24.97%
# C3: 38.50%
# C4: 25.81%

## Conoscendo la risoluzione dell'immagine è possibile calcolare le aree di territorio coinvolte dall'incendio

dNBR_c4$map # richiamando la variabile della mappa ottengo le informazioni sulla risoluzione lungo x e y    
            # x: 23.20628 m
            # y: 23.21091 m
            
# L'area di un pixel quindi è 
# 23.20628 * 23.21091 = 538.6389 m2
# L'area totale dell'immagine è
# 538.6389 * pxtot = 510571504 m2 = 510.571504 km2



##############################################################
#  STEP 7  -  Costruzione di un dataset con i dati ottenuti  #
##############################################################

## Ottenuti i dati di interesse li inserisco in un dataset

Classi_deltaNBR<-c("C1","C2", "C3", "C4") # alla variabile Classi associo i nomi delle classi ottenute
Danno<- c( "Alto", "Intermedio", "Nullo", "Nullo")  # alla variabile Danno associo una descrizione qualitativa del danno sulla base dei valori di deltaNBR (più alti per danni maggiori)
Area_perc_dan<-c(0.1072, 0.2497, 0.3850, 0.2581)  # alla variabile Area_percentuale associo i valori ricavati precedentemente
Area_km2_dan<-Area_perc_dan*510.571504  # moltiplicando l'area percentuale per l'area totale in km2 ottengo le aree in km2 che rientrano nelle 4 classi

perc_dan<-data.frame(Classi_deltaNBR, Danno, Area_perc_dan, Area_km2_dan)  # con la funzione data.frame inserisco le variabili all'interno di un dataset che associo alla variabile percent
perc_dan

## PLOT13 - Grafico a barre dei dati presenti nel dataframe
g2<-ggplot(perc_dan, aes(x=Danno,y=Area_km2_dan)) + geom_bar(stat="identity", width=0.2, (aes(fill = Danno))) # con la funzione ggplot creo un grafico a barre che mostra 
                                                                                                                                     # le aree e le classi che rientrano nei danni

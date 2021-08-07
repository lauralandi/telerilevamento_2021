                                                      #--------------------------------------------------------------------#                                                            
                                                      #   ANALISI IN REMOTE SENSING DELL'AREA DEL MONTIFERRU IN SARDEGNA   #
                                                      #                COLPITA DAGLI INCENDI DI LUGLIO 2021                #
                                                      #                                                                    #
                                                      #                           10 Agosto 2021                           #   
                                                      #                            Laura Landi                             #
                                                      #                                                                    #
                                                      #               Esame di telerilevamento geo-ecologico               #
                                                      #--------------------------------------------------------------------#


                                  #----- INDICE -----#

#  STEP 1  -  Richiamare le library necessarie al codice e definire la working directory  ......... 026
#  STEP 2  -  Scelta delle immagini su cui effettuare l'analisi e importazione  ................... 042
#  STEP 3  -  Elaborazione e osservazione delle immagini  ......................................... 094
#  STEP 4  -  Focus sull'area di interesse per l'analisi  ......................................... 156
#  STEP 5 -   Classificazione della vegetazione pre-incendio  ..................................... 194
#  STEP 6  -  Calcolo degli indici NDVI e NBR  .................................................... 277
#  STEP 7  -  Classificazione del deltaNBR  ....................................................... 439
#  STEP 8  -  Costruzione e plot di un dataset con i dati ottenuti  ............................... 505



#-------------------------------------------------------------------------------------------#
#   STEP 1  -  Richiamare le library necessarie al codice e definire la working directory   #
#-------------------------------------------------------------------------------------------#

library(raster) # pacchetto con funzioni per elaborare file raster
library(RStoolbox) # pacchetto con funzioni per processare le immagini (tra cui unsuperClass)
library(ggplot2) # pacchetto con diverse funzioni per creare e modificare grafici
library(ggpubr) # pacchetto che aggiunge ulteriori funzioni per personalizzare i grafici (tra cui ggarrange con common.legend)
library(grid)  # pacchetto che aggiunge ulteriori funzioni per personalizzare i grafici ( tra cui gpar)
library(gridExtra) # pacchetto con funzioni per lavorare con grafici (tra cui grid.arrange)
library(wesanderson) # pacchetto con diverse palette di colori ispirate a Wes Anderson
library(rasterVis) # pacchetto con funzioni aggiuntive per elaborare e plottare oggetti raster (tra cui levelplot)

setwd("C:/lab/sardegna2/")  # definire la working directory



#---------------------------------------------------------------------------------#
#   STEP 2  -  Scelta delle immagini su cui effettuare l'analisi e importazione   #
#---------------------------------------------------------------------------------#

## Le due immagini utilizzate derivano dai satelliti Sentinel-2 e rappresentano la zona del Montiferru in provincia di 
## Oristano il 10 e il 25 Luglio 2021, ovvero prima e dopo gli incendi che hanno colpito l'area in quel mese.

## Le diverse bande in cui Sentinel-2 fornisce le immagini sono:

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


## Nello specifico per questa analisi sono state sfruttate le bande:
##   B02= blue 
##   B03= green
##   B04= red  
##   B08= NIR
##   B11= SWIR
##   B12= SWIR


# Le diverse bande per entrambe le immagini sono scaricate separatamente dal sito Sentinel Hub EO Browser. 
# Per importarle come due singoli oggetti RasterStack contenenti tutte le bande seguo i passaggi:

rlist22<-list.files(pattern="july22") # con la funzione list.files creo una lista di file con le diverse bande con pattern "july10" e associo alla variabile rlist10
import22<-lapply(rlist22,raster) # con la funzione lapply applico la funzione raster su tutti i file della lista e li importo associati alla variabile import10
july22<-stack(import22) # con la funzione stack creo un unico oggetto RasterStack che contiene come layer tutte le bande importate

rlist30<-list.files(pattern="july30") # con la funzione list.files creo una lista di file con le diverse bande con pattern "july25" e associo alla variabile rlist25
import30<-lapply(rlist30,raster) # con la funzione lapply applico la funzione raster su tutti i file della lista e li importo associati alla variabile import25
july30<-stack(import30) # con la funzione stack creo un unico oggetto RasterStack che contiene come layer tutte le bande importate

cl=colorRampPalette(c("gray48", "white"))(100)  # definisco una color palette

# con la funzione levelplot visualizzo le bande importate per l'immagine del 22 Luglio
p0<-levelplot(july22,col.regions=cl,main="Bande dell'immagine 22 Luglio",       # la funzione levelplot permette di arricchire il grafico con la color palette scelta
          names.attr=c("B01","B02","B03","B04","B05","B06","B07","B08","B09","B11","B12","B8A")) # e il titolo principale e dei layer
p0

# con la funzione levelplot visualizzo le bande importate per l'immagine del 30 Luglio
p00<-levelplot(july30,col.regions=cl,main="Bande dell'immagine 30 Luglio",       # la funzione levelplot permette di arricchire il grafico la color palette scelta
          names.attr=c("B01","B02","B03","B04","B05","B06","B07","B08","B09","B11","B12","B8A"))                                # e il titolo principale e dei layer
p00

# Richiamando le due variabili posso leggere i nomi e la posizione dei layer appena importati:
july22 
# names: july22_B01, july22_B02, july22_B03, july22_B04, july22_B05, july22_B06, july22_B07, july22_B08, july22_B09, july22_B11, july22_B12, july22_B8A 
july30
# names: july30_B01, july30_B02, july30_B03, july30_B04, july30_B05, july30_B06, july30_B07, july30_B08, july30_B09, july30_B11, july30_B12, july30_B8A


## Visualizzo le due immagini (22 e 30 Luglio) in RGB veri colori (r=red, g=green, b=blue):

p1<-ggRGB(july22, 4, 3, 2 , stretch="lin") + # con ggRGB monto le bande in RGB in veri colori e applico uno stretch lineare per migliorare il contrasto
    ggtitle("22 Luglio 2021") +     # titolo dell'immagine
    xlab("Long") + ylab("Lat") +    # titoli degli assi
    theme(panel.background = element_blank(), plot.title = element_text(size=11, face="bold", color="red"), 
          axis.title=element_text(size=10), axis.text= element_text(size=8))   # con theme modifico agli elementi del grafico (sfondo, testo dei titoli e valori degli assi)
        
p2<-ggRGB(july30, 4, 3, 2 , stretch="lin") +   # con ggRGB monto le bande in RGB in veri colori e applico uno stretch lineare per migliorare il contrasto
    ggtitle("30 Luglio 2021") +   # titolo dell'immagine
    xlab("Long") + ylab("Lat") +  # titoli degli assi
    theme(panel.background = element_blank(), plot.title = element_text(size=11, face="bold", color="red"), 
          axis.title=element_text(size=10), axis.text= element_text(size=8))    # con theme modifico agli elementi del grafico (sfondo, testo dei titoli e valori degli assi)

grid.arrange(p1, p2, nrow = 2, top=grid.text("Immagini in veri colori", gp=gpar(fontsize=15,font=2)))     
# con la funzione grid.arrange plotto le due immagini insieme in un unico grafico aggiungendo un titolo



#-------------------------------------------#
#   STEP 3  -  Elaborazione delle immagini  #
#-------------------------------------------#

# Per analizzare meglio l'area bruciata dagli incendi mi focalizzo su un'area più ristretta.

plotRGB(july30, 4, 3, 2, stretch="lin")  # plotto l'immagine del 30 Luglio in RGB
e<- drawExtent(show=TRUE, col="red") # con la funzione drawExtent disegno un riquadro sull'immagine aperta e genero un oggetto extent associato alla variabile e
e   # richiamando la variabile e ottengo le sue informazioni:
    # class      : Extent 
    # xmin       : 943140.1 
    # xmax       : 967073.2 
    # ymin       : 4881607 
    # ymax       : 4906605
#e<-extent(943140.1, 967073.2, 4881607, 4906605)

july22_crop<- crop(july22, e)  # con la funzione crop ritaglio le immagini nelle dimensioni definite dalla variabile "e" e le associo a due nuove variabili
july30_crop<- crop(july30, e)


## Visualizzo le due immagini ritagliate in RGB veri colori (r=red, g=green, b=blue):

p7<-ggRGB(july22_crop, 4, 3, 2, stretch="lin") +  # con ggRGB monto le bande in falsi colori e applico uno stretch lineare per migliorare il contrasto
    ggtitle("22 Luglio 2021") +    # titolo dell'immagine 
    xlab("Long") + ylab("Lat") +    # titoli degli assi
    theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", color="red"), 
          axis.title=element_text(size=10), axis.text= element_text(size=8))   # con theme modifico agli elementi del grafico (sfondo, testo dei titoli e valori degli assi)
        
p8<-ggRGB(july30_crop, 4, 3, 2, stretch="lin") +   # con ggRGB monto le bande in falsi colori e applico uno stretch lineare per migliorare il contrasto
    ggtitle("30 Luglio 2021") +     # titolo dell'immagine
    xlab("Long") + ylab("Lat") +  # titoli degli assi
    theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", color="red"), 
          axis.title=element_text(size=10), axis.text= element_text(size=8))  # con theme modifico agli elementi del grafico (sfondo, testo dei titoli e valori degli assi)

grid.arrange(p7, p8, ncol = 2, top=grid.text("Immagini in veri colori", gp=gpar(fontsize=18,font=2)))  
# con la funzione grid.arrange plotto le due immagini insieme in un unico grafico aggiungendo un titolo


## Visualizzo le due immagini ritagliate in falsi colori (r=NIR, g=green, b=blue):

p7.1<-ggRGB(july22_crop, 8, 3, 2, stretch="lin") +  # con ggRGB monto le bande in falsi colori e applico uno stretch lineare per migliorare il contrasto
    ggtitle("22 Luglio 2021") +    # titolo dell'immagine 
    xlab("Long") + ylab("Lat") +    # titoli degli assi
    theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", color="red"), 
          axis.title=element_text(size=10), axis.text= element_text(size=8))   # con theme modifico agli elementi del grafico (sfondo, testo dei titoli e valori degli assi)
        
p8.1<-ggRGB(july30_crop, 8, 3, 2, stretch="lin") +   # con ggRGB monto le bande in falsi colori e applico uno stretch lineare per migliorare il contrasto
    ggtitle("30 Luglio 2021") +     # titolo dell'immagine
    xlab("Long") + ylab("Lat") +  # titoli degli assi
    theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", color="red"), 
          axis.title=element_text(size=10), axis.text= element_text(size=8))  # con theme modifico agli elementi del grafico (sfondo, testo dei titoli e valori degli assi)

grid.arrange(p7.1, p8.1, ncol = 2, top=grid.text("Immagini in falsi colori", gp=gpar(fontsize=18,font=2))) 
# con la funzione grid.arrange plotto le due immagini insieme in un unico grafico aggiungendo un titolo
# Questa modalità di visualizzazione esalta in rosso la vegetazione, che ha un'alta riflettanza nella banda del NIR (con valori maggiori per le aree boschive 
# e minori per le aree a prevalenza di vegetazione erbacea e coltivate) e mostra come il 30 Luglio si sia formata una grossa area scura dove la vegetazione è 
# stata distrutta a causa degli incendi


## Visualizzo le due immagini ritagliate in falsi colori (r=SWIR(B12), g=SWIR(B11), b=red):

p7.2<-ggRGB(july22_crop, 11, 10, 4, stretch="lin") +  # con ggRGB monto le bande in falsi colori e applico uno stretch lineare per migliorare il contrasto
    ggtitle("22 Luglio 2021") +    # titolo dell'immagine 
    xlab("Long") + ylab("Lat") +    # titoli degli assi
    theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", color="red"), 
          axis.title=element_text(size=10), axis.text= element_text(size=8))   # con theme modifico agli elementi del grafico (sfondo, testo dei titoli e valori degli assi)
        
p8.2<-ggRGB(july30_crop, 11, 10, 4, stretch="lin") +   # con ggRGB monto le bande in falsi colori e applico uno stretch lineare per migliorare il contrasto
    ggtitle("30 Luglio 2021") +     # titolo dell'immagine
    xlab("Long") + ylab("Lat") +  # titoli degli assi
    theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", color="red"), 
          axis.title=element_text(size=10), axis.text= element_text(size=8))  # con theme modifico agli elementi del grafico (sfondo, testo dei titoli e valori degli assi)

grid.arrange(p7.2, p8.2, ncol = 2, top=grid.text("Immagini in falsi colori", gp=gpar(fontsize=18,font=2)))  
# con la funzione grid.arrange plotto le due immagini insieme in un unico grafico aggiungendo un titolo
# Questa modalità di visualizzazione esalta in rosso le aree bruciate, poichè esse hanno un'alta riflettanza nella banda del SWIR, ed evidenza le zone più intensamente colpite



#-------------------------------------------------------------#
#   STEP 4 - Classificazione della vegetazione pre-incendio   #
#-------------------------------------------------------------#

# Applicando una classificazione sull'immagine del 22 Luglio definisco due classi che rappresentano tipi diversi di vegetazione, una con una maggiore riflettanza nel NIR 
# (aree boschive) e una con minore riflettanza (aree coltivate).

set.seed(60)  # la funzione set.seed fissa il set di pixel di partenza e permette di poter replicare più volte lo stesso risultato, 
                                                            # altrimenti il processo randomico porterebbe ogni volta qualche differenza
july22_c2<-unsuperClass(july22_crop, nClasses=2)  # con la funzione unsuperClass applico una classificazione non supervisionata con un numero di classi definito (qui 2) 
                                                    # e la associo alla variabile july22_c2

## Visualizzo la mappa di classificazione pre-incendio:

p9<-ggplot(july22_c2$map, aes(x,y)) +   # con ggplot creo il grafico che contiene la mappa (richiamata con $ all'interno dell'oggetto di output della classificazione)
     geom_raster(aes(fill=factor(layer))) +  # geom_raster permette di plottare nel grafico un elemento raster
     scale_fill_manual(values=c('darkgreen', 'green'),  # definisco i colori delle classi
                       name=("Copertura"), labels=c("Boschiva", "Coltivata")) +  # definisco nome della legenda ed etichette dei colori corrispondenti
     ggtitle("Mappa di classificazione pre-incendio") +    # titolo dell'immagine
     xlab("Long") + ylab("Lat") +    #titoli degli assi
     theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold",  hjust=0.5), 
           axis.title=element_text(size=10), axis.text= element_text(size=8),
           legend.title = element_text(size=10, face="bold"),
           legend.text = element_text(size = 10))  # con theme modifico gli elementi del grafico (sfondo, titolo, testo dei titoli e valori degli assi, 
                                                                                                   # titolo ed etichette della legenda)
p9
# La mappa di classificazione mostra un'area prevalentemente boschiva (C1) nella zona SE e una dominata da aree coltivate (C2)


## Ottenuta la classificazione è possibile calcolare le percentuali di area che rientrano nelle diverse classi

freq(july22_c2$map)  # con la funzione freq ricavo il numero di pixel che ricade in ogni classe
#  value   count
#    1     452842
#    2     658622

pxtot<- 452842 + 658622  # calcolo la somma dei pixel totali dell'immagine
perc_c<-freq(july22_c2$map)/pxtot   # normalizzo il numero di pixel di ogni classe per il numero totale per ottenere i valori proporzionali tra 0 e 1
perc_c
#  value   count
#   1    0.4074284
#   2    0.5925716

# Per cui approssimando si ha:
# C1 (aree boschive): 40.74%
# C2 (aree coltivate): 59.26%

## Conoscendo la risoluzione dell'immagine è possibile quantificare le aree classificate:

july22_c2$map # richiamando la variabile della mappa di classificazione ottengo le informazioni sulla sua risoluzione lungo x e y    
 # x: 23.20628 m
 # y: 23.21091 m
            
# L'area di un pixel quindi è 
# 23.20628 * 23.21091 = 538.6389 m2
# L'area totale dell'immagine è
# 538.6389 * pxtot = 598677746 m2 = 598.677746 km2

## Inserisco questi dati in un dataset le cui colonne saranno:
Classi_<-c("C1","C2") # alla variabile Classi associo i nomi delle classi ottenute
Copertura<- c("Boschiva", "Coltivata")  # alla variabile Copertura associo una descrizione della copertura
Area_perc_cop<-c(0.4074284, 0.5925716)  # alla variabile Area_perc_cop associo i valori percentuali ricavati precedentemente
Area_km2_cop<-Area_perc_cop*598.677746  # moltiplicando l'area percentuale per l'area totale in km2 ottengo le aree in km2 che rientrano nelle 2 classi

perc_cop<-data.frame(Classi_, Copertura, Area_perc_cop, Area_km2_cop)  # con data.frame inserisco le variabili all'interno di un dataset che associo alla variabile perc_cop
perc_cop  # visualizzo il dataframe 


## Visualizzo il grafico a barre dei dati presenti nel dataframe:

g1<-ggplot(perc_cop, aes(x=Classi_, y=Area_km2_cop)) +  # con ggplot creo un grafico che contiene i dati del dataset 
    geom_bar(stat="identity", width=0.5, (aes(fill = Copertura))) +  # geom_bar permette di creare un grafico a barre
    ggtitle("Aree di copertura vegetale pre-incendi") +  # titolo del grafico
    xlab("Classi") + ylab("Area (km2)") +  # titoli degli assi
    theme(plot.title = element_text(size=16, face="bold",  hjust=0.5), 
           axis.title=element_text(size=10), axis.text= element_text(size=10),
           legend.title = element_text(size=12, face="bold"),
           legend.text = element_text(size = 10))  # con theme modifico gli elementi del grafico ( titolo, testo dei titoli e valori degli assi, 
                                                                                                  # titolo ed etichette della legenda)
g1  # visualizzo il grafico



#------------------------------------------------#
#   STEP 5  -  Calcolo degli indici NDVI e NBR   #
#------------------------------------------------#

# Per analizzare l'area incendiata è utile calcolare due indici: il Normalized Difference Vegetation Index (NDVI) e il Normalized Burn Ratio (NBR)

# L'indice NDVI è utilizzato per descrivere lo stato fisiologico della vegetazione e si basa sulla firma spettrale dei vegetali, che quando
# sono in salute mostrano un picco di riflettanza nel NIR e assorbimento nel RED.
# La sua variazione spaziale permette di ricoscere aree con diversi tipi di vegetazione: valori maggiori per le aree boschive 
# e minori per quelle coltivate o a vegetazione prevalentemente erbacea.
# La sua variazione temporale permette invece, in questo caso specifico, di riconoscere il danno subito dalla vegetazione dopo l'incendio: 
# le aree colpite avranno infatti un calo drastico del valore di NDVI.
## NDVI= (NIR-RED)/(NIR+RED)

# L'indice NBR è utilizzato per mappare la severità delle aree incendiate e si basa anch'esso sulla caratteristica firma spettrale dei vegetali: una vegetazione 
# in normale stato di salute mostra un picco di riflettanza nel NIR e un valore invece debole nello SWIR, mentre a seguito di un incendio, e quindi di una 
# perdita del materiale fotosintetizzante, la riflettanza nel NIR cala drasticamente e si ha un netto aumento di riflettanza nello SWIR.
# Nel calcolo nel NBR si usa una banda di SWIR con lunghezze d'onda comprese tra  2080 e 2350, che nel caso di Sentinel-2 è la B12.
## NBR=  (NIR-SWIR)/(NIR+SWIR)

# Le bande necessarie a calcolare gli indici e i rispettivi layer nelle due immagini sono quindi:

#    BANDA   NOME LAYER JULY10   NOME LAYER JULY25
#   NIR=B08     july22_B08          july30_B08
#   RED=B04     july22_B04          july30_B04
#   SWIR=B12    july22_B12          july30_B12


## Calcolo del NDVI

NDVI_july22<-(july22_crop$july22_B08-july22_crop$july22_B04)/(july22_crop$july22_B08+july22_crop$july22_B04) # NDVI=(NIR-RED)/(NIR+RED)
NDVI_july30<-(july30_crop$july30_B08-july30_crop$july30_B04)/(july30_crop$july30_B08+july30_crop$july30_B04) # con il $ scelgo il layer che mi serve all'interno del RasterStack

NDVI_july22
# values: -0.987013, 0.9280152  (min, max)

NDVI_july30
# values     : -0.6161616, 0.9210305  (min, max)

# Visti i valori max e min dei due NDVI, definisco un range comune [-0.99 , 0.93] per confrontare i due grafici con un'unica scala colore.
# Nel plot escludo poi i valori negativi per per eliminare l'acqua e valorizzare l'area vegetata


## Visualizzo a confronto le mappe di NDVI del 22 Luglio e del 30 luglio:

clz<-wes_palette("Zissou1", 100, type = c("continuous"))  # associo alla variabile clz una palette di colori dal pacchetto wesanderson

p10<-ggplot(NDVI_july22, aes(x,y)) +  # con ggplot creo il grafico che contiene la mappa di NDVI calcolata al 22 luglio
     geom_raster(aes(fill=layer)) +  # geom_raster permette di plottare nel grafico un elemento raster
     scale_fill_gradientn(colors = clz, limits = c(0, 0.93))+  # definisco la scala colore e ne impongo i limiti max e min
     guides(fill = guide_colourbar(barwidth= 15)) +  # definisco la lunghezza della barra della scala colori
     ggtitle("NDVI 22 Luglio 2021") +     # titolo del grafico
     xlab("Long") + ylab("Lat") +   # titoli degli assi
     theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", hjust=0.5),                                                                                         
          axis.title=element_text(size=10), axis.text= element_text(size=8),                          
          legend.title = element_blank())    # con theme modifico gli elementi del grafico (titolo, testo dei titoli valori degli assi, titolo ed etichette della legenda)
                
p11<-ggplot(NDVI_july30, aes(x,y)) +   # con ggplot creo il grafico che contiene la mappa di NDVI calcolata al 30 luglio
     geom_raster(aes(fill=layer)) +   # geom_raster permette di plottare nel grafico un elemento raster
     scale_fill_gradientn(colors = clz, limits = c(0, 0.93))+   # definisco la scala colore e ne impongo i limiti max e min 
     guides(fill = guide_colourbar(barwidth= 15)) +  # definisco la lunghezza della barra della scala colori
     ggtitle(" NDVI 30 Luglio 2021") +    # titolo del grafico
     xlab("Long") + ylab("Lat") +    # titoli degli assi
     theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", hjust=0.5),  
          axis.title=element_text(size=10), axis.text= element_text(size=8),
          legend.title = element_blank())    # con theme modifico gli elementi del grafico ( titolo, testo dei titoli e valori degli assi, titolo ed etichette della legenda)
             
ggarrange(p10, p11, ncol = 2, common.legend=TRUE, legend="bottom")  # uso ggarrange per plottare le due mappe in un unico grafico e inserire una legenda comune
# Da questo plot si osserva bene il calo drastico di NDVI nella zona colpita dagli incendi

          
## Calcolo del NBR

NBR_july22<-(july22_crop$july22_B08-july22_crop$july22_B12)/(july22_crop$july22_B08+july22_crop$july22_B12)  # NBR= (NIR-SWIR)/(NIR+SWIR)
NBR_july30<-(july30_crop$july30_B08-july30_crop$july30_B12)/(july30_crop$july30_B08+july30_crop$july30_B12)  # con il $ scelgo il layer che mi serve all'interno del RasterStack

NBR_july22
# values: -0.9692308, 0.8666667  (min, max)

NBR_july30
# values: -0.6619545, 0.8108208  (min, max)

# Visti i valori max e min dei due NBR, per definire una scala di colore comune da plottare sui due grafici impongo un range tra -0.97 e 0.87
# per entrambi e poi plotto una legenda comune nel grafico


## Visuaizzo a confronto le mappe di NBR del 22 Luglio e del 30 luglio:

p13<-ggplot(NBR_july22, aes(x,y)) +   # con ggplot creo il grafico che contiene la mappa di NBR calcolata il 10 luglio
     geom_raster(aes(fill=layer)) +  # geom_raster permette di plottare nel grafico un elemento raster
     scale_fill_gradientn(colors = clz,limits = c(-0.97, 0.87))+  # impongo i limiti max e min della scala di colore
     guides(fill = guide_colourbar(barwidth= 15)) +  # definisco la lunghezza della barra della scala colori
     ggtitle("NBR 10 Luglio 2021") +     # titolo dell'immagine
     xlab("Long") + ylab("Lat") +    #titoli degli assi
     theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", hjust=0.5),  
          axis.title=element_text(size=10), axis.text= element_text(size=8),
          legend.title = element_blank())  # con theme modifico gli elementi del grafico ( titolo, testo dei titoli e valori degli assi, titolo ed etichette della legenda)
        
p14<-ggplot(NBR_july30, aes(x,y)) +  # con ggplot creo il grafico che contiene la mappa di NBR calcolata il 25 luglio
     geom_raster(aes(fill=layer)) +  # geom_raster permette di plottare nel grafico un elemento raster
     scale_fill_gradientn(colors = clz,limits = c(-0.97, 0.87))+  # impongo i limiti max e min della scala di colore
     guides(fill = guide_colourbar(barwidth= 15)) +  # definisco la lunghezza della barra della scala colori
     ggtitle(" NBR 25 Luglio 2021") +     # titolo dell'immagine
     xlab("Long") + ylab("Lat") +    #titoli degli assi
     theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", hjust=0.5),  
          axis.title=element_text(size=10), axis.text= element_text(size=8),
          legend.title = element_blank())   # con theme modifico gli elementi del grafico ( titolo, testo dei titoli e valori degli assi, titolo ed etichette della legenda)

ggarrange(p13, p14, ncol = 2, common.legend=TRUE, legend="bottom")   # uso ggarrange per plottare le due mappe in un unico grafico e inserire una legenda comune
# Anche in questo caso il calo di NBR nella seconda foto indica un aumento della riflettanza nello SWIR simultaneamente a un calo nel NIR dovuti all'incendio


## Per quantificare il calo di entrambi gli indici dovuto all'incendio se ne calcola la differenza:

deltaNDVI<- NDVI_july22 - NDVI_july30  # differenza tra NDVI pre-incendio e NDVI post-incendio
deltaNBR<- NBR_july22 - NBR_july30  # differenza tra NBR pre-incendio e NBR post-incendio

deltaNDVI
# values: -1.126814, 0.8725326  (min, max)

deltaNBR
# values: -1.162757, 1.347663  (min, max)

# Visti i valori max e min dei due NDVI, definisco un range comune [-1.16 , 1.35] per confrontare i due grafici con un'unica scala colore.
# Nel plot escludo poi i valori minori di -0.1 legati all'acqua e valorizzare l'area vegetata


## Visualizzo il deltaNDVI e il deltaNBR:

p12<-ggplot(deltaNDVI, aes(x,y)) +  # con ggplot creo il grafico che contiene la mappa di deltaNDVI calcolata
     geom_raster(aes(fill=layer)) +  # geom_raster permette di plottare nel grafico un elemento raster
     scale_fill_gradientn(colors = clz, limits = c(-0.1, 1.35)) +   # definisco la scala colore e ne impongo i limiti max e min
     guides(fill = guide_colourbar(barwidth= 15))  +  # definisco la lunghezza della barra della scala colori
     ggtitle("deltaNDVI") +     # titolo del grafico
     xlab("Long") + ylab("Lat") +   # titoli degli assi
     theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", hjust=0.5), 
          axis.title=element_text(size=10), axis.text= element_text(size=8),
          legend.title = element_blank())    # con theme modifico gli elementi del grafico ( titolo, testo dei titoli e valori degli assi, titolo ed etichette della legenda)

p15<-ggplot(deltaNBR, aes(x,y)) +   # con ggplot creo il grafico che contiene la mappa di deltaNBR calcolata
     geom_raster(aes(fill=layer)) +   # geom_raster permette di plottare nel grafico un elemento raster
     scale_fill_gradientn(colors = clz, limits = c(-0.1, 1.35)) +    # impongo i limiti max e min della scala di colore
     guides(fill = guide_colourbar(barwidth= 15)) +  # definisco la lunghezza della barra della scala colori
     ggtitle("deltaNBR") +     # titolo dell'immagine
     xlab("Long") + ylab("Lat") +    # titoli degli assi
     theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", hjust=0.5),  
          axis.title=element_text(size=10), axis.text= element_text(size=8),
          legend.title = element_blank())   # con theme modifico gli elementi del grafico ( titolo, testo dei titoli e valori degli assi, titolo ed etichette della legenda)

ggarrange(p12, p15, ncol = 2, common.legend=TRUE, legend="bottom")  # uso ggarrange per plottare le due mappe in un unico grafico e inserire una legenda comune



#---------------------------------------------#
#   STEP 6  -  Classificazione del deltaNBR   #
#---------------------------------------------#

# Per riconoscere e quantificare le aree più danneggiate dagli incendi applico una classificazione dell'indice deltaNBR 

set.seed(53)  # la funzione set.seed fissa il set di pixel di partenza e permette di poter replicare più volte lo stesso risultato, 
                                                       # altrimenti il processo randomico porterebbe ogni volta qualche differenza
dNBR_c3<-unsuperClass(deltaNBR, nClasses=3) # con la funzione unsuperClass applico una classificazione non supervisionata con un numero di classi definito (qui 3) 
                                                    # e la associo alla variabile dNBR_c3

## PLOT12 - La mappa di classificazione del deltaNBR

p16<-ggplot(dNBR_c3$map, aes(x,y)) +  # con ggplot creo il grafico che contiene la mappa calcolata
     geom_raster(aes(fill=factor(layer))) +  # geom_raster permette di plottare nel grafico un elemento raster
     scale_fill_manual(values=c('red','yellow','darkgreen'),  # definisco i colori delle classi
                       name=("Danno"), labels=c("Moderato", "Alto", "Nullo")) +   # definisco nome della legenda ed etichette dei colori corrispondenti
     ggtitle("Mappa di classificazione del deltaBNR") +    # titolo del grafico
     xlab("Long") + ylab("Lat") +    # titoli degli assi
     theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold",  hjust=0.5),  
           axis.title=element_text(size=10), axis.text= element_text(size=8),                                                                                
           legend.title = element_text(size=10, face="bold"),
           legend.text = element_text(size = 10)) # con theme modifico gli elementi del grafico (sfondo, titolo, testo dei titoli e valori degli assi, 
                                                                                                                    # titolo ed etichette della legenda)
p16
# La mappa di classificazione mette in evidenza una zona riconducibile alle aree non coinvolte dagli incendi (C3) e altre due alle zone invece colpite, con due 
# diversi gradi di severità (C2, con i maggiori valori di deltaNBR, e C1)

## Visualizzo a confronto le mappe di classificazione pre-incendio e di classificazione deltaNBR
grid.arrange(p9, p16, ncol = 2)  # con la funzione grid.arrange plotto le due immagini insieme in un unico grafico 

# Confrontando le due mappe di classificazione si osserva come il grado di severità maggiore di danno (C1) sono prevalenti nella porzione corrispondente alle aree boschive, 
# dove il valore di riflettanza nel NIR di partenza era maggiore e quindi il calo di NBR è stato più drastico

## Visualizzo a confronto l'immagine in falsi colori e la classificazione di deltaNBR
grid.arrange(p8, p16, ncol = 2)  # con la funzione grid.arrange plotto le due immagini insieme in un unico grafico 


## Ottenuta la classificazione è possibile calcolare le percentuali di area che rientrano nelle diverse classi

freq(dNBR_c3$map)  # con la funzione freq ricavo il numero di pixel che ricade in ogni classe
#  value   count
#    1     227780
#    2     139781
#    3     743903

perc_d<-freq(dNBR_c3$map)/pxtot   # normalizzo il numero di pixel di ogni classe per il numero totale dei pixel per ottenere i valori percentuali
perc_d
#  value   count
#   1    0.2049369
#   2    0.1257630
#   3    0.6693001

# Per cui approssimando si ha:
# C1: 20.49%  --> Severità moderata
# C2: 12.58%  --> Severità alta
# C3: 66.93%  --> Severità nulla

## Conoscendo la risoluzione dell'immagine è possibile quantificare le aree di territorio coinvolte dall'incendio
# Area dell'immagine= 598.677746 km2 
# (Vedi procedimento per il calcolo dell'area alla riga 246)


## Inserisco i dati otenuti in un dataset, le cui colonne saranno:
Classi_deltaNBR<-c("C1","C2", "C3") # alla variabile Classi associo i nomi delle classi ottenute
Danno<- c( "Moderato", "Alto", "Nullo")  # alla variabile Danno associo una descrizione qualitativa del danno sulla base dei valori di deltaNBR (più alti per danni maggiori)
Area_perc_dan<-c(0.2049, 0.1258, 0.6693)  # alla variabile Area_perc_dan associo i valori percentuali ricavati precedentemente
Area_km2_dan<-Area_perc_dan*598.677746  # moltiplicando l'area percentuale per l'area totale in km2 ottengo le aree in km2 che rientrano nelle 3 classi

perc_dan<-data.frame(Classi_deltaNBR, Danno, Area_perc_dan, Area_km2_dan)  # con data.frame inserisco le variabili all'interno di un dataset che associo alla variabile perc_dan
perc_dan  # visualizzo il data.frame


## Visualizzo il grafico a barre dei dati presenti nel dataframe

g2<-ggplot(perc_dan, aes(x=factor(Danno,level = c("Alto", "Moderato", "Nullo")), y=Area_km2_dan)) +  # con ggplot creo un grafico che contiene i dati del dataset 
    geom_bar(stat="identity", width=0.5, (aes(fill = Danno))) +  # geom_bar permette di creare un grafico a barre
    ggtitle("Aree danneggiate dall'incendio") +  # titolo del grafico
    xlab("") + ylab("Area (km2)") +  # titoli degli assi
    theme(plot.title = element_text(size=16, face="bold",  hjust=0.5), 
           axis.title=element_text(size=10), axis.text= element_text(size=10),
          axis.text.x=element_blank(),
           axis.ticks.x=element_blank(),
           legend.title = element_text(size=12, face="bold"),
           legend.text = element_text(size = 10))    # con theme modifico gli elementi del grafico ( titolo, testo dei titoli e valori degli assi, 
                                                                                                  # titolo ed etichette della legenda)
g2    # visualizzo il grafico

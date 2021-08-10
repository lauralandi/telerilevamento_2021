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

#  STEP 1  -  Richiamare i pacchetti necessari al codice e definire la working directory  ......... 024
#  STEP 2  -  Scelta delle immagini e importazione  ............................................... 041
#  STEP 3  -  Elaborazione delle immagini  ........................................................ 123
#  STEP 4 -   Classificazione della vegetazione pre-incendio  ..................................... 201
#  STEP 5  -  Calcolo degli indici NDVI e NBR  .................................................... 284
#  STEP 6  -  Classificazione della severità di danno  ............................................ 436



#-------------------------------------------------------------------------------------------#
#   STEP 1  -  Richiamare i pacchetti necessari al codice e definire la working directory   #
#-------------------------------------------------------------------------------------------#

library(raster) # pacchetto con funzioni per elaborare file raster
library(RStoolbox) # pacchetto con funzioni per processare le immagini (tra cui unsuperClass)
library(ggplot2) # pacchetto con diverse funzioni per creare e modificare grafici
library(ggpubr) # pacchetto che aggiunge ulteriori funzioni per personalizzare i grafici (tra cui ggarrange con common.legend)
library(grid)  # pacchetto che aggiunge ulteriori funzioni per personalizzare i grafici (tra cui gpar)
library(gridExtra) # pacchetto con funzioni per lavorare con grafici (tra cui grid.arrange)
library(wesanderson) # pacchetto con diverse palette di colori ispirate a Wes Anderson
library(rasterVis) # pacchetto con funzioni aggiuntive per elaborare e plottare oggetti raster (tra cui levelplot)

setwd("C:/lab/sardegna2/")  # definire la working directory



#-----------------------------------------------------#
#   STEP 2  -  Scelta delle immagini e importazione   #
#-----------------------------------------------------#

## Le due immagini utilizzate per l'analisi derivano dai satelliti Sentinel-2 e rappresentano la zona del Montiferru in provincia di 
## Oristano il 22 e il 30 Luglio 2021, ovvero prima e dopo l'incendio che ha colpito l'area a partire dal 23 Luglio.

## Le diverse bande in cui Sentinel-2 fornisce le immagini sono:

##       BANDA                  LUNGHEZZA D'ONDA (micrometri)      
##   B01= coastal aerosol                 0.443                           
##   B02= blue                            0.490                           
##   B03= green                           0.560                           
##   B04= red                             0.665                           
##   B05= vegetation red edge             0.705                           
##   B06= vegetation red edge             0.740                           
##   B07= vegetation red edge             0.783                           
##   B08= NIR                             0.842                           
##   B08A= vegetation red edge            0.865                           
##   B09= water vapour                    0.945                           
##   B11= SWIR                            1.610                           
##   B12= SWIR                            2.190                           


## Nello specifico per questa analisi sono state sfruttate le bande:
##   B02= blue 
##   B03= green
##   B04= red  
##   B08= NIR
##   B11= SWIR
##   B12= SWIR


# Le diverse bande per entrambe le immagini sono state scaricate separatamente dal sito Sentinel Hub EO Browser. 
# Per importarle come due singoli oggetti RasterStack contenenti tutte le bande seguo i passaggi:

rlist22<-list.files(pattern="july22") # con la funzione list.files creo una lista di file con le diverse bande con pattern "july22" e associo alla variabile rlist22
import22<-lapply(rlist22,raster) # con la funzione lapply applico la funzione raster su tutti i file della lista e li importo associati alla variabile import22
july22<-stack(import22) # con la funzione stack creo un unico oggetto RasterStack che contiene come layer tutte le bande importate e associo alla variabile july22

rlist30<-list.files(pattern="july30") # con la funzione list.files creo una lista di file con le diverse bande con pattern "july30" e associo alla variabile rlist30
import30<-lapply(rlist30,raster) # con la funzione lapply applico la funzione raster su tutti i file della lista e li importo associati alla variabile import30
july30<-stack(import30) # con la funzione stack creo un unico oggetto RasterStack che contiene come layer tutte le bande importate e associo alla variabile july30

cl=colorRampPalette(c("gray48", "white"))(100)  # definisco una color palette sui toni del grigio

# con la funzione levelplot visualizzo le bande importate per l'immagine del 22 Luglio
p0<-levelplot(july22,col.regions=cl,main="Bande dell'immagine 22 Luglio",       # la funzione levelplot permette di arricchire il grafico con la color palette scelta
          names.attr=c("B01","B02","B03","B04","B05","B06","B07","B08","B09","B11","B12","B8A"))                                   # e il titolo principale e dei layer
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
          axis.title=element_text(size=10), axis.text= element_text(size=8))   # con theme modifico gli elementi del grafico (sfondo, testo dei titoli e dei valori degli assi)
        
p2<-ggRGB(july30, 4, 3, 2 , stretch="lin") +   # con ggRGB monto le bande in RGB in veri colori e applico uno stretch lineare per migliorare il contrasto
    ggtitle("30 Luglio 2021") +   # titolo dell'immagine
    xlab("Long") + ylab("Lat") +  # titoli degli assi
    theme(panel.background = element_blank(), plot.title = element_text(size=11, face="bold", color="red"), 
          axis.title=element_text(size=10), axis.text= element_text(size=8))    # con theme modifico gli elementi del grafico (sfondo, testo dei titoli e dei valori degli assi)

grid.arrange(p1, p2, nrow = 2, top=grid.text("Immagini in veri colori", gp=gpar(fontsize=15,font=2)))     
# con la funzione grid.arrange plotto le due immagini insieme in un unico grafico aggiungendo un titolo



#-------------------------------------------#
#   STEP 3  -  Elaborazione delle immagini  #
#-------------------------------------------#

# Per analizzare meglio l'area bruciata dagli incendi mi focalizzo su un'area più ristretta.

plotRGB(july30, 4, 3, 2, stretch="lin")  # plotto l'immagine del 30 Luglio in RGB
e<- drawExtent(show=TRUE, col="red") # con la funzione drawExtent disegno un riquadro sull'immagine aperta e genero un oggetto extent associato alla variabile e
e   # richiamando la variabile posso visualizzare le sue informazioni:
    # class      : Extent 
    # xmin       : 943140.1 
    # xmax       : 967073.2 
    # ymin       : 4881607 
    # ymax       : 4906605

july22_crop<- crop(july22, e)  # con la funzione crop ritaglio le immagini nelle dimensioni definite dalla variabile "e" e le associo a due nuove variabili
july30_crop<- crop(july30, e)


## Visualizzo le due immagini ritagliate in RGB veri colori (r=red, g=green, b=blue):

p7<-ggRGB(july22_crop, 4, 3, 2, stretch="lin") +  # con ggRGB monto le bande in falsi colori e applico uno stretch lineare per migliorare il contrasto
    ggtitle("22 Luglio 2021") +    # titolo dell'immagine 
    xlab("Long") + ylab("Lat") +    # titoli degli assi
    theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", color="red"), 
          axis.title=element_text(size=10), axis.text= element_text(size=8))   # con theme modifico gli elementi del grafico (sfondo, testo dei titoli e dei valori degli assi)
        
p8<-ggRGB(july30_crop, 4, 3, 2, stretch="lin") +   # con ggRGB monto le bande in falsi colori e applico uno stretch lineare per migliorare il contrasto
    ggtitle("30 Luglio 2021") +     # titolo dell'immagine
    xlab("Long") + ylab("Lat") +  # titoli degli assi
    theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", color="red"), 
          axis.title=element_text(size=10), axis.text= element_text(size=8))  # con theme modifico gli elementi del grafico (sfondo, testo dei titoli e dei valori degli assi)

grid.arrange(p7, p8, ncol = 2, top=grid.text("Immagini in veri colori", gp=gpar(fontsize=18,font=2)))  
# con la funzione grid.arrange plotto le due immagini insieme in un unico grafico aggiungendo un titolo


## Visualizzo le due immagini ritagliate in falsi colori (r=NIR, g=green, b=blue):

p7.1<-ggRGB(july22_crop, 8, 3, 2, stretch="lin") +  # con ggRGB monto le bande in falsi colori e applico uno stretch lineare per migliorare il contrasto
    ggtitle("22 Luglio 2021") +    # titolo dell'immagine 
    xlab("Long") + ylab("Lat") +    # titoli degli assi
    theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", color="red"), 
          axis.title=element_text(size=10), axis.text= element_text(size=8))   # con theme modifico gli elementi del grafico (sfondo, testo dei titoli e dei valori degli assi)
        
p8.1<-ggRGB(july30_crop, 8, 3, 2, stretch="lin") +   # con ggRGB monto le bande in falsi colori e applico uno stretch lineare per migliorare il contrasto
    ggtitle("30 Luglio 2021") +     # titolo dell'immagine
    xlab("Long") + ylab("Lat") +  # titoli degli assi
    theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", color="red"), 
          axis.title=element_text(size=10), axis.text= element_text(size=8))  # con theme modifico gli elementi del grafico (sfondo, testo dei titoli e dei valori degli assi)

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
          axis.title=element_text(size=10), axis.text= element_text(size=8))   # con theme modifico gli elementi del grafico (sfondo, testo dei titoli e dei valori degli assi)
        
p8.2<-ggRGB(july30_crop, 11, 10, 4, stretch="lin") +   # con ggRGB monto le bande in falsi colori e applico uno stretch lineare per migliorare il contrasto
    ggtitle("30 Luglio 2021") +     # titolo dell'immagine
    xlab("Long") + ylab("Lat") +  # titoli degli assi
    theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", color="red"), 
          axis.title=element_text(size=10), axis.text= element_text(size=8))  # con theme modifico gli elementi del grafico (sfondo, testo dei titoli e dei valori degli assi)

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
           legend.text = element_text(size = 10))  # con theme modifico gli elementi del grafico (sfondo, titolo, testo dei titoli e dei valori degli assi, 
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


## Costruisco un grafico a barre dei dati presenti nel dataframe:

g1<-ggplot(perc_cop, aes(x=Copertura, y=Area_km2_cop)) +  # con ggplot creo un grafico che contiene i dati del dataset 
    geom_bar(stat="identity", width=0.5, (aes(fill = Copertura))) + # geom_bar permette di creare un grafico a barre
    scale_fill_manual(values=c("darkgreen", "green")) +  # definisco i colori di riempimento delle barre
    ggtitle("Aree di copertura vegetale pre-incendi") +  # titolo del grafico
    xlab("Copertura") + ylab("Area (km2)") +  # titoli degli assi
    theme(plot.title = element_text(size=16, face="bold",  hjust=0.5), 
           axis.title=element_text(size=12), axis.text= element_text(size=10),
           legend.position = "none")  # con theme modifico gli elementi del grafico (titolo, testo dei titoli e dei valori degli assi, legenda)
                                                                                                  
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

# L'indice NBR è utilizzato per mappare la severità dei danni delle aree incendiate e si basa anch'esso sulla caratteristica firma spettrale dei vegetali: una vegetazione 
# in normale stato di salute mostra un picco di riflettanza nel NIR e ssorbimento nello SWIR, mentre a seguito di un incendio, e quindi di una 
# perdita del materiale fotosintetizzante, la riflettanza nel NIR cala drasticamente e si ha un netto aumento di riflettanza nello SWIR.
# Nel calcolo nel NBR si usa una banda di SWIR con lunghezze d'onda comprese tra 2080 e 2350 micrometri, che nel caso di Sentinel-2 è la B12.
## NBR=  (NIR-SWIR)/(NIR+SWIR)

# Le bande necessarie a calcolare gli indici e i rispettivi layer nelle due immagini sono quindi:

#    BANDA   NOME LAYER JULY22   NOME LAYER JULY30
#   NIR=B08     july22_B08          july30_B08
#   RED=B04     july22_B04          july30_B04
#   SWIR=B12    july22_B12          july30_B12


## Calcolo del NDVI

NDVI_july22<-(july22_crop$july22_B08-july22_crop$july22_B04)/(july22_crop$july22_B08+july22_crop$july22_B04) # NDVI=(NIR-RED)/(NIR+RED)
NDVI_july30<-(july30_crop$july30_B08-july30_crop$july30_B04)/(july30_crop$july30_B08+july30_crop$july30_B04) # con il $ scelgo il layer che mi serve all'interno del RasterStack

NDVI_july22
# values: -0.987013, 0.9280152  (min, max)

NDVI_july30
# values: -0.6161616, 0.9210305  (min, max)

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
          legend.title = element_blank())    # con theme modifico gli elementi del grafico (titolo, testo dei titoli e dei valori degli assi, legenda)
                
p11<-ggplot(NDVI_july30, aes(x,y)) +   # con ggplot creo il grafico che contiene la mappa di NDVI calcolata al 30 luglio
     geom_raster(aes(fill=layer)) +   # geom_raster permette di plottare nel grafico un elemento raster
     scale_fill_gradientn(colors = clz, limits = c(0, 0.93))+   # definisco la scala colore e ne impongo i limiti max e min 
     guides(fill = guide_colourbar(barwidth= 15)) +  # definisco la lunghezza della barra della scala colori
     ggtitle(" NDVI 30 Luglio 2021") +    # titolo del grafico
     xlab("Long") + ylab("Lat") +    # titoli degli assi
     theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", hjust=0.5),  
          axis.title=element_text(size=10), axis.text= element_text(size=8),
          legend.title = element_blank())    # con theme modifico gli elementi del grafico ( titolo, testo dei titoli e dei valori degli assi, legenda)
             
ggarrange(p10, p11, ncol = 2, common.legend=TRUE, legend="bottom")  # uso ggarrange per plottare le due mappe in un unico grafico e inserire una legenda comune
# Da questo plot si osserva bene il calo di NDVI nella zona colpita dagli incendi

          
## Calcolo del NBR

NBR_july22<-(july22_crop$july22_B08-july22_crop$july22_B12)/(july22_crop$july22_B08+july22_crop$july22_B12)  # NBR= (NIR-SWIR)/(NIR+SWIR)
NBR_july30<-(july30_crop$july30_B08-july30_crop$july30_B12)/(july30_crop$july30_B08+july30_crop$july30_B12)  # con il $ scelgo il layer che mi serve all'interno del RasterStack

NBR_july22
# values: -0.9692308, 0.8666667  (min, max)

NBR_july30
# values: -0.6619545, 0.8108208  (min, max)

# Visti i valori max e min dei due NBR, definisco un range comune [-0.97 , 0.87] per confrontare i due grafici con un'unica scala colore. 


## Visualizzo a confronto le mappe di NBR del 22 Luglio e del 30 luglio:

p13<-ggplot(NBR_july22, aes(x,y)) +   # con ggplot creo il grafico che contiene la mappa di NBR calcolata il 22 luglio
     geom_raster(aes(fill=layer)) +  # geom_raster permette di plottare nel grafico un elemento raster
     scale_fill_gradientn(colors = clz,limits = c(-0.97, 0.87))+  # impongo i limiti max e min della scala di colore
     guides(fill = guide_colourbar(barwidth= 15)) +  # definisco la lunghezza della barra della scala colori
     ggtitle("NBR 22 Luglio 2021") +     # titolo dell'immagine
     xlab("Long") + ylab("Lat") +    #titoli degli assi
     theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", hjust=0.5),  
          axis.title=element_text(size=10), axis.text= element_text(size=8),
          legend.title = element_blank())  # con theme modifico gli elementi del grafico ( titolo, testo dei titoli e dei valori degli assi, legenda)
        
p14<-ggplot(NBR_july30, aes(x,y)) +  # con ggplot creo il grafico che contiene la mappa di NBR calcolata il 30 luglio
     geom_raster(aes(fill=layer)) +  # geom_raster permette di plottare nel grafico un elemento raster
     scale_fill_gradientn(colors = clz,limits = c(-0.97, 0.87))+  # impongo i limiti max e min della scala di colore
     guides(fill = guide_colourbar(barwidth= 15)) +  # definisco la lunghezza della barra della scala colori
     ggtitle(" NBR 30 Luglio 2021") +     # titolo dell'immagine
     xlab("Long") + ylab("Lat") +    #titoli degli assi
     theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", hjust=0.5),  
          axis.title=element_text(size=10), axis.text= element_text(size=8),
          legend.title = element_blank())   # con theme modifico gli elementi del grafico ( titolo, testo dei titoli e dei valori degli assi, legenda)

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
# Nel plot escludo poi i valori minori di -0.1 legati all'acqua per valorizzare l'area vegetata


## Visualizzo il deltaNDVI e il deltaNBR:

p12<-ggplot(deltaNDVI, aes(x,y)) +  # con ggplot creo il grafico che contiene la mappa di deltaNDVI calcolata
     geom_raster(aes(fill=layer)) +  # geom_raster permette di plottare nel grafico un elemento raster
     scale_fill_gradientn(colors = clz, limits = c(-0.1, 1.35)) +   # definisco la scala colore e ne impongo i limiti max e min
     guides(fill = guide_colourbar(barwidth= 15))  +  # definisco la lunghezza della barra della scala colori
     ggtitle("deltaNDVI") +     # titolo del grafico
     xlab("Long") + ylab("Lat") +   # titoli degli assi
     theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", hjust=0.5), 
          axis.title=element_text(size=10), axis.text= element_text(size=8),
          legend.title = element_blank())    # con theme modifico gli elementi del grafico (titolo, testo dei titoli e dei valori degli assi, legenda)

p15<-ggplot(deltaNBR, aes(x,y)) +   # con ggplot creo il grafico che contiene la mappa di deltaNBR calcolata
     geom_raster(aes(fill=layer)) +   # geom_raster permette di plottare nel grafico un elemento raster
     scale_fill_gradientn(colors = clz, limits = c(-0.1, 1.35)) +    # impongo i limiti max e min della scala di colore
     guides(fill = guide_colourbar(barwidth= 15)) +  # definisco la lunghezza della barra della scala colori
     ggtitle("deltaNBR") +     # titolo dell'immagine
     xlab("Long") + ylab("Lat") +    # titoli degli assi
     theme(panel.background = element_blank(), plot.title = element_text(size=13, face="bold", hjust=0.5),  
          axis.title=element_text(size=10), axis.text= element_text(size=8),
          legend.title = element_blank())   # con theme modifico gli elementi del grafico (titolo, testo dei titoli e dei valori degli assi, legenda)

ggarrange(p12, p15, ncol = 2, common.legend=TRUE, legend="bottom")  # uso ggarrange per plottare le due mappe in un unico grafico e inserire una legenda comune



#--------------------------------------------------------#
#   STEP 6  -  Classificazione della severità di danno   #
#--------------------------------------------------------#

# Per riconoscere e quantificare le aree più danneggiate dagli incendi applico una classificazione dell'indice deltaNBR 

set.seed(53)  # la funzione set.seed fissa il set di pixel di partenza e permette di poter replicare più volte lo stesso risultato, 
                                                       # altrimenti il processo randomico porterebbe ogni volta qualche differenza
dNBR_c3<-unsuperClass(deltaNBR, nClasses=3) # con la funzione unsuperClass applico una classificazione non supervisionata con un numero di classi definito (qui 3) 
                                                    # e la associo alla variabile dNBR_c3

## Visualizzo la mappa di classificazione del deltaNBR:

p16<-ggplot(dNBR_c3$map, aes(x,y)) +  # con ggplot creo il grafico che contiene la mappa di classificazione calcolata
     geom_raster(aes(fill=factor(layer))) +  # geom_raster permette di plottare nel grafico un elemento raster
     scale_fill_manual(values=c('red','yellow','darkgreen'),  # definisco i colori delle classi
                       name=("Danno"), labels=c("Moderato", "Alto", "Nullo")) +   # definisco nome della legenda ed etichette dei colori corrispondenti
     ggtitle("Mappa di classificazione del deltaNBR") +    # titolo del grafico
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
# (Vedi procedimento per il calcolo dell'area alla riga 253)


## Inserisco i dati ottenuti in un dataset, le cui colonne saranno:
Classi_deltaNBR<-c("C1","C2", "C3") # alla variabile Classi_deltaNBR associo i nomi delle classi ottenute
Danno<- c( "Moderato", "Alto", "Nullo")  # alla variabile Danno associo una descrizione qualitativa del danno sulla base dei valori di deltaNBR (più alti per danni maggiori)
Area_perc_dan<-c(0.2049, 0.1258, 0.6693)  # alla variabile Area_perc_dan associo i valori percentuali ricavati precedentemente
Area_km2_dan<-Area_perc_dan*598.677746  # moltiplicando l'area percentuale per l'area totale in km2 ottengo le aree in km2 che rientrano nelle 3 classi

perc_dan<-data.frame(Classi_deltaNBR, Danno, Area_perc_dan, Area_km2_dan)  # con data.frame inserisco le variabili all'interno di un dataset che associo alla variabile perc_dan
perc_dan  # visualizzo il data.frame


## Costruisco un grafico a barre dei dati presenti nel dataframe

g2<-ggplot(perc_dan, aes(x=factor(Danno,level = c("Alto", "Moderato", "Nullo")), y=Area_km2_dan)) +  # con ggplot creo un grafico che contiene i dati del dataset 
    geom_bar(stat="identity", width=0.5, (aes(fill = Danno))) +  # geom_bar permette di creare un grafico a barre
    scale_fill_manual(values=c("yellow", "red", "darkgreen")) +  # definisco i colori di riempimento delle barre
    ggtitle("Aree danneggiate dall'incendio") +  # titolo del grafico
    xlab("Danno") + ylab("Area (km2)") +  # titoli degli assi
    theme(plot.title = element_text(size=16, face="bold",  hjust=0.5), 
           axis.title=element_text(size=12), axis.text= element_text(size=10),
           legend.position = "none")    # con theme modifico gli elementi del grafico ( titolo, testo dei titoli e valori degli assi, legenda)
                                                                                                 
g2    # visualizzo il grafico



#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
################################################################################################################################################################################
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#


#                                                                              Corso di
#                                                                  TELERILEVAMENTO GEO-ECOLOGICO
#                                                                           A.A. 2020-2021

#                                                                          R Code Complete                                                      


                                                                  #----------  SUMMARY: -----------#

#                                                                  1. Remote sensing - First Code
#                                                                  2. R Code - Time series
#                                                                  3. R Code - Copernicus
#                                                                  4. R Code - Knitr
#                                                                  5. R Code - Multivariate analysis
#                                                                  6. R Code - Classification
#                                                                  7. R Code - ggplot2
#                                                                  8. R Code - Vegetation Indices
#                                                                  9. R Code - Land Cover
#                                                                 10. R Code - Variability
#                                                                 11. R Code - Spectral Signatures
#                                                                 12. R Code - NO2

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

# 1. Remote sensing - First Code

# Primo codice in R per telerilevamento

# con install.packages installo i pacchetti che mi interessano (è sufficiente farlo una volta)
# install.packages("raster") 
# install.packages("RStoolbox")
# install.packages("ggplot2")
# una volta installati i pacchetti uso library per richiamare quelli che servono nel codice e poterli utilizzare
library(raster) # pacchetto con funzioni per elaborare file raster  
library(RStoolbox) # pacchetto con funzioni per processare le immagini (tra cui unsuperClass)
library(ggplot2) # pacchetto con diverse funzioni per creare e modificare grafici

setwd("C:/lab/") #imposto la working directory

### BANDE LANDSAT:
#B1= blu
#B2= verde
#B3= rosso
#B4= near infrared
#B5= middle infrared
#B6= infrarosso termico
#B7= infrarosso medio (altro sensore)

p224r63_2011 <- brick("p224r63_2011_masked.grd") # con la funzione brick estraggo i dati raster e li associo a una variabile
p224r63_2011 # leggo le informazioni raster
plot(p224r63_2011) # plotto l'immagine con la scala colori di default

cl1<-colorRampPalette(c("blue","green","orange","yellow")) (100) # definisco una nuova scala colore con la funzione colorRampPalette
plot(p224r63_2011,col=cl1) # plotto l'immagine con la nuova scala colore

dev.off() # pulisco la finestra grafica
plot(p224r63_2011$B1_sre, col=cl1) # plotto una sola banda a scelta usando $ per definire il layer specifico dell'oggetto RasterBrick che mi interessa

# con la funzione par definisco la visualizzazione di più grafici insieme in un multipanel con righe e colonne a mia scelta:
par(mfrow=c(2,1)) # 2 righe e 1 colonna
plot(p224r63_2011$B1_sre,col=cl1) 
plot(p224r63_2011$B2_sre,col=cl1)

par(mfrow=c(4,1)) # 4 righe e 1 colonna
plot(p224r63_2011$B1_sre,col=cl1) # plotto le immagini da inserire nel multipanel
plot(p224r63_2011$B2_sre,col=cl1)
plot(p224r63_2011$B3_sre,col=cl1)
plot(p224r63_2011$B4_sre,col=cl1)

par(mfrow=c(2,2)) # 2 righe e 2 colonne
plot(p224r63_2011$B1_sre,col=cl1) # plotto le immagini da inserire nel multipanel
plot(p224r63_2011$B2_sre,col=cl1)
plot(p224r63_2011$B3_sre,col=cl1)
plot(p224r63_2011$B4_sre,col=cl1)

# associo una scala colore diversa ad ogni banda e le plotto in un quadrato 2x2
par(mfrow=c(2,2)) # 2 righe 2 colonne
clb<- colorRampPalette(c("darkblue", "blue", "lightblue")) (100)  # definisco le scale colori
clv<- colorRampPalette(c("darkgreen", "green", "lightgreen")) (100)
clr<- colorRampPalette(c("darkred", "red", "orange")) (100)
clnir<- colorRampPalette(c("pink", "salmon","magenta")) (100)
plot(p224r63_2011$B1_sre,col=clb)  # plotto le immagini all'interno del multipanel con le nuove scale colori
plot(p224r63_2011$B2_sre,col=clv)
plot(p224r63_2011$B3_sre,col=clr)
plot(p224r63_2011$B4_sre,col=clnir)


### PLOT IN RGB
# con la funzione plotRGB posso montare sui canali RGB le bande in diverse combinazioni per ottenere visualizzazioni più efficaci delgi elementi che mi interessano
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin") # immagine a colori naturali, R=RED G=GREEN B=BLUE
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") # immagine in falsi colori R=NIR G=RED B=GREEN
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin") # immagine in falsi colori R=RED G=NIR B=GREEN
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin") # immagine in falsi colori R=RED G=GREEN B=NIR
# la funzione stretch modifica la distribuzione dei valori di riflettanza tra il minimo e il massimo per migliorare il contrasto e la visibilità dell'immagine

plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="hist") # le due funzioni stretch di default sono linear e histogram


pdf("pdf1") # salvo il risultato come pdf nella working directory
par(mfrow=c(2,2)) # imposto un multipanel 2x2 dove plottare le immagini
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin") # immagine a colori naturali, R=RED G=GREEN B=BLUE
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") # immagine in falsi colori R=NIR G=RED B=GREEN
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin") # immagine in falsi colori R=RED G=NIR B=GREEN
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin") # immagine in falsi colori R=RED G=GREEN B=NIR
dev.off() # pulisco la finestra grafica dopo il procedimento per salvare correttamente il pdf

#confronto le immagin a colori naturali e in falsi colori (nir in verde) con diversi stretch (lin e his)
par(mfrow=c(3,1))  # imposto un multipanel 1x3 dove plottare le immagini
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin") # immagine a colori naturali, R=RED G=GREEN B=BLUE con stretch lin
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin") # immagine in falsi colori R=RED G=NIR B=GREEN con stretch lin
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="hist") # immagine in falsi colori R=RED G=NIR B=GREEN con stretch his


### OSSERVARE CAMBIAMENTI NEL TEMPO DELLA STESSA ZONA

p224r63_1988 <- brick("p224r63_1988_masked.grd") # con la funzione brick importo l'immagine del 1988 come oggetto RasterBrick
plot(p224r63_1988) # plotto tutte le bande separate
plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin") # plotto l'immagine a colori naturali

# multitemporal set con stretch lineare: plotto su due righe l'immagine del 1988 e del 2011 per confrontarle
par(mfrow=c(2,1)) # multipanel 1x2
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin") # immagine 1988
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") # immagine 2011

# multitemporal set con stretch histogram: plotto su due righe l'immagine del 1988 e del 2011 per confrontarle
par(mfrow=c(2,1)) # multipanel 1x2
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="hist") #immagine 1988
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="hist") #immagine 2011

# creo un pdf con un multipanel 4x4 delle immagini 1988 e 2011 in stretch lineare e hist
pdf("multitemporal_set_1988_2011_lin_hist") # imposto la creazione del pdf
par(mfrow=c(2,2)) # imposto righe e colonne 2x2 dove plottare le immagini
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin") # plotto immagine 1988
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") # plotto immagine 2011
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="hist") #plotto immagine 1988
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="hist") #plotto immagine 2011
dev.off() # pulisco la finestra grafica dopo il procedimento per salvare correttamente il pdf

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

# 2. R Code - Time series

### TIME SERIES ANALYSIS
### GREENLAND INCREASE OF TEMPERATURE
### Data and code from Emanuela Cosma

# con install.packages installo i pacchetti che mi interessano (è sufficiente farlo una volta)
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

levelplot(TGr$lst_2000) # con la funzione levelplot il singolo layer è plottato insieme ai grafici che, lungo la direzione x e y, rappresentano la media rispettivamente
                                                                                                                                                  # di ogni colonna e riga
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

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

# 3. R Code - Copernicus

### VISUALIZZAZIONE DATI COPERNICUS

# con install.packages installo i pacchetti che mi interessano (è sufficiente farlo una volta)
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

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

# 4. R Code - Knitr

### REPORT IN KNITR

setwd("C:/lab/") # definisco la working directory

library(knitr)  # il pacchetto knitr permette di realizzare dei report

stitch("R_code_greenland.r", template=system.file("misc", "knitr-template.Rnw", package="knitr")) # la funzione stitch crea un report automatico sulla base di uno
                                                                                                  # script R, salvato nella wd e definito dal suo path, e un template  

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

# 5. R Code - Multivariate analysis

### ANALISI MULTIVARIATA

# uso library per richiamare i pacchetti che servono nel codice e poterli utilizzare

library(raster)  # pacchetto con funzioni per elaborare file raster
library(RStoolbox) # pacchetto con funzioni per processare le immagini (tra cui unsuperClass)

setwd("C:/lab/")  #definisco la working directory

p224r63_2011 <- brick("p224r63_2011_masked.grd")  # importo il file come oggetto RasterBrick, ovvero comporto da più layer

plot(p224r63_2011)  # plotto tutti i layer del raster importato

plot(p224r63_2011$B1_sre,p224r63_2011$B2_sre,col="red", pch=19, cex=2)  # plotto una grafico a dispersione (scatterplot) che mette in relazione i valori di due layer 
                                                                                                                                          # ponendoli sugli assi x e y

pairs(p224r63_2011) # con la funzione pairs creo una matrice di scatterplot che mettono in relazione i diversi layer scon tutte le combinazioni

### resampling (ricampionamento)
p224r63_2011res <- aggregate(p224r63_2011, fact=10)  # ricampiono il raster con un fattore 10 (in uscita ho 1 pixel ogni 100 del raster di partenza)

par(mfrow=c(2,1))  # imposto un multipanel con due righe dove plottare le due immagini
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch='lin') # immagine originale in RGB
plotRGB(p224r63_2011res, r=4, g=3, b=2, stretch='lin')  # immagine ricampionata in RGB

# L'analisi multivariata delle componenti principali PCA (Principal Component Analysis) permette la riduzione del numero di variabili senza perdere troppe informazioni.
# La PCA consiste nel proiettare le variabili originarie in un nuovo sistema cartesiano nel quale le variabili vengono ordinate in ordine decrescente di varianza: 
# la variabile con maggiore varianza viene proiettata sul primo asse (PC1), la seconda sul secondo asse (PC2) e così via. La riduzione della complessità avviene limitandosi 
# ad analizzare solo le componenti principali, ovvero quelle che spiegano maggiore varianza.

p224r63_2011res_PCA<-rasterPCA(p224r63_2011res) # la funzione rasterPCA applica un'analisi PCA e produce un oggetto che contiene la mappa e le informazioni sul modello
summary(p224r63_2011res_PCA$model) # la funzione summary fa un sommario delle info del modello generato dall'analisi di rasterPCA, con le percentuali di varianza 
                                                                                                                                            # spiegate da ogni PC
plot(p224r63_2011res_PCA$map)  # plotto tutte le componenti principali generate dall'analisi PCA

plotRGB(p224r63_2011res_PCA$map, r=1, g=2, b=3, stretch='lin')  # plotto in RGB le prime tre PC (quelle che spiegano maggiore varianza)
str(p224r63_2011res_PCA) # descrive le struttura dell'oggetto prodotto dall'analisi

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

# 6. R Code - Classification
  
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

set.seed(42) # la funzione set.seed fa in modo che il risultato sia sempre lo stesso nei diversi run utilizzando sempre lo stesso set di pixel che altrimenti 
                                                                                                                                 # sarebbe ogni volta randomico
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

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

# 7. R Code - ggplot2

##### GGPLOT2

# uso library per richiamare i pacchetti che servono nel codice e poterli utilizzare
library(raster) # pacchetto con funzioni per elaborare file raster
library(RStoolbox) # pacchetto con funzioni per processare le immagini (tra cui unsuperClass)
library(ggplot2) # pacchetto con diverse funzioni per creare e modificare grafici
library(gridExtra) # pacchetto con funzioni per lavorare con grafici (tra cui grid.arrange)

setwd("~/lab/") # definisco la wd

p224r63 <- brick("p224r63_2011_masked.grd")  # importo l'immagine come RasterBrick

ggRGB(p224r63,3,2,1, stretch="lin") # con la funzione ggRGB del pacchetto ggplot2 visualizzo l'immagine in RGB in veri colori
ggRGB(p224r63,4,3,2, stretch="lin")  # con la funzione ggRGB del pacchetto ggplot2 visualizzo l'immagine in RGB in falsi colori

p1 <- ggRGB(p224r63,3,2,1, stretch="lin")  # associo l'output del primo plot a una variabile p1
p2 <- ggRGB(p224r63,4,3,2, stretch="lin")  # associo l'output del secondo plot a una variabile p2

grid.arrange(p1, p2, nrow = 2) # con la funzione grid.arrange del pacchetto gridExtra costruisco un multipanel con due righe dove plottare le due immagini

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

# 8. R Code - Vegetation Indices

### INDICI DI VEGETAZIONE

# uso library per richiamare i pacchetti che servono nel codice e poterli utilizzare
library(raster) # pacchetto con funzioni per elaborare file raster
library(RStoolbox)  # pacchetto con funzioni per processare le immagini (tra cui unsuperClass)
library(rasterdiv) # pacchetto che permette di calcolare diversi indici (tra cui il wordlwide NDVI)
library(rasterVis)  # pacchetto con funzioni aggiuntive per elaborare oggetti raster

setwd("C:/lab/")  # definisco la wd

# importo le due immagini come oggetti RasterBrick:
defor1<-brick("defor1.jpg")
defor2<-brick("defor2.jpg")

# Le bande dell'immagino sono:
# B1=NIR B2=RED B3=GREEN

par(mfrow=c(2,1))  # creo un multipanel di due righe dove plottare le immagini
plotRGB(defor1, r=1, g=2, b=3, stretch='lin')  # plotto le immagini all'interno del multipanel in veri colori
plotRGB(defor2, r=1, g=2, b=3, stretch='lin')

# Calcolo del DVI = (NIR-RED)
# I layer del RasteBrick defor1 si chiamano defor1.1, defor 1.2, defor1.3 e corrispondono alle tre bande
cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100) # definisco una scala colori
DVI1<-defor1$defor1.1 - defor1$defor1.2 # calcolo il DVI della prima immagine applicando la sottrazione tra matrici B1-B2 (NIR-RED) e utilizzando $ per specificare
                                                                                                                                         # il layer che mi interessa
plot(DVI1, col=cl, main="DVI at time 1") # plotto il risultato dell'operazione con la scala colori scelta e un titolo

DVI2<-defor2$defor2.1 - defor2$defor2.2 # calcolo il DVI della seconda immagine sottraendo B1-B2 (NIR-RED) 
plot(DVI2, col=cl, main="DVI at time 2") # plotto il DVI ottenuto

par(mfrow=c(2,1)) # creo un multipanel con due righe dove plottare i due DVI
plot(DVI1, col=cl, main="DVI at time 1")
plot(DVI2, col=cl, main="DVI at time 2")

# sottraggo il secondo DVI al primo per capire di quanto è calato nel tempo e plotto il risultato con una scala di colori scelta
deltaDVI<- DVI1 - DVI2
cld <- colorRampPalette(c('blue','white','red'))(100) 
plot(deltaDVI, col=cld, main="differenza DVI")

## calcolo del NDVI = (NIR-RED)/(NIR+RED)
# (normalizzo il DVI per la somma delle due bande)
NDVI1<-(defor1$defor1.1-defor1$defor1.2)/(defor1$defor1.1+defor1$defor1.2) # applico l'operazione tra matrici specificando i layer corrispondenti alle bande con $
NDVI2<-(defor2$defor2.1-defor2$defor2.2)/(defor2$defor2.1+defor2$defor2.2)
par(mfrow=c(2,1))  # creo un multipanel con due righe dove plottare i due NDVI calcolati
plot(NDVI1, col=cl, main="NDVI at time 1")
plot(NDVI2, col=cl, main="NDVI at time 2")

deltaNDVI<- NDVI1 - NDVI2  # calcolo la differenza tra i due NDVI per valutare quanto è calato nel tempo
plot(deltaNDVI, col=cld, main="differenza DVI")  # plotto il risultato del deltaNDVI


## RStoolbox:: spectralIndices

vi1<-spectralIndices(defor1, green=3, red=2, nir=1)  # la funzione spectraIndices del pacchetto RStoolbox produce in output una serie di indici multispettrali,
                                                      # tra cui anche NDVI, calcolati sul raster di input
plot(vi1, col=cl) # plotto gli indici calcolati 

vi2 <- spectralIndices(defor2, green = 3, red = 2, nir = 1) # applico la funzione spectralIndices anche sulla seconda immagine per ottenere gli indici
plot(vi2, col=cl)  # plotto il secondo risultato

### worldwide NDVI
plot(copNDVI) # copNDVI è un RasterLayer che fa parte del pacchetto rasterdiv e rappresenta l'NDVI a scala globale
copNDVI<-raster::reclassify(copNDVI, cbind(253:255,NA))  # riclassifico l'immagine trasformando in NA (non valori) i pixel scelti, in questo caso l'acqua, 
                                                         # tramite la funzione cbind

levelplot(copNDVI) # plotto il risultato con i pixel acqua eliminati

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

# 9. R Code - Land Cover

### LAND COVER

# uso library per richiamare i pacchetti che servono nel codice e poterli utilizzare
library(raster)  # pacchetto con funzioni per elaborare file raster
library(RStoolbox)  # pacchetto con funzioni per processare le immagini (tra cui unsuperClass)
library(ggplot2)   # pacchetto con diverse funzioni per creare e modificare grafici
library(gridExtra)  # pacchetto con funzioni per lavorare con grafici (tra cui grid.arrange)

setwd("C:/lab/")  # definisco la working directory

# importo le due immagini come RasterBrick:
defor1 <- brick("defor1.jpg") # immagine più vecchia
defor2 <- brick("defor2.jpg")  # immagine più recente

# I layer e le corrispondenti bande sono:
# defor1.1 = NIR
# defor1.2 = red
# defor1.3 = green


par(mfrow=c(2,1)) # creo un multipanel di due righe dove plottare le immagini importate
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")  # plotto la prima immagine in falsi colori r=nir g=red b=green
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin") # plotto la seconda immagine in falsi colori r=nir g=red b=green

# plotto le due immagini in falsi colori con la funzione ggRGB e le associo a due variabili p1 e p2
p1<-ggRGB(defor1, r=1, g=2, b=3, stretch="Lin") 
p2<-ggRGB(defor2, r=1, g=2, b=3, stretch="Lin")
grid.arrange(p1,p2,nrow=2) # con grid.arrange plotto le due immagini insieme su due righe

# applico una classificazione alla prima immagine, più vecchia:
set.seed(42)  # la funzione set.seed permette di ottenere sempre lo stesso risultato
d1c <- unsuperClass(defor1, nClasses=2)  # applico una classificazione a due classi
cl <- colorRampPalette(c('black','green'))(100) # definisco una prima color palette
cl2 <- colorRampPalette(c('green','black'))(100) # definisco una seconda color palette
par(mfrow=c(2,1)) # creo un multipanel di due righe dove plottare la mappa di classificazione con le due diverse colorpalette
plot(d1c$map, col=cl)  # mappa di classificazione con la prima color palette
plot(d1c$map, col=cl2)  # mappa di classificazione con la seconda color palette

# applico lo stesso procedimento di classificaizone sulla seconda foto, più recente:
set.seed(42)  # la funzione set.seed permette di ottenere sempre lo stesso risultato
d2c<- unsuperClass(defor2, nClasses=2)   # applico una classificazione a due classi
par(mfrow=c(2,1))  # creo un multipanel di due righe dove plottare la mappa di classificazione con le due diverse colorpalette
plot(d2c$map, col=cl) # mappa di classificazione con la prima color palette
plot(d2c$map, col=cl2)  # mappa di classificazione con la seconda color palette


# applico una classifazione a 3 classi su entrambe le immagini
d1c3 <- unsuperClass(defor1, nClasses=3) # applico una classificazione a 3 classi sulla prima foto
d2c3 <- unsuperClass(defor2, nClasses=3)  # applico una classificazione a 3 classi sulla seconda foto
par(mfrow=c(2,1)) # creo un multipanel a due righe dove plottare le due mappe di classificazione ottenute
plot(d1c3$map) # mappa di classificazione prima immagine
plot(d2c3$map) # mappa di classificazione seconda immagine

#### CALCOLARE LE AREE DI FORESTA PERSE

# Ottenuta la classificazione posso calcolare la frequenza dei pixel nelle diverse classi:
freq(d1c$map)  # la funzione freq restituisce il numero di pixel che ricade in ogni classe
#     value  count
# [1,]     1 305922
# [2,]     2  35370

somma1<- 305922+35370 # sommo i pixel di ogni classe per conoscere il totale
prop1<-freq(d1c$map)/somma1 # divido i valori di pixel appartenenti ad ogni classe per il totale in modo da ottenere i valori proporzionali delle classi 
#       value     count
# [1,] 2.930042e-06 0.8963644
# [2,] 5.860085e-06 0.1036356

# dai valori proporzionali definisco le percentuali di ogni classe:
# classe 1 = 89,6% 
# classe 2 = 10,3%

# applicp lo stesso procedimento per la classificazione della seconda foto:
freq(d2c$map)
#      value  count
# [1,]     1 178312
# [2,]     2 164414

somma2<-178312+164414
prop2<-freq(d2c$map)/somma2
#            value     count
# [1,] 2.917783e-06 0.5202757
# [2,] 5.835565e-06 0.4797243

# classe 1 = 52%
# classe 2 = 47,9%


### GENERARE UN DATASET (DATAFRAME)

# per creare un dataset definisco le variabili che corrisponderanno alle colonne:
cover<-c("Forest","Agricolture") # applico alla variabile cover le etichette che voglio associare alle due classi
percent_1992<-c(89.64,10.36) # applico alla variabile percent_1992 i valori percentuali calcolati dalla classificazione della prima immagine
percent_2006<-c(52.03,47.97) # applico alla variabile percent_2006 i valori percentuali calcolati dalla classificazione della seconda immagine
percentages<-data.frame(cover,percent_1992,percent_2006)  # con la funzione data.frame creo un dataset contenente le variabili precedentemente definite
percentages  # richiamando la variabile mi restituisce il dataset costruito

# creo due grafici con i valori delle due classificazioni all'interno del dataser
g1<-ggplot(percentages, aes(x=cover,y=percent_1992,color=cover)) +  # con ggplot creo un grafico con i dati del dataset impostando i dati del 1992 sull'asse y 
                                                                                                                     # e le classi (le loro etichette) sull'asse x
            geom_bar(stat="identity", fill="white")   # geom_bar permette di relizzare un grafico a barre

g2<-ggplot(percentages, aes(x=cover,y=percent_2006,color=cover)) + geom_bar(stat="identity", fill="white") # creo un secondo grafico a barre con i dati del 2006 sull'asse y

# aes indica l'estetica del grafico
# geom indica il tipo di geometria con cui visualizzare il dato
# color indica a quali oggetti facciamo discriminare le classi
# stat="identity" indica che usiamo i dati del nostro dataset

grid.arrange(g1,g2,nrow=2)  # con grid.arrange plotto i due grafici su due righe

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

# 10. R Code - Variability

### VARIABILITA' SPAZIALE

# uso library per richiamare i pacchetti che servono nel codice e poterli utilizzare
library(raster)  # pacchetto con funzioni per elaborare file raster
library(RStoolbox)  # pacchetto con funzioni per processare le immagini (tra cui unsuperClass)
library(ggplot2)  # pacchetto con diverse funzioni per creare e modificare grafici
library(viridis)  # pacchetto con diverse palette di colori
library(gridExtra) # pacchetto con funzioni per lavorare con grafici (tra cui grid.arrange)

setwd("C:/lab/")  # definisco la working directory

sentinel<-brick("sentinel.png")  # importo l'immagine come oggetto RasterBrick

# Le layer presenti in questa immagine corrispondono alle bande:
#  sentinel.1= NIR , sentinel.2=RED , sentinel.3=GREEN

plotRGB(sentinel, r=1, g=2, b=3, stretch="lin")  # plotto l'immagine in falsi colori con r=nir g=red b=green
plotRGB(sentinel, r=2, g=1, b=3, stretch="lin")  # plotto l'immagine in falsi colori con r=red g=nir b=green

# calcolo l'indice NDVI 

# per separare le bande associo alle variabili NIR RED e GREEN i layer del RasterBrick corrispondenti a quelle bande
NIR<-sentinel$sentinel.1  # il primo layer è la banda NIR
RED<-sentinel$sentinel.2  # il secondo layer è la banda RED
GREEN<-sentinel$sentinel.3  # il secondo layer è la banda GREEN
NDVI=(NIR-RED)/(NIR+RED)  # calcolo NDVI secondo la sua formula 
plot(NDVI)  # visualizzo il risultato del calcolo di NDVI

cl <- colorRampPalette(c('black','white','red','magenta','green'))(100)  # definisco una color palette
plot(NDVI, col=cl)  # plotto il risultato del NDVI con la nuova color palette

### usare la funzione focal (da pacchetto raster) per analisi statistica utilizzando la moving window
# con w definisco le dimensioni della moving window, con fun la variabile statistica da calcolare

# Calcolo della deviazione standard

NDVI_sd3<-focal(NDVI, w=matrix(1/9,nrow=3,ncol=3), fun=sd) 
 # la funzione focal utilizza la tecnica della finestra mobile per calcolare diverse variabili statistiche in questo caso definiamo come variabile statistica di 
# interesse la deviazione standard (fun=sd) e definiamo la dimensione della finestra mobile 3x3 (1/9,nrow=3, ncol=3).  
# La finestra mobile calcola nel suo pixel centrale la variabile statistica delle celle circostanti, poi ripete il calcolo scorrendo lungo l'immagine
# Associo il risultato di questo calcolo alla funzione NDVI_sd3

cl2 <- colorRampPalette(c('blue','green','pink','magenta','orange','brown','red','yellow'))(100)  # definisco una color palette
plot(NDVI_sd3, col=cl2)  # plotto il risultato della deviazione standard con la scala colori scelta

# applico la funzione focal per calcolare la media
NDVI_m3<-focal(NDVI, w=matrix(1/9,nrow=3,ncol=3), fun=mean)
plot(NDVI_m3, col=cl2)  # visualizzo il risultato

# applico la funzione focal per calcolare la deviazione standar ma con una finestra mobile di dimensione 9x9
NDVI_sd9<-focal(NDVI, w=matrix(1/81,nrow=9,ncol=9), fun=sd)
plot(NDVI_sd9, col=cl2)  # visualizzo il risultato

# applico la funzione focal per calcolare la deviazione standar ma con una finestra mobile di dimensione 5x5
NDVI_sd5<-focal(NDVI, w=matrix(1/25,nrow=5,ncol=5), fun=sd)
plot(NDVI_sd5, col=cl2)  # visualizzo il risultato

# più piccola è la finestra mobile è maggiore è il dettaglio del risultato. A seconda del tipo di analisi che dobbiamo effettuare può esserci utile
# un grado maggiore o minore di dettaglio

sentinel_pca<-rasterPCA(sentinel)  # Applico un'analisi PCA all'immagine originale
plot(sentinel_pca$map)  # visualizzo la mappa risultate dall'analisi PCA
summary(sentinel_pca$model)  # visualizzo il sommario delle informazioni del modello dell'analisi PCA
# la PC1 spiega il 67,3% della variabilità totale

PC1<-sentinel_pca$map$PC1  # associo alla variabile PC1 il layer corrispondente all'interno della mappa prodotta dalla PCA
PC1_sd3<-focal(PC1, w=matrix(1/9,nrow=3,ncol=3), fun=sd)  # con la funzione focal calcolo la deviazione standard della PC1 con una finestra mobile 3x3
plot(PC1_sd3, col=cl2)  # visualizzo il risultato ottenuto

PC1_sd5<-focal(PC1, w=matrix(1/25,nrow=5,ncol=5), fun=sd)  # con la funzione focal calcolo la deviazione standard della PC1 con una finestra mobile 5x5
plot(PC1_sd5, col=cl2, main="PC1 standard deviation")  # visualizzo il risultato ottenuto

PC1_sd7<-focal(PC1, w=matrix(1/49,nrow=7,ncol=7), fun=sd)  # con la funzione focal calcolo la deviazione standard della PC1 con una finestra mobile 7x7
plot(PC1_sd7, col=cl2, main="PC1 standard deviation")  # visualizzo il risultato ottenuto

##########
# con ggplot creo un grafico dove visualizzo il risultato della deviazione standard calcolata con finestra mobile 5x5 con le diverse scale di colore fornite dal pacchetto 
# viridis:“viridis”, “magma”, “plasma”, “inferno”, “cividis”, “mako”, “rocket” ,“turbo” :

# geom_raster mi permette di plottare un oggetto raster
# scale_fill_viridis() mi permette di scegliere la palette dal pacchetto viridis
# con ggtitle aggiungo un titolo

p0 <- ggplot() + geom_raster(PC1_sd5, mapping = aes(x = x, y = y, fill = layer)) + scale_fill_viridis() +  ggtitle("viridis palette")

p1 <- ggplot() + geom_raster(PC1_sd5, mapping = aes(x = x, y = y, fill = layer)) + scale_fill_viridis(option="magma") +  ggtitle("magma palette")

p2 <- ggplot() + geom_raster(PC1_sd5, mapping = aes(x = x, y = y, fill = layer)) + scale_fill_viridis(option="plasma") +  ggtitle("plasma palette")

p3 <- ggplot() + geom_raster(PC1_sd5, mapping = aes(x = x, y = y, fill = layer)) + scale_fill_viridis(option="inferno") +  ggtitle("inferno palette")

p4 <- ggplot() + geom_raster(PC1_sd5, mapping = aes(x = x, y = y, fill = layer)) + scale_fill_viridis(option="cividis") +  ggtitle("cividis palette")

p5 <- ggplot() + geom_raster(PC1_sd5, mapping = aes(x = x, y = y, fill = layer)) + scale_fill_viridis(option="mako") +  ggtitle("mako palette")

p6 <- ggplot() + geom_raster(PC1_sd5, mapping = aes(x = x, y = y, fill = layer)) + scale_fill_viridis(option="rocket") + ggtitle("rocket palette")

p7 <- ggplot() + geom_raster(PC1_sd5, mapping = aes(x = x, y = y, fill = layer)) + scale_fill_viridis(option="turbo") + ggtitle("turbo palette")

grid.arrange(p0, p1, p2, p3, p4, p5, p6, p7, nrow = 2)  # con grid.arrange creo un grafico multipanel con due righe dove plotto 
                                                         #  tutte le mappe che ho associato alle variabili p0, p1, p2, p3, p4, p5, p6, p7

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

# 11. R Code - Spectral Signatures

## FIRMA SPETTRALE

# uso library per richiamare i pacchetti che servono nel codice e poterli utilizzare
library(raster) # pacchetto con funzioni per elaborare file raster
library(rgdal) # pacchetto con ulteriori funzioni per elaborare file raster
library(ggplot2) # pacchetto con diverse funzioni per creare e modificare grafici

setwd("C:/lab/")  # definisco la working directory

defor2<-brick("defor2.jpg")  # importo l'immagine come oggetto RasterBrick

# I layer che compongo il RasterBrick importato e le bande corripondenti sono:
# defor2.1 defor2.2 defor 2.3
# NIR       red      green

# per costruire la firma spettrale di un elemento devo sapere i suoi valori di riflettanza nelle diverse bande

plotRGB(defor2, r=1, g=2, b=3, stretch="lin")  # plotto l'immagine in falsi colori r=nir g=red b=green

click(defor2, id=T, xy=T, cell=T, type= "p", pch=16, col="yellow")  # con la funzione click posso interagire con l'immagine precedentemente plottata per riconoscere
                                                                     # i valori di riflettanza delle bande in un punto specifico cliccandovi sopra
 # xy=TRUE perchè lavoriamo su coordinate spaziali
# con type definisco il tipo di click (p=puntuale)
# pch è il simbolo del punto sulla mappa (16 e un tondo pieno)

# La funzione click a mano a mano che clicco restituisce le informazioni dei punti: le coordinate x e y e i valori di riflettanza nei diversi layer 
#                                                                                                              (e quindi nelle corrispettive bande)

# punto1 (all'interno di una zona di foresta):
#     x     y   cell     defor2.1   defor2.2  defor2.3
# 1 345.5 223.5 182464      202        3          8
#                           NIR       RED       GREEN

# punto 2 (sul corso d'acqua):
#      x     y   cell     defor2.1   defor2.2   defor2.3
# 1 191.5 178.5 214575        0         54         63
#                            NIR        RED       GREEN

# per creare un dataset definisco le variabili che corrisponderanno alle colonne::
band<-c(1,2,3)   # alla variabile band associo i numeri delle bande (1=nir 2=red 3=green)
forest<-c(202,3,8)  # alla variabile forest associo i valori di riflettanza nelle tre bande restituiti dal punto1
water<-c(0,54,63)  # alla variabile water associo i valori di riflettanza nelle tre bande restituiti dal punto2
spect_sign<-data.frame(band,forest,water)  # la funzione data.frame crea il dataset che le variabili definite prima

# con ggplot creo un grafico con gli elementi del dataset e lo associo alla variabile g1
g1<-ggplot(spect_sign, aes(x=band)) +  # richiamo il dataframe di cui voglio ottenere il grafico
              geom_line(aes(y=forest), color="green") +  # con geom_line realizzo un grafico di linee con i valori di forest sull'asse y e colore verde
              geom_line(aes(y=water), color="blue") +  # con geom_line realizzo un grafico di linee con i valori di water sull'asse y e colore blu
              labs(x="bande",y="riflettanza")  # definisco le etichette degli assi

g1 # richiamando la variabile visualizzo il grafico con le due firme spettrali, della foresta e dell'acqua, ovvero l'andamento della riflettanza nelle diverse bande



######
# ANALISI MULTITEMPORALE

defor1<-brick("defor1.jpg")  # importo l'immagine più vecchia come oggetto RasterBrick
defor2<-brick("defor2.jpg")  # importo l'immagine più recente come oggetto RasterBrick
              
# costruisco la firma spettrale dell'immagine più vecchia:
plotRGB(defor1, r=1, g=2, b=3, stretch="lin")  # apro il plot dell'immagine
click(defor1, id=T, xy=T, cell=T, type= "p", pch=16, col="yellow")   # con la funzione click prendo 6 punti di cui ottengo i valori di riflettanza 

# punto1
#  x     y   cell defor1.1 defor1.2 defor1.3
# 1 80.5 325.5 108609      218       12       32

# punto2
#      x     y   cell defor1.1 defor1.2 defor1.3
# 1 104.5 306.5 122199      207       10       27

# punto3
#      x     y   cell defor1.1 defor1.2 defor1.3
# 1 105.5 330.5 105064      217        6       25

# punto4
#     x     y   cell defor1.1 defor1.2 defor1.3
# 1 50.5 320.5 112149      205       18       35

# punto5
#     x     y  cell defor1.1 defor1.2 defor1.3
# 1 90.5 381.5 68635      215       23       38

# punto6
#     x     y  cell defor1.1 defor1.2 defor1.3
# 1 60.5 402.5 53611      206       20       44


# ripeto lo stesso procedimento sulla seconda immagine per ottenere la firma spettrale dell'immagine più recente
plotRGB(defor2, r=1, g=2, b=3, stretch="lin")
click(defor2, id=T, xy=T, cell=T, type= "p", pch=16, col="yellow")

# punto1
#  x     y   cell defor2.1 defor2.2 defor2.3
# 1 54.5 321.5 111907      186      108      108

# punto2
#     x     y   cell defor2.1 defor2.2 defor2.3
# 1 31.5 320.5 112601      177       10       18

# punto3
#     x     y  cell defor2.1 defor2.2 defor2.3
# 1 47.5 338.5 99711      174       97      103

# punto4
#     x     y  cell defor2.1 defor2.2 defor2.3
# 1 64.5 375.5 73199      180      100      101

# punto5
#     x     y  cell defor2.1 defor2.2 defor2.3
# 1 35.5 381.5 68868      214      177      168

# punto6
#     x     y  cell defor2.1 defor2.2 defor2.3
# 1 25.5 349.5 91802      189      163      166

## creo un dataset con i dati ottenuti
band<-c(1,2,3)  # i numeri delle bande (1=nir 2=red 3=green)
defor1_1<-c(218,12,32)  # i valori di rflettanza delle tre bande di tutti i punti delle due foto
defor2_1<-c(186,108,108)
defor1_2<-c(207,10,27)
defor2_2<-c(177,10,18)
defor1_3<-c(217,6, 25)
defor2_3<-c(174,97,103)
defor1_4<-c( 205,18,35)
defor2_4<-c(180,100,101)
defor1_5<-c(215,23,38)
defor2_5<-c(214,177, 168)
defor1_6<-c(206,20,44)
defor2_6<-c(189,163,166)
multitemp<-data.frame(band,defor1_1,defor2_1, defor1_2,defor2_2,
                     defor1_3,defor2_3, defor1_4,defor2_4,
                     defor1_5,defor2_5, defor1_6,defor2_6)   # con data.frame creo il dataset con tutte le variabili definite 

# con ggplot creo un grafico che unisca tutte le firme spettrali dei 12 punti analizzati (6 nella immagine più vecchia, in verde, e 6 in quella più recente, in blu)
ggplot(multitemp, aes(x=band)) +
              geom_line(aes(y=defor1_1), color="green") +  # con geom_line inserisco le curve delle firme spettrali (grafico a linee)
              geom_line(aes(y=defor2_1), color="blue") +
              geom_line(aes(y=defor1_2), color="green") +
               geom_line(aes(y=defor2_2), color="blue") +
                geom_line(aes(y=defor1_3), color="green") +
               geom_line(aes(y=defor2_3), color="blue") +
                geom_line(aes(y=defor1_4), color="green") +
               geom_line(aes(y=defor2_4), color="blue") +
              geom_line(aes(y=defor1_5), color="green") +
               geom_line(aes(y=defor2_5), color="blue") +
        geom_line(aes(y=defor1_6), color="green") +
               geom_line(aes(y=defor2_6), color="blue") +
              labs(x="bande",y="riflettanza")  # inserisco i titoli degli assi



# con lo stesso procedimento costruisco la firma spettrale di un'altra immagine 

loza<-brick("loza.jpg")  # importo l'immagine come RasterBrick
plotRGB(loza, r=1, g=2, b=3, stretch="hist")  # apro il plot dell'immagine
click(loza, id=T, xy=T, cell=T, type= "p", pch=16, col="yellow")  # con la funzione click seleziono i punti di cui ottenere i valori di rifettanza

# punto1
# x      y    cell loza.1 loza.2 loza.3
# 1 3869.5 2975.5 4101918     43     85     75

# punto2
#       x      y     cell loza.1 loza.2 loza.3
# 1 2389.5 1838.5 10431254    248    223    183

# punto3
#      x      y    cell loza.1 loza.2 loza.3
# 1 796.5 2329.5 7695773      0     19     20

# punto4
#       x      y     cell loza.1 loza.2 loza.3
# 1 3040.5 1330.5 13260449    150     89     68

# definisco le variabili con cui costruire il dataset:
band<-c(1,2,3) # i numeri delle bande (1=nir 2=red 3=green)
zona1<-c(43,85,75) # i valori di rflettanza delle tre bande di tutti i punti delle due foto
zona2<-c(248,223,183)
zona3<-c(0,19,20)
zona4<-c(150,89,68)
tab<-data.frame(band,zona1,zona2,zona3,zona4)  # con data.frame creo il dataset con i valori di riflettanza nelle tre bande dei quattro punti selezionati

# con ggplot creo un grafico che unisca tutte le firme spettrali dei quattro punti analizzati
ggplot(multitemp, aes(x=band)) +
              geom_line(aes(y=zona1), color="green") + # con geom_line inserisco le curve delle firme spettrali (grafico a linee)
              geom_line(aes(y=zona2), color="blue") +
              geom_line(aes(y=zona3), color="red") +
               geom_line(aes(y=zona4), color="magenta") +
              labs(x="bande",y="riflettanza")  # inserisco i titoli degli assi

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#


# 12. R Code - NO2

# ESERCITAZIONE SULLA DIFFERENZA DI NO2 IN ATMOSFERA TRA GENNAIO E MARZO 2020  

# 1. set working directory su EN
# 2. import the first image (single band)
# 3. plot the first image with color ramp palette
# 4. import the last image and plot it
# 5. make the difference between the two images and plot it
# 6. plot everything altogether
# 7. import the all set
# 8. replicate plot images 1 and 13 using the stack
# 9. compute principle component analysis
# 10. compute of local variability (standard deviation) of PC1


library(raster)  # pacchetto con funzioni per elaborare file raster


#----------- 1. set working directory su EN  -----------#

setwd("C:/lab/EN") # definisco la working directory


#-----------  2. import the first image (single band)  -----------#

EN01<-raster("EN_0001.png")  # importo la prima immagine (NO2 a Gennaio) come oggetto raster


#-----------  3. plot the first image with color ramp palette  -----------#

cl<-colorRampPalette(c("blue","green","yellow")) (100)  # definisco una color palette
plot(EN01, col=cl)  # visualizzo l'immagine importata con la color palette scelta


#----------- 4. import the last image and plot it  -----------#

EN13<-raster("EN_0013.png")  # importo la seconda immagine (NO2 a Marzo) come oggetto raster
plot(EN13, col=cl)  # visualizzo l'immagine importata con la color palette scelta


#----------- 5. make the difference between the two images and plot it  -----------#

deltaEN<-EN01-EN13  # calcolo la differenza tra i valori di NO2 di Gennaio (EN01) e Marzo (EN13)
plot(deltaEN, col=cl)  # visualizzo il risultato ottenuto


#----------- 6.  plot everything altogether  -----------#

par(mfrow=c(3,1)) # imposto un multipanel su 3 righe dove plottare le immagini
plot(EN01,col=cl, main="NO2 in January")  # mappa di NO2 a Gennaio
plot(EN13,col=cl, main="NO2 in March")  # mappa di NO2 a Marzo
plot(deltaEN, col=cl, main="Difference between January and march") # mappa della differenza di NO2 tra Gennaio e Marzo


#----------- 7. import the all set  -----------#

# per importare tuti i file in una stessa variabile seguo il procedimento:
rlist<-list.files(pattern="EN") #creo una lista di file con pattern "EN" e associo alla variabile rlist
import<-lapply(rlist,raster)  # con lapply applico la funzione raster su tutti i file della lista e li importo associati alla variabile import
EN<-stack(import) # con stack creo un unico oggetto RasterStack che contiene come layer tutti i file importati


#----------- 8. replicate plot images 1 and 13 using the stack -----------#

par(mfrow=c(2,1)) # imposto un multipanel su 2 righe dove plottare le immagini
plot(EN$EN_0001,col=cl, main="NO2 in January") # mappa di NO2 a Gennaio presa dal layer corrispondente del RasterStack tramite $
plot(EN$EN_0013,col=cl, main="NO2 in March")  # mappa di NO2 a Marzo presa dal layer corrispondente del RasterStack tramite $


#----------- 9. compute principle component analysis  -----------#

EN_PCA<-rasterPCA(EN) # la funzione rasterPCA applica un'analisi PCA e produce un oggetto che contiene la mappa e le informazioni sul modello
summary(EN_PCA$model) # la funzione summary fa un sommario delle info del modello generato dall'analisi di rasterPCA, con le percentuali di varianza spiegate da ogni PC
plotRGB(EN_PCA$map, r=1, g=2, b=3, stretch='lin') # plotto in RGB le prime tre componenti principali generate dall'analisi PCA 


#----------- 10. compute of local variability (standard deviation) of PC1  -----------#

PC1<-EN_PCA$map$PC1 # associo alla variabile PC1 il layer corrispondente all'interno della mappa prodotta dalla PCA
PC1_sd3<-focal(PC1, w=matrix(1/9,nrow=3,ncol=3), fun=sd) # con la funzione focal calcolo la deviazione standard della PC1 con una finestra mobile 3x3
plot(PC1_sd3, col=cl) # visualizzo il risultato ottenuto



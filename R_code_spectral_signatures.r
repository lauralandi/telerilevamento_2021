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

# con ggplot creo un grafico che unica tutte le firme spettrali dei 12 punti analizzati (6 nella immagine più vecchia, in verde, e 6 in quella più recente, in blu)
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
band<-c(1,2,3)
zona1<-c(43,85,75)
zona2<-c(248,223,183)
zona3<-c(0,19,20)
zona4<-c(150,89,68)
tab<-data.frame(band,zona1,zona2,zona3,zona4)  # con data.frame creo il dataset con i valori di riflettanza nelle tre bande dei quattro punti selezionati

# costruisco il grafico a linee con i valori dei quattro punti selezionati
ggplot(multitemp, aes(x=band)) +
              geom_line(aes(y=zona1), color="green") +
              geom_line(aes(y=zona2), color="blue") +
              geom_line(aes(y=zona3), color="red") +
               geom_line(aes(y=zona4), color="magenta") +
              labs(x="bande",y="riflettanza")

      

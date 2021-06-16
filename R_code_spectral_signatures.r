## FIRMA SPETTRALE

library(raster)
library(rgdal)
library(ggplot2)

setwd("C:/lab/")

defor2<-brick("defor2.jpg")

# defor2.1 defor2.2 defor 2.3
# NIR       red      green

plotRGB(defor2, r=1, g=2, b=3, stretch="lin")

click(defor2, id=T, xy=T, cell=T, type= "p", pch=16, col="yellow")
    # xy TRUE perchè lavoriamo su coordinate spaziali
# type è tipo di click (p=puntuale)
# pch è il simbolo (cerca su google immagini)
# nell'usare la funzione click il plot deve rimanere aperto

#     x     y   cell defor2.1 defor2.2 defor2.3
# 1 345.5 223.5 182464      202        3        8
#      x     y   cell defor2.1 defor2.2 defor2.3
# 1 191.5 178.5 214575        0       54       63

## creo un dataframe
# prima definisco le colonne (in questo caso tre):
band<-c(1,2,3)
forest<-c(202,3,8)
water<-c(0,54,63)
spect_sign<-data.frame(band,forest,water)

# plottiamo un grafico con questi dati
ggplot(spect_sign, aes(x=band)) +
              geom_line(aes(y=forest), color="green") +
              geom_line(aes(y=water), color="blue") +
              labs(x="bande",y="riflettanza")

######
# ANALISI MULTITEMPORALE

defor1<-brick("defor1.jpg")

              
# spectral signature defor1
plotRGB(defor1, r=1, g=2, b=3, stretch="lin")
click(defor1, id=T, xy=T, cell=T, type= "p", pch=16, col="yellow")    

#  x     y   cell defor1.1 defor1.2 defor1.3
# 1 80.5 325.5 108609      218       12       32
#      x     y   cell defor1.1 defor1.2 defor1.3
# 1 104.5 306.5 122199      207       10       27
#      x     y   cell defor1.1 defor1.2 defor1.3
# 1 105.5 330.5 105064      217        6       25
#     x     y   cell defor1.1 defor1.2 defor1.3
# 1 50.5 320.5 112149      205       18       35
#     x     y  cell defor1.1 defor1.2 defor1.3
# 1 90.5 381.5 68635      215       23       38
#     x     y  cell defor1.1 defor1.2 defor1.3
# 1 60.5 402.5 53611      206       20       44


# spectral signature defor2
plotRGB(defor2, r=1, g=2, b=3, stretch="lin")
click(defor2, id=T, xy=T, cell=T, type= "p", pch=16, col="yellow")

#  x     y   cell defor2.1 defor2.2 defor2.3
# 1 54.5 321.5 111907      186      108      108
#     x     y   cell defor2.1 defor2.2 defor2.3
# 1 31.5 320.5 112601      177       10       18
#     x     y  cell defor2.1 defor2.2 defor2.3
# 1 47.5 338.5 99711      174       97      103
#     x     y  cell defor2.1 defor2.2 defor2.3
# 1 64.5 375.5 73199      180      100      101
#     x     y  cell defor2.1 defor2.2 defor2.3
# 1 35.5 381.5 68868      214      177      168
#     x     y  cell defor2.1 defor2.2 defor2.3
# 1 25.5 349.5 91802      189      163      166

## creo dataset
band<-c(1,2,3)
defor1_1<-c(218,12,32)
defor2_1<-c(186,108,108)
defor1_2<-c(207,10,27)
defor2_2<-c(177,10,18)
multitemp<-data.frame(band,defor1_1,defor2_1, defor1_2,defor2_2)

# plotto il grafico
ggplot(multitemp, aes(x=band)) +
              geom_line(aes(y=defor1_1), color="green") +
              geom_line(aes(y=defor2_1), color="blue") +
              geom_line(aes(y=defor1_2), color="green") +
               geom_line(aes(y=defor2_2), color="blue") +
              labs(x="bande",y="riflettanza")



### prova con altra immagine
loza<-brick("loza.jpg")
plotRGB(loza, r=1, g=2, b=3, stretch="hist")
click(loza, id=T, xy=T, cell=T, type= "p", pch=16, col="yellow")

# x      y    cell loza.1 loza.2 loza.3
# 1 3869.5 2975.5 4101918     43     85     75
#       x      y     cell loza.1 loza.2 loza.3
# 1 2389.5 1838.5 10431254    248    223    183
#      x      y    cell loza.1 loza.2 loza.3
# 1 796.5 2329.5 7695773      0     19     20
#       x      y     cell loza.1 loza.2 loza.3
# 1 3040.5 1330.5 13260449    150     89     68

band<-c(1,2,3)
zona1<-c(43,85,75)
zona2<-c(248,223,183)
zona3<-c(0,19,20)
zona4<-c(150,89,68)
tab<-data.frame(band,zona1,zona2,zona3,zona4)

ggplot(multitemp, aes(x=band)) +
              geom_line(aes(y=zona1), color="green") +
              geom_line(aes(y=zona2), color="blue") +
              geom_line(aes(y=zona3), color="red") +
               geom_line(aes(y=zona4), color="magenta") +
              labs(x="bande",y="riflettanza")
       
      

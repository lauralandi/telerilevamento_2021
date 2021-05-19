### VARIABILITA' SPAZIALE

library(raster)
library(RStoolbox)
library(ggplot2)
library(viridis)
library(gridExtra)

setwd("C:/lab/")

sentinel<-brick("sentinel.png")

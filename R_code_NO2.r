# R_code_NO2.r

# 1. set working directory su EN
# 2. import the first image (single band)
# 3. plot the first image with color ramp palette
# 4. import the last image and plot it
# 5. make the difference between the two images and plot it
# 6. plot everything altogether
# 7. import the all set
# 8. replicate plot images 1 and 13 using the stack


library(raster) 
library(RStoolbox) 
library(ggplot2) 
library(rasterVis)
library(knitr)
library(ncdf4)
library(gridExtra)
library(viridis)

# 1.
setwd("C:/lab/EN")

# 2.
EN01<-raster("EN_0001.png")

# 3.
cl<-colorRampPalette(c("blue","green","yellow")) (100)
plot(EN01, col=cl)

# 4.
EN13<-raster("EN_0013.png")
plot(EN13, col=cl)

# 5.
deltaEN<-EN01-EN13
plot(deltaEN, col=cl)

# 6.
par(mfrow=c(3,1)) # 2 righe e 1 colonna
plot(EN01,col=cl, main="NO2 in January")
plot(EN13,col=cl, main="NO2 in March")
plot(deltaEN, col=cl, main="Difference between January and march")

# 7.
rlist<-list.files(pattern="EN") 
import<-lapply(rlist,raster) 
EN<-stack(import)

# 8.
par(mfrow=c(2,1)) # 2 righe e 1 colonna
plot(EN$EN_0001,col=cl, main="NO2 in January")
plot(EN$EN_0013,col=cl, main="NO2 in March")



# R_code_NO2.r

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

# 9.

EN_PCA<-rasterPCA(EN)
summary(EN_PCA$model)
plotRGB(EN_PCA$map, r=1, g=2, b=3, stretch='lin')

# 10.

PC1<-EN_PCA$map$PC1
PC1_sd3<-focal(PC1, w=matrix(1/9,nrow=3,ncol=3), fun=sd)
plot(PC1_sd3, col=cl)










### REPORT IN KNITR

setwd("C:/lab/") # definisco la working directory

library(knitr)  # il pacchetto knitr permette di realizzare dei report

stitch("R_code_greenland.r", template=system.file("misc", "knitr-template.Rnw", package="knitr")) # la funzione stitch crea un report automatico sulla base di uno
                                                                                                  # script R, salvato nella wd e definito dal suo path, e un template  


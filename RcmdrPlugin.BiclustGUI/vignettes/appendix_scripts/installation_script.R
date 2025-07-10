# Project: BiclustGUI
# 
# Author: lucp8394
###############################################################################


install.packages("biclust")
#install.packages("BcDiag")
install.packages("BcDiag", repos="http://R-Forge.R-project.org") # More up to date at the moment than the CRAN release
install.packages("superbiclust")
install.packages("Rcmdr")
install.packages("isa2")

source("http://bioconductor.org/biocLite.R")
biocLite("iBBiG")
biocLite("fabia")
biocLite("rqubic")
biocLite("BicARE")

install.packages("RcmdrPlugin.BiclustGUI", repos="http://R-Forge.R-project.org")

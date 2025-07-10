#############################################
## AUTOMATIC INSTALLATION FOR CRAN RELEASE ##
#############################################

setRepositories(ind=c(1:5))
install.packages("RcmdrPlugin.BiclustGUI")


#################################################
## MANUAL INSTALLATION FOR DEVELOPMENT RELEASE ##
#################################################

## PACKAGES AVAILABLE ON CRAN ##
install.packages("biclust")
install.packages("BcDiag")
install.packages("superbiclust")
install.packages("Rcmdr")
install.packages("isa2")
install.packages("s4vd")


install.packages("gplots") # Extra package

## PACKAGES AVAILABLE ON BIOCONDUCTOR ##
source("http://bioconductor.org/biocLite.R")
biocLite("iBBiG")
biocLite("fabia")
biocLite("rqubic")
biocLite("BicARE")

## BiclustGUI - Release Version ##

install.packages("RcmdrPlugin.BiclustGUI")

## Biclust GUI - In Development Version ##
install.packages("RcmdrPlugin.BiclustGUI",
		repos="http://R-Forge.R-project.org")

## Biclust GUI (ISA2 VERSION) - In Development Version ##
install.packages("RcmdrPlugin.BiclustGUI.Extra",
		repos="http://R-Forge.R-project.org")


############################################################
## Note on initial launch of Rcmdr: 
## You might be prompted to isntall a few additional packages

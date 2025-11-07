## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(cowplot)

## ----setup--------------------------------------------------------------------
library(andurinha)

## ----echo=FALSE, fig.retina=5, fig.width=7.1, fig.align='center'--------------
ggplot() +
  geom_blank() +
  geom_segment(aes(x = c(4.1, 8.1, 12.1), xend = c(4.9, 8.9, 12.9), y = .25, yend = .25),
               arrow = arrow(length = unit(.3, "cm"))) +
  annotate("rect", 
           xmin = c(1, 5, 9, 13), xmax = c(4, 8, 12, 16),
           ymin = 0, ymax = 0.5,
           color = c("white", "white", "white", "cyan3"),
           fill = c("grey", "orange", "grey", "grey88"), alpha = .5) +
  annotate("text", 
           x = c(2.5, 6.5, 10.5, 14.5), 
           y = 0.25, 
           label = c("Import", "Find peaks", "Visualisation", "Peaks \n selection")) +
  scale_x_discrete() +
  scale_y_discrete() +
  theme_void() 


## ----eval=FALSE---------------------------------------------------------------
#  spectra <- importSpectra(path = tempdir(), sep = ";")
#  head(spectra)
#  
#  #>    WN     A     B     C
#  #> 1 399 0.011 0.008 0.009
#  #> 2 401 0.008 0.006 0.006
#  #> 3 403 0.006 0.005 0.006
#  #> 4 405 0.005 0.005 0.005
#  #> 5 407 0.005 0.005 0.003
#  #> 6 409 0.003 0.004 0.002

## -----------------------------------------------------------------------------
# Search peaks based on absorbance sum spectrum
# with standarised absorbance data
fp.abs <- findPeaks(andurinhaData, ndd = FALSE)
summary(fp.abs)
dim(fp.abs$sumSpectrum_peaksTable)

# Search peaks based on second derivative sum spectrum
# with standarised absorbance data
fp.ndd <- findPeaks(andurinhaData)
summary(fp.ndd)
dim(fp.ndd$sumSpectrum_peaksTable)

# Search peaks based on second derivative sum spectrum
# with no standarised absorbance data
fp.nZs <- findPeaks(andurinhaData, scale = FALSE)
summary(fp.nZs)
dim(fp.nZs$sumSpectrum_peaksTable)

## ----fig.retina=5, fig.width=7.1, fig.height=5.5, fig.align='center'----------
# Graphic overview of the raw data
gOverview(andurinhaData)

## ----fig.retina=5, fig.width=7.1, fig.height=7.5, fig.align='center'----------
# Graphic overview of the processed data
# Peaks searched based on the second derivative sum spectrum
# with standarised absorbance data
gOverview(fp.ndd$dataZ, fp.ndd$secondDerivative)

## ----fig.retina=5, fig.width=7.1, fig.height=3, fig.align='center'------------
# Peaks searched based on absorbance sum spectrum
plotPeaks(fp.abs[[3]]$WN, 
          data_abs = fp.abs$dataZ)

## ----fig.retina=5, fig.width=7.1, fig.height=6, fig.align='center'------------
# Peaks searched based on the second derivative sum spectrum
plotPeaks(fp.ndd[[4]]$WN, 
          data_abs = fp.ndd$dataZ, 
          data_ndd = fp.ndd$secondDerivative)

## -----------------------------------------------------------------------------
# Select cutOff based on absorbance sum spectrum 
# to clean your peaks table
round(fp.abs$sumSpectrum_peaksTable, 2) %>% 
   arrange(desc(sumSpectrum))
# In that case cleaning may not be necesary

# Select cutOff based on second derivative sum spectrum 
# to clean your peaks table
round(fp.ndd$sumSpectrum_peaksTable, 2) %>% 
   arrange(desc(sumSpectrum)) %>%
   filter(sumSpectrum > 0.18)
# In that case a cut off of 0.25 my be selected

## -----------------------------------------------------------------------------
# Run finPeaks() with the new cutOff
# based on the second derivative sum spectrum
fp.ndd2 <- findPeaks(andurinhaData, cutOff = 0.25)

## ----fig.retina=5, fig.width=7.1, fig.height=3, fig.align='center'------------
# plotPeaks
# based on absorbance sum spectrum
# no cleaning needed
plotPeaks(fp.ndd2[[3]]$WN,
          data_abs = fp.ndd2$dataZ)

## ----fig.retina=5, fig.width=7.1, fig.height=6, fig.align='center'------------
# plotPeaks
# based on the second derivative sum spectrum
plotPeaks(fp.ndd2[[4]]$WN,
          data_abs = fp.ndd2$dataZ,
          data_ndd = fp.ndd2$secondDerivative)


## ----install, eval=FALSE------------------------------------------------------
#  install.packages("earlywarnings")

## ----install2, eval=FALSE-----------------------------------------------------
#  library(devtools)
#  install_github("earlywarningtoolbox/earlywarnings-R/earlywarnings")

## ----loading, eval=TRUE-------------------------------------------------------
library(earlywarnings)  

## ----movpotential, message=FALSE, warning=FALSE, fig.width=5, fig.height=5----
# Create simulated example data
X <- c(rnorm(1000, mean = 0), rnorm(1000, mean = -2), 
 	           rnorm(1000, mean = 2))
param <- seq(0,5,length=3000) 

# Run potential analysis
res <- movpotential_ews(X, param)

# Visualize
p <- PlotPotential(res$res, title = '', 
	       	   xlab.text = '', ylab.text = '', 
		   cutoff = 0.5, plot.contours = TRUE, binwidth = 0.2)
print(p)

## ----citation-----------------------------------------------------------------
citation("earlywarnings")

## ----sessioninfo--------------------------------------------------------------
sessionInfo()


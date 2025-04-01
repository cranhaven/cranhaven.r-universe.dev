\name{mammary}
\alias{mammary}
\title{Microarray gene expression time course data for mammary gland development in mice}
 \description{
  The data contains 30 genes identified using cluster analysis from 12488 probe
  sets representing approximately 8600 genes.  The data are measured over 54 arrays of   
  3 replicates each on 18 time points 
  of the developmental stages of mammary gland in mice. 
  See Stein et al. (2004) for more details.
  }
\usage{
data(mammary)
}
\format{
   Data is in longitudinal format with 30 columns, 54 rows and a number of extra
   attributes  (see R package longitudinal).
}
\source{
  This data is described in Stein et al. (2004) and can be freely
  obtained from the R package smida.
}
\references{
 Stein T, Morris J, Davies C, Weber Hall S, Duffy M, Heath V, Bell A, Ferrier R,    
   Sandilands G, Gusterson B, et al. (2004). Involution of the mouse mammary gland is 
   associated with an immune cascade and an acute phase response, involving LBP, CD14 
   and STAT3. Breast Cancer Res, 6(2), R 75 - 91.

 Wit E. and McClure J. (2004). Statistics for Microarrays: Design, Analysis and 
   Inference. Wiley.
}
\examples{
# load "longitudinal" library
library(longitudinal)

# load data sets
data(mammary)
}


\name{lung73}
\alias{lung73}
\alias{lung73.pvclust}
\alias{lung73.sb}
\alias{lung.pvclust}
\alias{lung.sb}
\docType{data}
\title{Clustering of 73 Lung Tumors}
\description{
  Bootstrapping hierarchical clustering of the DNA microarray data set
  of 73 lung tissue samples each containing 916 observed genes.
}
\usage{
data(lung73)
}
\format{
  \code{lung73.pvclust} and \code{lung.pvclust} are objects of class \code{"pvclust"}
  defined in \pkg{pvclust} of Suzuki and Shimodaira (2006).

  \code{lung73.sb} and \code{lung.sb} are an object of class \code{"scalebootv"} of length
  72.
}
\details{
  The microarray dataset of Garber et al. (2001) is reanalyzed in Suzuki
  and Shimodaira (2006), and is found in \code{data(lung)} of
  the \pkg{pvclust} package. We reanalyze it, again, by the script shown in
  Examples. The result of \code{pvclust} is stored in
  \code{lung73.pvclust} and \code{lung.pvclust}, and model fitting to bootstrap probabilities
  by the \pkg{scaleboot} package
  is stored in \code{lung73.sb} and \code{lung.sb}.
  A wide scale range is used in lung73.pvclust and lung73.sb, and the default scale range of pvclust is used in lung.pvclust and lung.sb.
 The microarray
  dataset is not included in \code{data(lung73)}, but it is found in
  \code{data(lung)} of the \pkg{pvclust} package.
}
\source{
  Garber, M. E. et al. (2001)
  Diversity of gene expression in adenocarcinoma of the lung,
  \emph{Proceedings of the National Academy of Sciences},
  98, 13784-13789 (dataset is available from
  \url{http://genome-www.stanford.edu/lung_cancer/adeno/}).
}
\references{
Suzuki, R. and Shimodaira, H. (2006).
pvclust: An R package for hierarchical clustering with p-values,
\emph{Bioinformatics}, 22, 1540-1542 (software is available from
CRAN or
\url{http://stat.sys.i.kyoto-u.ac.jp/prog/pvclust/}).
}
\seealso{\code{\link{sbpvclust}}, \code{\link{sbfit.pvclust}}}

\examples{
\dontrun{
## Parallel setup
library(parallel)
length(cl <- makeCluster(detectCores()))
## script to create lung73.pvclust and lung73.sb
## multiscale bootstrap resampling of hierarchical clustering
library(pvclust)
data(lung)
### default pvclust scales
lung.pvclust <- pvclust(lung, nboot=10000, parallel=cl)
lung.sb <- sbfit(lung.pvclust,cluster=cl) # model fitting
### wider range of scales than pvclust default
sa <- 9^seq(-1,1,length=13) 
lung73.pvclust <- pvclust(lung,r=1/sa,nboot=10000,parallel=cl) 
lung73.sb <- sbfit(lung73.pvclust,cluster=cl) # model fitting
}

## replace si/au/bp entries in pvclust object
library(pvclust)
data(lung73) # loading the previously computed bootstrap

### the original pvclust result
plot(lung.pvclust, print.pv = c("si", "au", "bp"), cex=0.5, cex.pv=0.5)
pvrect(lung.pvclust, pv="si") #  (defualt pvclust uses pv="au")

### default pvclust scales with p-values of k=2
lung.k2 <- sbpvclust(lung73.pvclust,lung73.sb, k=2)
plot(lung.k2, print.pv = c("si", "au", "bp"), cex=0.5, cex.pv=0.5)
pvrect(lung.k2, pv="si")

### wider scales with p-values of k=3 (default of scaleboot)
lung73.k3 <- sbpvclust(lung73.pvclust,lung73.sb)
plot(lung73.k3, print.pv = c("si", "au", "bp"), cex=0.5, cex.pv=0.5)
pvrect(lung73.k3, pv="si")

## diagnostics of fitting

### diagnose edges 61,...,69
lung73.sb[61:69] # print fitting details
plot(lung73.sb[61:69]) # plot curve fitting
summary(lung73.sb[61:69]) # print raw(=bp)/si/au p-values

### diagnose edge 67
lung73.sb[[67]] # print fitting
plot(lung73.sb[[67]],legend="topleft") # plot curve fitting
summary(lung73.sb[[67]]) # print au p-values

}
\keyword{datasets}

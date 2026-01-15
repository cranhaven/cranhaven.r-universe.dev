\name{profileBic}
\alias{profileBic}
\title{
The  profileBic function.
}
\description{Provides profile plots for biclustered and clustered data.
}
\usage{
profileBic(dset, bres, mname = c("fabia", "isa2", "biclust","bicare"), bplot = "all",
gby = "genes", bnum = 1, teta = 120, ph = 30, fabia.thresZ=0.5,fabia.thresL=NULL,
BClabel=TRUE,gene.lines=NULL,condition.lines=NULL)
}
\arguments{
  \item{dset}{data matrix.}
  \item{bres}{biclustering result.}
  \item{mname}{method name; 'biclust', 'isa2', 'fabia' or 'bicare'.}
  \item{bplot}{types of plots; 'all','lines', 'boxplot', 'histogram' or '3D'.}
  \item{gby}{grouped by; 'genes', or 'conditions'.}
  \item{bnum}{Existing biclusters; '1','2',...}
  \item{teta}{numerical value to rotate the 3D; 0, 90, 180,...}
  \item{ph}{numerical value to rotate the 3D; 0, 90, 180,...}
  \item{fabia.thresZ}{Bicluster threshold for \code{mname="fabia"}. Threshold for sample belonging to bicluster; default 0.5.}
  \item{fabia.thresL}{Bicluster threshold for \code{mname="fabia"}. Threshold for loading belonging to bicluster (if not given it is estimated).}
  \item{BClabel}{\code{TRUE}/\code{FALSE} to show BC labels on the lines plot.}
  \item{gene.lines}{Vector of indices or names of genes inside of Bicluster \code{bnum}. These gene profiles will be highlighted in the line plot (\code{bplot='lines'}).}
  \item{condition.lines}{Vector of indices or names of conditions inside of Bicluster \code{bnum}. These condition profiles will be highlighted in the line plot (\code{bplot='lines'}).}
  
}
\details{
The profile.bic function checks if all parameters are correctly submitted and then identifies the biclustered and clustered data.

Note that the \code{"biclust"} option for \code{mname} will also accept results from the packages \pkg{iBBiG} and \pkg{rqubic}.}
\value{
profile.bic(dset, bres, mname="biclust", bplot="all", gby="genes", bnum=1, teta=120, ph=30)
}
\references{
Van't Veer, L.J., Dai, H., van de Vijver, M.J., He, Y.D., Hart, A.A. et al. (2002). Gene expression profiling predicts clinical outcome of breast cancer,\emph{Nature}, 415, 530-536.

Kaiser S. and Leisch F. (2008). A Toolbox for Bicluster Analysis in R. \emph{Ludwigstrasse.} 33.
}

\author{Mengsteab Aregay \email{mycs.zab@gmail.com}}

\examples{
# create the biclustering result
data(breastc)
library(biclust)
bic<- biclust(breastc, method=BCPlaid())
# 3 biclusters found

# 3D profile plot for biclustered and clustered data.
profileBic(dset=breastc,bres=bic,mname="biclust",
bplot="3D",gby="genes",teta=-30,ph=50,bnum=1)
}

\keyword{profileBic}


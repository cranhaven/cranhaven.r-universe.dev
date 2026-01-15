\name{writeBic}
\alias{writeBic}
\title{The writeBic function} 

\description{Provides a summary output in a text format, extracted from 'biclust','isa2' and 'fabia' bicluster algorithms. }

\usage{
writeBic(dset, fileName, bicResult, bicname, 
mname = c("fabia", "isa2", "biclust","bicare"), append = TRUE, delimiter = " ", 
fabia.thresZ=0.5,fabia.thresL=NULL)
}

\arguments{
  \item{dset}{data matrix}
  \item{fileName}{the name of the bicluster file to be saved.}
  \item{bicResult}{bicluster result obtained from 'biclust','isa2' or 'fabia'}
  \item{bicname}{the title to be given for the biclustered data.}
  \item{mname}{method name; 'biclust', 'isa2', 'fabia' or 'bicare'}
  \item{append}{logical value; TRUE as default}
  \item{delimiter}{delimiter in created output file; default value is " ".}
  \item{fabia.thresZ}{Bicluster threshold for \code{mname="fabia"}. Threshold for sample belonging to bicluster; default 0.5.}
  \item{fabia.thresL}{Bicluster threshold for \code{mname="fabia"}. Threshold for loading belonging to bicluster (if not given it is estimated).}
}

\details{The original function was developed in 'biclust' package by Kaiser \emph{et.al} (2008). We extend the function to be used for further bicluster algorithms, such as; 'isa2', 'fabia' and 'bicare'.

Note that the \code{"biclust"} option for \code{mname} will also accept results from the packages \pkg{iBBiG} and \pkg{rqubic}.
}
\value{Biclustered text file with title, total number of biclustered, dimension and name of the biclustered genes(rows) or conditions(columns).}

\references{
Van't Veer, L.J., Dai, H., van de Vijver, M.J., He, Y.D., Hart, A.A. et al. (2002). Gene expression profiling predicts clinical outcome of breast cancer,\emph{Nature}, 415, 530-536.

Kaiser S. and Leisch F. (2008). A Toolbox for Bicluster Analysis in R. \emph{Ludwigstrasse.} 33.

Csardi G., Kutalik Z., and Bergmann S.(2010). Modular analysis of gene expression data with R. \emph{Bioinformatics,} 26, 1376-7
}

\author{Mengsteab Aregay 


\email{mycs.zab@gmail.com}}
\seealso{\pkg{biclust}}

\examples{
# create the biclustering result
data(breastc)
library(fabia)
fab<- fabia(breastc)
# write the biclustering result into a text file
writeBic(dset=breastc,fileName="fabiabreast.txt",
bicResult=fab, bicname="Biclust results for fabia",
mname="fabia")
}

\keyword{writeBic}
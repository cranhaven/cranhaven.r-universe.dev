\name{longitudinalData-package}
\alias{longitudinalData}
\alias{longitudinalData-package}
\docType{package}
\title{~ Package overview: longitudinalData ~}

\description{
  \code{longitudinalData} package provide some tools to deal with the clusterization
  of longitudinal data.
}
\details{
  \tabular{ll}{
    Package: \tab longitudinalData\cr
    Type: \tab Package\cr
    Version: \tab 2.4.1\cr
    Date: \tab 2016-02-02\cr
    License: \tab GPL (>= 2)\cr
    LazyData: \tab yes\cr
    Depends: \tab methods,clv,rgl,misc3d\cr
    URL:\tab http://www.r-project.org\cr
  }
}


\section{Overview}{
  \code{longitudinalData} provide some tools to deal with the clustering of longitudinal data, mainly:

  \enumerate{
    \item \link{plotTrajMeans}

    \item \link{imputation}

    \item \link{qualityCriterion}
  }
}

\section{Author}{
Christophe Genolini\cr
1. UMR U1027, INSERM, Université Paul Sabatier / Toulouse III / France\cr
2. CeRSM, EA 2931, UFR STAPS, Université de Paris Ouest-Nanterre-La Défense / Nanterre / France
}

\references{
  [1] Christophe M. Genolini and Bruno Falissard\cr
  "KmL: k-means for longitudinal data"\cr
  Computational Statistics, vol 25(2), pp 317-328, 2010\cr

  [2] Christophe M. Genolini and Bruno Falissard\cr
  "KmL: A package to cluster longitudinal data"\cr
  Computer Methods and Programs in Biomedicine, 104, pp e112-121, 2011
}

\seealso{
  Classes: \code{\linkS4class{LongData}}, \code{\linkS4class{Partition}} \cr
  Methods: \code{\link{longData}}, \code{\link{partition}}, \code{\link{ordered}}\cr
  Plot: \code{\link{plotTrajMeans}}, \code{\link{plotTrajMeans3d}}\cr
  Imputation: \code{\link{imputation}}\cr
  Criterion: \code{\link{qualityCriterion}}\cr
}

\examples{
### Generation of artificial longData
data(artificialJointLongData)
myData <- longData3d(artificialJointLongData,timeInData=list(var1=2:12,var2=13:23,var3=24:34))

part <- partition(rep(1:3,each=50))
plotTrajMeans3d(myData,part)

### Quality criterion
qualityCriterion(myData,part)
}

\keyword{package}  % Package Summaries
\keyword{dplot}    % Computations Related to Plotting
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering


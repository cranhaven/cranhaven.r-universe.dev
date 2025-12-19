\name{plotclust}
\alias{plotclust}
\alias{plotclust}

\title{Barplot of Bicluster}
\description{ Draws a graph to compare the values inside the diffrent biclusters with the values outside the bicluster}
\usage{
plotclust(res,x,bicluster=TRUE,legende=FALSE,noC=5,wyld=3,Titel="Plotclust",...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The data matrix}
  \item{res}{BiclustResult object if bicluster=TRUE else a normal kcca object.}
  \item{bicluster}{If TRUE,res is treated as a BiclustResult object}
  \item{legende}{Draws a legend.}
  \item{noC}{Number of Clusters drawn}
  \item{wyld}{Gives the distance between plot and axis.}
  \item{Titel}{Gives the title of the plot.}
  \item{...}{Additional plot options}
  }
%\details{}
%\value{}
%\references{}

\author{
  Sebastian Kaiser
  \email{sebastian.kaiser@stat.uni-muenchen.de}
  }

\seealso{
\code{\link{bubbleplot}} for simultaneous representation of biclusters.
\code{\link{parallelCoordinates}}for single representation of biclusters as lines of gene or condition profiles.
\code{\link{drawHeatmap}}for Heatmap representation of biclusters.
  }
\examples{

  s2=matrix(rnorm(400),20,20)
  s2[12:16,12:16]=rnorm(25,3,0.3)
  set.seed(1)
  bics <- biclust(s2,BCPlaid(), back.fit = 2, shuffle = 3, fit.model = ~m + a + b,
  iter.startup = 5, iter.layer = 30,  verbose = TRUE)
  plotclust(bics,s2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
\keyword{cluster}

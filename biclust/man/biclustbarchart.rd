\name{biclustbarchart}
\alias{biclustbarchart}


\title{Bicluster Barchart}
\description{ Draws a barchart for a Bicluster result representing the columns}
\usage{
biclustbarchart(x, Bicres, which=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The data matrix}
  \item{Bicres}{BiclustResult object with a bicluster result set. If this value
    is set to NULL, the data matrix is drawn as a heatmap, without any reordering. Default NULL.}
  \item{which}{If specified gives the ploting order of the columns from bottom to top}
  \item{...}{Additional plot options passed to barchart}
  }
%\details{}
%\value{}
%\references{}

\author{
  Sebastian Kaiser
  \email{sebastian.kaiser@stat.uni-muenchen.de}
  }

\seealso{
\code{\link{bubbleplot}} for simultaneous representation of biclusters,
\code{\link{parallelCoordinates}}for single representation of biclusters as lines of gene or condition profiles,
\code{\link{drawHeatmap}}for Heatmap representation of biclusters and 
\code{\link{biclustmember}} for a membership graph.
  }
\examples{
  set.seed(1)
  x=matrix(rnorm(900),30,30)
  x[1:5,1:5]=rnorm(25,3,0.3)
  x[11:15,11:15]=rnorm(25,-3,0.3)
  x[21:25,21:25]=rnorm(25,6,0.3)
  colnames(x)<-paste("Var.",1:30)
  bics <- biclust(x,BCPlaid(), back.fit = 2, shuffle = 3, fit.model = ~m
+ a + b, iter.startup = 5, iter.layer = 30,  verbose = TRUE)  
  biclustbarchart(x,bics, col="#A3E0D8")
  ord<-bicorder(bics, cols=TRUE, rev=TRUE)
  biclustbarchart(x,bics,which=ord)
  

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
\keyword{cluster}

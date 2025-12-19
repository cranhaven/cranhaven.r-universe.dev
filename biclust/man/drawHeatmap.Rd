\name{drawHeatmap}
\alias{drawHeatmap}
\alias{drawHeatmap2}
\alias{heatmap}

\title{Draw Heatmap}
\description{ Draws a microarray data matrix as a heatmap, with rows and colums
  reordered so the rows and columns of the input bicluster will be at top-left
  of the matrix.}
\usage{
drawHeatmap(x,bicResult=NULL,number=NA,local=TRUE, beamercolor=FALSE,paleta,...)
drawHeatmap2(x,bicResult=NULL,number=NA,plotAll=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The data matrix where the bicluster is to be drawn.}
  \item{bicResult}{BiclustResult object with a bicluster result set. If this value
    is set to NULL, the data matrix is drawn as a heatmap, without any reordering. Default NULL.}
  \item{number}{Bicluster to be drawn from the result set 'bicResult'. If bicResult is set to
    NULL, this value is ignored. Default NA}
  \item{local}{If TRUE, only rows and columns of the bicluster were drawn.}
  \item{plotAll}{If TRUE, all Bicluster of result set 'bicResult' were drawn.}
  \item{beamercolor}{If TRUE, palete colors are used.}
  \item{paleta}{Colors}
  \item{...}{Additional plot options}
  }
\details{'plotAll' only works if there is a exclusive rows and column Result!}
%\value{}
%\references{}

\author{
  Rodrigo Santamaria \email{rodri@usal.es},
  Sebastian Kaiser
  }

\seealso{
\code{\link{bubbleplot}} for simultaneous representation of biclusters.\\
\code{\link{parallelCoordinates}}for single representation of biclusters as lines of gene or condition profiles.
  }
\examples{
%-- ==>  Define data, use random,
%--	or do  help(data=index)  for the standard data sets.
  #Random 100x50 matrix with a single, up-regulated 10x10 bicluster
  s2=matrix(rnorm(5000),100,50)
  s2[11:20,11:20]=rnorm(100,3,0.3)
  set.seed(1)
  bics <- biclust(s2,BCPlaid(), back.fit = 2, shuffle = 3, fit.model = ~m + a + b,
  iter.startup = 5, iter.layer = 30,  verbose = TRUE)
  drawHeatmap(s2,bics,1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
\keyword{cluster}

\name{parallelCoordinates}
\alias{parallelCoordinates}
\alias{parallel coordinates}

\title{Parallel Coordinates}
\description{Represents expression levels through gene and/or condition profiles in
  a bicluster as lines.}
\usage{
parallelCoordinates(x, bicResult, number, plotBoth = FALSE, plotcol = TRUE,
compare = TRUE, info = F, bothlab = c("Rows", "Columns"), order = FALSE,
order2 = 0,ylab = "Value" , col=1,...)
}
\arguments{
  \item{x}{The data matrix of the bicluster to be drawn}
  \item{bicResult}{BiclustResult object with a bicluster result set}
  \item{number}{Bicluster to be drawn from the result set 'bicResult'}
  \item{plotBoth}{If 'TRUE', Parallel Coordinates of rows (Genes) and columns (Conditions) were drawn one below the other.}
  \item{plotcol}{If 'TRUE', columns profiles are drawn, so each line represents one of the columns in the bicluster. Otherwise, row profiles are drawn. Default 'TRUE'}
  \item{compare}{If 'TRUE',  values of the complete data matrix are considered and drawn as shaded lines. Default 'TRUE'}
  \item{info}{If 'TRUE', a prepared Title is drawn}
  \item{bothlab}{Names of the x Axis if PlotBoth}
  \item{order}{Rows and/or Columns are in increasing order.}
  \item{order2}{Which ordering.}
  \item{ylab}{ylab}
  \item{col}{col}
  \item{...}{Plot Parameters}
  }


 \author{
  Rodrigo Santamaria, Martin Sill and Sebastian Kaiser
  \email{sebastian.kaiser@stat.uni-muenchen.de}
  }

\seealso{
\code{\link{drawHeatmap}} for alternative representation of biclusters and \code{\link{bubbleplot}} for simultaneous representation of biclusters.
  }
\examples{
  #Random 100x50 matrix with a single, up-regulated 10x10 bicluster
  s2=matrix(rnorm(5000),100,50)
  s2[11:20,11:20]=rnorm(100,3,0.3)
  set.seed(1)
  bics <- biclust(s2,BCPlaid(), back.fit = 2, shuffle = 3, fit.model = ~m + a + b,
  iter.startup = 5, iter.layer = 30,  verbose = TRUE)
  parallelCoordinates(x=s2,bicResult=bics,number=1, plotBoth=TRUE,
plotcol=TRUE, compare=TRUE, info=TRUE,bothlab=c("Genes Bicluster
1","Conditions Bicluster 1"), order =TRUE)
  parallelCoordinates(x=s2,bicResult=bics,number=1, plotBoth=FALSE, plotcol=TRUE, 
    compare=FALSE, info=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
\keyword{cluster}

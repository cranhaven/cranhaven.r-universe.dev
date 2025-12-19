\name{bubbleplot}
\alias{bubbleplot}
\alias{bubble plot}
\alias{star graph}

\title{Bubbleplot}
\description{ Draws a bubble plot where each bicluster is represented as a circle
  (bubble). Color represents the bicluster set to which bicluster pertains (up to three
    bicluster sets can be represented simultaneously). Brightness represents the bicluster homogeneity
    (darker, less homogeneous).
    Size represents the size of the bicluster, as (number of genes)x(number of conditions).
    Location is a 2D-projection of gene and condition profiles.
    }
    
\usage{
bubbleplot(x, bicResult1, bicResult2=NULL, bicResult3=NULL, projection="mean", 
  showLabels=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The data matrix from which biclusters were identified.}
  \item{bicResult1}{BiclustResult object with a bicluster result set whose biclusters will be drawn in green.}
  \item{bicResult2}{BiclustResult object with an optional second bicluster result set. Will be drawn in red (default NULL)}
  \item{bicResult3}{BiclustResult object with an optional third bicluster result set. Will be drawn in blue (default NULL)}
  \item{projection}{ Projection algorithm used to position bubbles. Allowed projections are
                 'mean', 'isomds' and 'cmdscale' (default 'mean').
                  See details section for a broader explanation.}
  \item{showLabels}{ If 'TRUE', puts a label over each bubble that tells the number within the
                  corresponding bicluster result (default 'FALSE').}
  }



\details{
Position of circles depend on a
 2D projection of the multidimensional point formed by rows and columns present
 in the bicluster.
 For example, if we have a 3x3  matrix to analyze and we find a bicluster with
 rows 1 and 3 and columns 2 and 3, the corresponding multidimensional point
 will be p=(1,0,1,0,1,1).
  For this example, 'mean' projection will map the bicluster with the point 
  x=(1+3)/2=2 and y=(2+3)/2=2,5. Other projections will take the point
  p and project it following the corresponding algorithms (see the
  corresponding help pages for details)
  }
 
%\value{}
%\references{}
\note{Bubbleplot 2D-projection, as any multidimensional scaling, loses information,
  trying to take the main relationships and trends of n-dimensional data. Thus,
  locations and intersections between bubbles-biclusters are only an estimate of its
  similarity. This visualization should be used just as a help to understand overall
  behavior of biclustering methods, detect trends and outliers, etc.}

\author{
  Rodrigo Santamaria
  \email{rodri@usal.es}
  }

\seealso{
\code{\link{drawHeatmap}} for single representation of biclusters inside data matrix,
\code{\link{parallelCoordinates}} for single representation of biclusters as lines of gene or condition profiles, \code{cmdscale, isomds} for multidimensional scaling and \code{plot} for other point representations.
  }
   
   \examples{
  
  #Simplified yeast microarray data
  \dontrun{ 
  data(BicatYeast)
  set.seed(1)
  bics1 <- biclust(BicatYeast,BCPlaid(), back.fit = 2, shuffle = 3, fit.model = ~m + a + b,
  row.release = 0.7, col.release = 0.7,
  verbose = FALSE, max.layers = 10, iter.startup = 5,
  iter.layer = 30)
  bubbleplot(BicatYeast,bics1, showLabels=TRUE)

  loma=binarize(BicatYeast,2)
  bics2=biclust(loma,BCBimax(), minr=4, minc=4, number=10)
  bubbleplot(BicatYeast,bics1,bics2)
  }
  }           

\keyword{hplot}
\keyword{cluster}

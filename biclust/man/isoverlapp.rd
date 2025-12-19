\name{isoverlapp}
\title{Is Bicresult overlapping?}
\alias{isoverlapp}


\description{Checks if Biclusterresult includes overlapping rows or columns}

\usage{
isoverlapp(bicResult)
}

\arguments{
  \item{bicResult}{Result of biclust function}
  }
  
  
\value{
\item{Overlapping}{Is there overlapping}
\item{Max.bicluster.Rows}{Maximal number of bicluster a single row is in}
\item{Max.bicluster.Cols}{Maximal number of bicluster a single col is in}



}
  
\author{Sebastian Kaiser 
\email{sebastian.kaiser@stat.uni-muenchen.de}
}


\seealso{ \code{\link{drawHeatmap}}}


\keyword{cluster}
\keyword{classif}

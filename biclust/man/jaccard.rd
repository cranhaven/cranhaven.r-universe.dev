\name{jaccardind}
\alias{jaccardind}
\alias{jaccard2}

\title{Jaccardind}
\description{ An adaption of the Jaccard Index for clustering is calculated.}
\usage{
jaccardind(bicres1,bicres2)
jaccard2(Rows, Cols)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bicres1}{A object of class Biclust}
  \item{bicres2}{A object of class Biclust}
  \item{Rows}{Matrix containing rows of biclusters}
  \item{Cols}{Matrix containing cols of biclusters}
  }
\details{
   The function calculates the percentage of datapoints in the same bicluster structure from all datapoints at least included in one bicluster.
}
\value{
  \code{jaccardind} calculates the Jaccard index
  \code{jaccard2} returns a similarity matrix containing the Jaccard
  index between all biclusters (upper triangle matrix)

}
%\references{}

\author{
  Sebastian Kaiser
  \email{sebastian.kaiser@stat.uni-muenchen.de}
  }

%\seealso{  }
\examples{
\dontrun{
data(BicatYeast)
res1<-biclust(BicatYeast, method=BCPlaid(), back.fit = 2, shuffle = 3,
  fit.model = ~m + a + b,iter.startup = 5, iter.layer = 30,  verbose = TRUE)
res2<-biclust(BicatYeast, method=BCCC())
jaccardind(res1,res2)

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{cluster}


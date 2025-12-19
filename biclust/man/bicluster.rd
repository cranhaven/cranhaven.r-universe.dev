\name{bicluster}
\alias{bicluster}
\alias{biclusternumber}


\title{Extract Bilcuster}
\description{ Function to extract the bicluster or the row and column numbers from a given bicluster result}
\usage{
bicluster(x, BicRes, number= 1:BicRes@Number)
biclusternumber(BicRes, number= 1:BicRes@Number)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The data matrix}
  \item{BicRes}{BiclustResult object}
  \item{number}{Which bicluster to be extracted}
  }
%\details{}
\value{Returns a list containing all extracted bicluster}
%\references{}

\author{
  Sebastian Kaiser
  \email{sebastian.kaiser@stat.uni-muenchen.de}
  }

\seealso{
\code{\link{writeclust}},\code{\link{writeBiclusterResults}}
  }
\examples{

  s2=matrix(rnorm(400),20,20)
  s2[12:16,12:16]=rnorm(25,3,0.3)
  set.seed(1)
  bics <- biclust(s2,BCPlaid(), back.fit = 2, shuffle = 3, fit.model = ~m + a + b,
  iter.startup = 5, iter.layer = 30,  verbose = TRUE)
  bicluster(s2, bics)
  biclusternumber(bics)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{cluster}
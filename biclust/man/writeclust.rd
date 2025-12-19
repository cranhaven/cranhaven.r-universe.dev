\name{writeclust}
\alias{writeclust}

\title{Write a Bicluster as a Cluster Result}
\description{ Draws a graph to compare the values inside the diffrent biclusters with the values outside the bicluster}
\usage{
writeclust(Biclusterresult,row=TRUE,noC=10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Biclusterresult}{BiclustResult object}
  \item{row}{If TRUE, cluster of rows were written.}
  \item{noC}{Number of Clusters written}
  }
%\details{}
%\value{}
%\references{}

\author{
  Sebastian Kaiser
  \email{sebastian.kaiser@stat.uni-muenchen.de}
  }

%\seealso{}
\examples{

  s2=matrix(rnorm(400),20,20)
  s2[12:16,12:16]=rnorm(25,3,0.3)
  set.seed(1)
  bics <- biclust(s2,BCPlaid(), back.fit = 2, shuffle = 3, fit.model = ~m + a + b,
  iter.startup = 5, iter.layer = 30,  verbose = TRUE)
  writeclust(bics)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
\keyword{cluster}

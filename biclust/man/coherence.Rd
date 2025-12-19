\name{coherence}
\alias{coherence}
\alias{constantVariance}
\alias{additiveVariance}
\alias{multiplicativeVariance}
\alias{signVariance}

\title{Coherence measures}
\description{ Different preliminary measures of how much constant or
  (additive, multiplicative, sign) coherent a bicluster is, following
  Madeira and Oliveira classification of biclusters.
  }

\usage{
constantVariance(x, resultSet, number, dimension="both")
additiveVariance(x, resultSet, number, dimension="both")
multiplicativeVariance(x, resultSet, number, dimension="both")
signVariance(x, resultSet, number, dimension="both")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The data matrix from which biclusters were identified}
  \item{resultSet}{BiclustResult object with a bicluster result set where is the bicluster to measure}
  \item{number}{Number of the bicluster withing the result set}
  \item{dimension}{"both" for determining overall variance, "row" for gene variance and "col" for column variance. Default "both"}
  }



\details{
  Returns the corresponding variance of genes or conditions as the average of the sum of euclidean
  distances between all rows and/or columns of the bicluster.
  For additive, multiplicative and sign variance first a transformation of the
  bicluster is done, so variance is computed on a matrix that reflects difference,
  rest or change of sign between rows, columns or both.
  
  The lower the value returned, the more constant or coherent the bicluster is.
  If the value returned is 0, the bicluster is ideally constant or coherent.
  Usually, a value above 1-1.5 is enough to determine the bicluster is not
  constant or coherent.
  }

%\value{}
%\references{}
\note{There are preliminary measures for coherence. Since transformations are
  different, measures are not normalized and comparison between, for example,
  additive and multiplicative variance is not meaningful. Only comparisons between
  different measures of the same kind of variance are reliable by now.}

\author{
  Rodrigo Santamaria
  \email{rodri@usal.es}
  }

%\seealso{}

\examples{
  #Simplified yeast microarray data
  data(BicatYeast)
  set.seed(1)
  bics1 <- biclust(BicatYeast,BCPlaid(), back.fit = 2, shuffle = 3, fit.model = ~m + a + b,
  row.release = 0.7, col.release = 0.7,
  verbose = FALSE, max.layers = 10, iter.startup = 5,
  iter.layer = 30)
  
  constantVariance(BicatYeast, bics1,1,"row")
  constantVariance(BicatYeast, bics1,1,"col")
  constantVariance(BicatYeast, bics1,1,"both")
  additiveVariance(BicatYeast, bics1,1,"both")
  multiplicativeVariance(BicatYeast, bics1,1,"both")
  signVariance(BicatYeast, bics1,1,"both")
  }


\keyword{array}
\keyword{cluster}

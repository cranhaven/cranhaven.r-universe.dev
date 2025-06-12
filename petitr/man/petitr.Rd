\name{petitr}
\Rdversion{1.1}
\alias{petitr}
\title{
Per capita growth rate from individual data
}
\description{
calculates the per capita growth rate of a series of individuals through the set of individual life tables
}
\usage{
petitr(tabvie, niter = 100, eps = 1e-07, m = 1, alpha = 0.05, s = 1)

}
\arguments{
  \item{tabvie}{
A data.frame with a first column recording the endpoint of age classes, and as many columns as individuals. For each individual, each row represents the number of offspring produced by the individual vetween age x-1 and age x. After death or after the end of reproductive life, each column must be filled by zeros . The last row represents therefore the maximum reproductive age observed in the data set. The number of columns is n+1, where n is the number of individuals.
}
  \item{niter}{
the maximum number of iterations for the Newton's method. Default is 100
}
  \item{eps}{
Precision required for the Newton's method. Default is 1e-07.
}
  \item{m}{
Size of the subsamples to drop one after one in the Jackknife method. Default is m=1. Any other value must divide exactly n,the number of individuals. 
}
  \item{alpha}{
First kind error risk. Default is alpha=0.05.
}
  \item{s}{
ex ratio expressed as the proportion of females in the total population. Default is 1, meaning a parthenogenetic population (ex. aphids). For a sexual population one would often set s=0.5.
}
}
\details{
Calls \code{\link{r}}, and \code{\link{xlxmx}}, called by  \code{\link{ranova}}.
}
\value{
a vector with the pseudovalues of r calculated by the jackknife method
}
\references{
Birch, L. C. 1948. The intrinsic rate of natural increase of an insect population. - Journal of Animal Ecology 17: 15-26.
Lotka, A. (1924). Elements of mathematical biology. Reprinted 1956 by Dover Publications Inc., New York, USA.
}
\author{
Jean-Sebastien Pierre jean-sebastien.pierre@univ-rennes1.fr
}

\seealso{
 \code{\link{r}}, and \code{\link{xlxmx}}, called by  \code{\link{ranova}}
}
\examples{
data(life)
petitr(life)
}
\keyword{ misc }

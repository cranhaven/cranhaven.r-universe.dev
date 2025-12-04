\name{richRarefy}
\alias{richRarefy}

\title{
Sample-based rarefaction curves
}
\description{
Uses Mao's tau estimator (Colwell et al, 2004) to obtain a rarefaction curve indicating the expected number of species observed if fewer samples were collected.
}
\usage{
richRarefy(incmat)
}
\arguments{
  \item{incmat}{
a 1/0 matrix of species incidence (presence), species x sites. A matrix of counts may also be provided.
}
}
\value{
A matrix with columns for the estimate and its standard deviation and rows for the number of samples pooled. Confidence limits may be obtained with estimate +/- 1.96 * SD.
}
\references{
Colwell, R. K., C. X. Mao, & J. Chang. 2004. Interpolating, extrapolating, and comparing incidence-based species accumulation curves. Ecology 85, 2717-2727. 
}
\author{
Mike Meredith
}
\examples{
data(seedbank)
plot(richRarefy(seedbank)[, 1], type='l')
}


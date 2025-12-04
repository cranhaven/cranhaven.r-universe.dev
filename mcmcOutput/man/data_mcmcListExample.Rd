\name{mcmcListExample}
\alias{mcmcListExample}
\encoding{UTF-8}
\docType{data}
\title{
An object of class \code{mcmc.list} produced by \code{rjags::coda.samples}
}
\description{
This is the output of a basic occupancy model applied to detection/non-detection data for blue ridge salamanders (\emph{Eurycea wilderae}) in Great Smoky Mountains National Park (MacKenzie et al, 2006 p99). Detections were recorded for 5 visits to each of 39 sites, and the data are the number of visits where the species was detected.

The model has five parameters:
\describe{
  \item{psi }{scalar, the probability of occupancy.}
  \item{p }{\code{p[1,1]} is the probability of detection given presence; \code{p[2,2]} is a dummy variable with values drawn from a Beta(0.5,0.5) distribution; \code{p[1,2]} and \code{p[2,1]} are not defined, so \code{p} is a "ragged array".}
  \item{z }{a vector of length 39, one value for each site.}
  \item{Tobs }{scalar, the Freeman-Tukey discrepancy for the observed data.}
  \item{Tsim }{scalar, the Freeman-Tukey discrepancy for the simulated data.}
}

The number of nodes monitored is 44 (\code{p[1,2]} and \code{p[2,1]} are not monitored).

Three MCMC chains were run, with 1000 adaptation iterations, 1000 burn-in, and 1000 iterations saved per chain after thinning by 10.
}

\usage{data("mcmcListExample")}

\format{
  \code{mcmcListExample} is \code{mcmc.list} object as defined in package \pkg{coda}.
}
\references{
MacKenzie, D I; J D Nichols; A J Royle; K H Pollock; L L Bailey; J E Hines 2006. \emph{Occupancy estimation and modeling : inferring patterns and dynamics of species occurrence}. Elsevier Publishing.
}

\examples{
data(mcmcListExample)
str(mcmcListExample)

# convert to class mcmcOutput
( mco <- mcmcOutput(mcmcListExample) )
summary(mco)

# Extract with "$"
p <- mco$p
str(p)
p[1:5,,]  # Elements of p not defined in the model are filled with NAs

# "[" with one index, produces new mcmcOutput object
head(mco[4:5])
print(mco[c("z[35]", "z[39]")])

# "[" with two indices
mco[1:5, "psi"]  # First 5 values for psi (chain #1)

# "[" with three indices
mco[1:5, 2, "psi"] # First 5 values for psi in chain #2

}
\keyword{datasets}

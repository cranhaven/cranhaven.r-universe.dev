\name{ShapeDist}
\alias{ShapeDist}
\title{
Calculate Procrustes distance between shapes
}
\description{
Use the Procrustes method to calculate distances between a set of shapes 
and a reference.
}
\usage{
ShapeDist(shapes, reference)
}
\arguments{
  \item{shapes}{Landmark data of shapes (list of matrices)}
  \item{reference}{Lanmark data of reference shape (matrix type)}
}
\details{
Procrustes distance provides a measure of coincidence of two point sets {xi} and {yi}, i=1..N. For this purpose the variance of point deviations is calculated at the optimal superposition of the sets. It allows to characterize the shape proximity of a given simplex to shape of a reference one.
}
\value{
A matrix containing procrustes distance between shapes. Procrustes distance is the square root of the sum of squared differences in the posititions of the landmarks in two shapes (Dryden and Mardia 1998).
This can be used to describe the difference between many landmark configurations (Rohlf and Slive 1990). 
}
\references{
Dryden, I. L., & Mardia, K. V. (1998). Statistical shape analysis (Vol. 4). Chichester: Wiley.

Rohlf, F. J., & Slice, D. (1990). Extensions of the Procrustes method for the optimal superimposition of landmarks. Systematic Biology, 39(1), 40-59.}
\author{Cabrera Juan Manuel}

\examples{
data("aegla_landmarks")
data("aegla_consensus")

#Calculate distances between set of shapes and a reference
ShapeDist(shapes = aegla_landmarks, reference = aegla_consensus)}

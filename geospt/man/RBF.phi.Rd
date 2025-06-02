\name{RBF.phi}
\alias{RBF.phi}

\title{
radial basis function evaluation
}
\description{
generate the value associated with radial basis functions; gaussian GAU), exponential (EXPON),
trigonometric (TRI), thin plate spline (TPS), completely regularized spline (CRS),
spline with tension (ST), inverse multiquadratic (IM), and multiquadratic (M)
}
\usage{
RBF.phi(distance, eta, func)
}

\arguments{
\item{distance}{corresponds to the Euclidean distance between two points in space}
\item{eta}{the optimal smoothing parameter is found by minimizing the root-mean-square prediction errors using cross-validation}
\item{func}{radial basis function model type, e.g. "GAU", "EXPON", "TRI", "TPS", "CRS", "ST", "IM" and "M", are currently available}
}
\value{
value obtained from the radial basis function generated with a distance,
a \emph{eta} smoothing parameter, and a function "GAU", "EXPON", "TRI", "TPS",  "CRS", "ST", "IM" or "M"
}
\examples{
data(preci) 
d1 <- dist(rbind(preci[1,],preci[2,])) 
RBF.phi(distance=d1, eta=0.5, func="TPS")
}

\keyword{ spatial }

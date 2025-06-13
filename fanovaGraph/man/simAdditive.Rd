\name{simAdditive}
\alias{simAdditive}

\title{
Simulate GP values from block-addtive kernel
}
\description{
Simulate Gaussian process values from a given block-additive kernel
}
\usage{
simAdditive(newdata, mu, parameter, covtype, cl, iso = FALSE, eps.R = 1e-08)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{newdata}{
matrix containing the points where to perform simulations
}
  \item{mu}{
trend parameter
}
  \item{parameter}{
list of size of 'cl' containing for each clique a list of parameters alpha (single value) and theta (numeric vector of values)
}
  \item{covtype}{
character string specifying the covariance structure to be used, to be chosen between "gauss", "matern5_2", "matern3_2", "exp" or "powexp" (see DiceKriging)
}
  \item{cl}{
list of cliques
}
  \item{iso}{
boolean vector indicating for each clique if it is isotropic (TRUE) or anisotropic (FALSE), defaults to iso = FALSE (all cliques anisotropic)
}
  \item{eps.R}{
small positive number indicating the nugget effect added to the covariance matrix diagonalk, defaults to eps.R = 1e-08
}
}
\value{
a vector containing the simulated values
}
\references{
Muehlenstaedt, T.; Roustant, O.; Carraro, L.; Kuhnt, S. (2011) Data-driven Kriging models based on FANOVA-decomposition, \emph{Statistics and Computing}.

Rasmussen, C. E.; Williams, C. K. I. (2006), \emph{Gaussian processes for machine learning}, MIT Press.
}
\author{
J. Fruth
}

\seealso{
\code{\link{kmAdditive}}, \code{\link[DiceKriging]{simulate}}
}
\examples{
### 2 dimensional simulation
x1 <- x2 <- seq(-1,1,,20)
x <- expand.grid(x1,x2)
covtype <- "matern3_2"
mu <- 0

op <- par(no.readonly=TRUE); par(mfrow=c(1,2), mar=c(1,1,1,1))
# non-additive simulation
parameter <- list(list(alpha=1, theta=c(0.8,0.8)))
cl <- list(1:2)
set.seed(1)
y <- simAdditive(x, mu, parameter, covtype, cl)
persp(x1,x2, matrix(y,20), theta=-40, col="lightblue", zlab="y")

# additive simulation
parameter <- list(list(alpha=0.5, theta=0.8),
                  list(alpha=0.5, theta=0.8))
cl <- list(1,2)
set.seed(1)
y <- simAdditive(x, mu, parameter, covtype, cl)
persp(x1,x2, matrix(y,20), theta=-40, col="lightblue", zlab="y")

par(op)
}
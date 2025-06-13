\name{predictAdditive}
\alias{predictAdditive}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Prediction Function with Modified Kernel
}
\description{Standard kriging prediction function for the modified correlation functions.
}
\usage{
predictAdditive(newdata, x, y, parameter, covtype = "gauss", eps.R = 1e-08, 
    cl, iso = FALSE, se.compute=FALSE)
}
\arguments{
  \item{newdata}{matrix containing the points where to perform predictions
}
  \item{x}{matrix of input data
}
  \item{y}{vector of output data
}
  \item{parameter}{(by \code{\link{kmAdditive}} estimated) kriging parameters,
  list of size of 'cl' containing for each clique a list of parameters alpha (single value) and theta (numeric vector of values)
}
  \item{covtype}{an optional character string specifying the covariance structure to be used, 
    to be chosen between "gauss", "matern5_2", "matern3_2", "exp" or "powexp" (see DiceKriging), defaults to "gauss"
}
  \item{eps.R}{small positive number indicating the nugget effect added to the covariance matrix diagonalk, defaults to \code{eps.R = 1e-08}
}
  \item{cl}{list of cliques
}
  \item{iso}{boolean vector indicating for each clique if it is isotropic (TRUE) or anisotropic (FALSE), defaults to \code{iso = FALSE} (all cliques anisotropic)
}
  \item{se.compute}{optional boolean. If FALSE, only the kriging mean is computed. If TRUE, the kriging variance (actually, the corresponding standard deviation) is computed, too}
}
\value{
  \item{mean }{ kriging mean computed at \code{newdata}. }
  \item{sd }{ kriging standard deviation computed at \code{newdata}. Only computed if 
              \code{se.compute=TRUE}. }
}
\references{
Muehlenstaedt, T.; Roustant, O.; Carraro, L.; Kuhnt, S. (2011) Data-driven Kriging models based on FANOVA-decomposition, \emph{Statistics and Computing}.
}
\author{
T. Muehlenstaedt, O. Roustant, J. Fruth
}

\seealso{
\code{\link{kmAdditive}}
}
\examples{
### example for ishigami function with cliques {1,3} and {2}
d <- 3
x <- matrix(runif(100*d,-pi,pi),nc=d)
y <- ishigami.fun(x)

cl <- list(c(2), c(1,3))

# constrained ML optimation with kernel defined by the cliques
parameter <- kmAdditive(x, y, cl = cl)

# prediction with the new model
xpred <- matrix(runif(500 * d,-pi,pi), ncol = d)
ypred <- predictAdditive(xpred, x, y, parameter, cl=cl)
yexact <- ishigami.fun(xpred)

# rmse
sqrt(mean((ypred[,1]- yexact)^2))

# scatterplot
par(mfrow=c(1,1))
plot(yexact, ypred[,1], asp = 1)
abline(0, 1)

### compare to one single clique {1,2,3}
cl <- list(c(1,2,3))

# constrained ML optimation with kernel defined by the cliques
parameter <- kmAdditive(x, y, cl = cl)

# prediction with the new model
ypred <- predictAdditive(xpred, x, y, parameter, cl=cl)

# rmse
sqrt(mean((ypred$mean- yexact)^2))

# scatterplot
par(mfrow=c(1,1))
plot(yexact, ypred$mean, asp = 1)
abline(0, 1)

### isotropic cliques

cl <- list(c(2),c(1,3))
parameter <- kmAdditive(x, y, cl = cl, iso=c(FALSE,TRUE))
ypred <- predictAdditive(xpred, x, y, parameter, cl=cl, iso=c(FALSE,TRUE))
sqrt(mean((ypred$mean- yexact)^2))

# the same since first clique has length 1
parameter <- kmAdditive(x, y, cl = cl, iso=c(TRUE,TRUE))
ypred <- predictAdditive(xpred, x, y, parameter, cl=cl, iso=c(TRUE,TRUE))
sqrt(mean((ypred$mean- yexact)^2))
}

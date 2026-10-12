\name{ras}
\alias{ras}

\title{
ras Updating Proejcting
}
\description{
Uses the ras technique to update the matrix of technical input coefficients \code{A}. You must have knowledge of or forecasts for the following three objects: (1) row sums \code{u1} of \code{A}, (2) column sums \code{v1} of \code{A}, and (3) total production \code{x1}.}
\usage{
ras(io, x1, u1, v1, tol, maxiter, verbose = FALSE)
}

\arguments{
  \item{io}{An \code{InputOutput} class object from \code{\link{as.inputoutput}}}
  \item{x1}{Vector. The forecast for future total production of each region-sector combination, matching the \code{X} object in \code{io}}
  \item{u1}{Vector. The forecast for future row sums of the matrix of technical input coefficients in \code{A} from \code{io}}
  \item{v1}{Vector. The forecast for future column sums of the matrix of technical input coefficients in \code{A} from \code{io}}
  \item{tol}{Numeric. The tolerance for convergence. Default: \code{1e-06}}
  \item{maxiter}{Numeric. The maximum number of iterations to try for convergence. Defualt: 10000}
  \item{verbose}{Logical. If \code{TRUE} will print the iteration and norm at each step. This is useful if the dataset is large. Deafult: \code{FALSE}}
}

\details{Uses the ras iterative technique for updating the matrix of technical input coefficients. This takes the form:
\deqn{lim_{n \Rightarrow \infty} A^{2n} = lim_{n \Rightarrow \infty} [\hat{R}^n ... \hat{R}^1]A_t[\hat{S}^1 ... \hat{S}^n] = \hat{A}_{t+1}}
where \eqn{R^1 = diag(u_{t+1}/u_0)}, \eqn{u_0 = A_tX}, and \eqn{u_{t+1} = }\code{u1}. Similarly \eqn{S^1 = diag(v_{t+1}/v_0)}, \eqn{v_0 = XR^1A_t}.

Each iteration calculates the full \eqn{ras} object; that is, 2 steps are caluclated per iteration.

See Blair and Miller (2009) for more details.
}

\value{
Produces the forecast of the matrix of technical input coefficients given the forecasted row sums, column sums, and total production.
}
\references{
Blair, P.D. and Miller, R.E. (2009). "Input-Output Analysis: Foundations and Extensions". Cambridge University Press
}
\author{
John J. P. Wade
}



\seealso{\code{\link{as.inputoutput}}, \code{\link{lq}}}
\examples{
data(toy.IO)
class(toy.IO)

set.seed(117)
growth <- 1 + 0.1 * runif(10)
sort(growth)

X <- toy.IO$X
X1 <- X * growth
U <- rowSums(toy.IO$Z)
U1 <- U * growth
V <- colSums(toy.IO$Z)
V1 <- V * growth

ras <- ras(toy.IO, X1, U1, V1, maxiter = 10, verbose = TRUE)
}


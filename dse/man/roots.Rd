\name{roots}
\alias{roots}
\alias{roots.SS}
\alias{roots.ARMA}
\alias{roots.TSestModel}

\title{Calculate Model Roots}
\description{Calculate roots of a TSmodel.}
\usage{
    roots(obj, ...)
    \method{roots}{SS}(obj, fuzz=0, randomize=FALSE, ...)
    \method{roots}{ARMA}(obj, fuzz=0, randomize=FALSE, warn=TRUE, by.poly=FALSE, ...)
    \method{roots}{TSestModel}(obj, ...)
    }
\arguments{
    \item{obj}{An object of class TSmodel.}
    \item{fuzz}{If non-zero then roots within fuzz 
       distance are considered equal.}
    \item{randomize}{
       Randomly arrange complex pairs of roots so the one with the positive imaginary
       part is not always first (so random experiments are not biased).}
    \item{warn}{If FALSE then warnings about unit roots added for TREND are not printed.}
    \item{by.poly}{
       If TRUE then roots are calculated by expanding the determinant of the A
       polynomial.  Otherwise, they are calculated by converting to a state
       space representation and calculating the eigenvalues of F. This second
       method is preferable for speed, accuracy, and because of a limitation
       in the degree of a polynomial which can be handled by polyroot.}
    \item{...}{arguments passed to other methods.}
}
\value{
The eigenvalues of the state transition matrix or the inverse of the roots of the
determinant of the AR polynomial are returned.
}
\details{
   The equality of roots for equivalent state space and ARMA models is
   illustrated in \cite{Gilbert (1993)}. The calculation of ARMA model roots is
   more stable if the model is converted to state space and the roots
   calculated from the state transition matrix (see \cite{Gilbert,2000}). The
   calculation is done this way by default. If \code{by.poly=TRUE} then
   the determinant of the AR polynomial is expanded to get the roots.
   }
\seealso{
  \code{\link{stability}},
  \code{\link{McMillanDegree}}
  }
\examples{
  data("eg1.DSE.data.diff", package="dse")
  model <- estVARXls(eg1.DSE.data.diff)
  roots(model)
  }
\references{
    Gilbert, P. D. (1993) State space and ARMA models: An overview of
    the equivalence. Working paper 93-4, Bank of Canada. Available at \url{http://www.bankofcanada.ca/1993/03/publications/research/working-paper-199/}

    Gilbert, P.D. (2000) A note on the computation of time series model roots. 
    \emph{Applied Economics Letters}, \bold{7}, 423--424
    }
 \concept{DSE}
\keyword{ts}


\name{print.TSestModel}
\alias{print.TSestModel}
\alias{print.SS}
\alias{print.ARMA}

\title{Display TSmodel Arrays}
\description{
Display TSmodel arrays.
}
\usage{
    \method{print}{SS}(x, digits=options()$digits, latex=FALSE, ...)
    \method{print}{ARMA}(x, digits=options()$digits, latex=FALSE, L=TRUE, fuzz=1e-10, ...)
    \method{print}{TSestModel}(x, ...)
    }

\arguments{
    \item{x}{An object of class TSmodel or TSestModel.}
    \item{digits}{the number of significant digits}
    \item{L}{logical
      if TRUE then ARMA model arrays are displayed as a polynomial matrix 
      with L indicating lags. Otherwise, each lag in the array is 
      displayed as a matrix.}
    \item{latex}{logical. If TRUE additional context is added to make the output
      suitable for inclusion in a latex document.}
    \item{fuzz}{
      ARMA model polynomial elements with absolute value less than fuzz
      are not displayed (i.e.-as if they are zero)}
    \item{...}{arguments passed to other methods.}
}
\value{The object is returned invisibly.}
\section{Side Effects}{The model arrays are displayed.}
\note{BUG: digits cannot be controlled for some numbers (e.g.- 1.0 is printed
as 0.9999999999)
}
\examples{
data("eg1.DSE.data.diff", package="dse")
model <- estVARXls(eg1.DSE.data.diff)
print(model)
print(model, digits=3)
print(model, digits=3, fuzz=0.001)
print(model, digits=3, fuzz=0.001, latex=TRUE)
}


\seealso{
    \code{\link{print}},
    \code{\link{summary}}
}
\concept{DSE}
\keyword{ts}



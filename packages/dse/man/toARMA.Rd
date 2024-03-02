\name{toARMA}
\alias{toARMA}
\alias{toARMA.ARMA}
\alias{toARMA.SS}
\alias{toARMA.TSestModel}

\title{Convert to an ARMA Model}
\description{
Convert a state space model to an ARMA representation. The state is 
eliminated by a method which uses an equivalence that can be demonstrated
by the Cayley Hamilton theorem.  It is not very parsimonious.

}
\usage{
    toARMA(model, ...)
    \method{toARMA}{ARMA}(model, ...)
    \method{toARMA}{SS}(model, fuzz=1e-10, ...)
    \method{toARMA}{TSestModel}(model, ...)
}
\arguments{
    \item{model}{An object of class TSmodel.}
    \item{fuzz}{Parameters closer than fuzz to one or zero are set to 1.0
     or 0.0 respectively}
    \item{...}{arguments to be passed to other methods.}
}
\value{
An object of class 'ARMA' 'TSmodel' containing an ARMA model.
}
\references{See, for example, 
  Aoki, M. (1990) \emph{State Space Modelling of Time Series}. 2d ed. rev. 
  and enl., Springer-Verlag.
  
  Aoki, M. and Havenner, A. (1991) State Space Modeling of Multiple Time Series.
  \emph{Econometric Reviews}, \bold{10}, 1--59.
  }
\seealso{
\code{\link{toSS}}
\code{\link{fixConstants}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
model <- toSS(estVARXls(eg1.DSE.data.diff))
model <- toARMA(model)
}
\concept{DSE}
\keyword{ts}



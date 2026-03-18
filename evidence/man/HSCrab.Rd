\name{HSCrab}
\alias{HSCrab}
\docType{data}
\title{ Morphology of horseshoe crabs. }
\description{
  Data on horseshoe crab morphology collected by Brockman(1996) and used
  by Agresti(2012).
  }
\usage{data(HSCrab)}
\format{
  A data frame with 173 observations on the following 5 variables:
  \describe{
    \item{\code{Col}}{an indicator variable for the carapace color}
    \item{\code{spineW}}{coded width of the spine}
    \item{\code{Width}}{maximal width of the carapace (cm) }
    \item{\code{Satell}}{number of satellite males}
    \item{\code{Weight}}{weight in g}
  }
}

\source{
  Brockman, H.J.(1996) Satellite male groups in horseshoe crabs,
  \emph{Limulus polyphemus} Ethology 102(1), 1--21.
}
\references{
  Agresti, A.(2012) Categorical Data Analysis (3rd ed.) Wiley, New York.

  van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
  }
\examples{
data(HSCrab)
plot(Weight ~ Width, col = Col, data = HSCrab)
}
\keyword{datasets}

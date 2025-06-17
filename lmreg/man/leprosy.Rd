\name{leprosy}
\alias{leprosy}
\docType{data}
\title{
Treatment of leprosy
}
\description{
Pre- and post-treatment scores on abundance of leprosy for patients receiving different treatments (Senedecor and Cochran, 1967).
}
\usage{data(leprosy)}
\format{
  A data frame with 30 observations on the following 3 variables.
  \describe{
    \item{\code{treatment}}{Treatment type: \code{A}, \code{D} or \code{F} (placebo), a character vector}
    \item{\code{pre}}{Pre-treatment score, a numerical vector}
    \item{\code{post}}{Post-treatment score, a numerical vector}
  }
}

\source{
Snedecor, G.W. and Cochran, W.G. (1967) Statistical Methods, Iowa State
University, Ames, p.421.
}

\examples{
data(leprosy)
head(leprosy) 
}
\keyword{datasets}

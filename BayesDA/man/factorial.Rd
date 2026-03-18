\name{factorial}
\alias{factorial}
\docType{data}
\title{ Data From a Chemical Experiment  }
\description{
  A factorial designed experiment from chemistry. Three experimental variables representing
  reactor conditions, and the response, conversion (\%) from n-Heptane to
  acetylene.
}
\usage{data(factorial)}
\format{
  A data frame with 16 observations on the following 4 variables.
  \describe{
    \item{\code{temperature}}{reactor temperature}
    \item{\code{ratio}}{ratio of H2 to n-heptane (mole ratio)}
    \item{\code{contact}}{Contact time (sec)}
    \item{\code{conversion}}{the response, conversion from n-heptane to acetylene}
  }
}
\details{
  This data is used in an exercise on regression with many explanatory
  variables, page 413 of second edition. Original authors assume a quadratic 
  functional form.
}
\examples{
data(factorial)
summary(factorial)
# non-Bayesian analysis:
fac.mod1 <- lm(conversion ~ temperature+ratio+contact+
                            I(temperature*ratio)+I(temperature*contact)+
                            I(ratio*contact)+I(temperature^2)+I(ratio^2)+I(contact^2), 
                  data=factorial)
summary(fac.mod1)
}
\keyword{datasets}
\concept{ridge regression}

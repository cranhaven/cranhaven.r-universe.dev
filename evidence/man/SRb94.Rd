\name{SRb94}
\alias{SRb94}
\docType{data}
\title{ Data of the growth of tissue cultures on five different media. }
\description{
  These data came from a designed experiment reported in Sokal and
  Rohlf(1995), box 9.4. The growth (in arbitrary units) of pea sections grown in
  tissue culture on five different sugars was replicated ten times.
}
\usage{data(SRb94)}
\format{
  A data frame with 50 observations on the following 2 variables:
  \describe{
    \item{\code{L}}{length difference in mm}
    \item{\code{Treatm}}{a factor with levels "Contr", "fruct.",
      "gluc.", "gluc&fruct.", and "sucr."}
  }
}
\source{
  Sokal, R.R., and Rohlf, F.J. Biometry. Freeman, New York.
  }
\references{
  van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\examples{
data(SRb94)
with(SRb94, meansplot(L, Treatm))
}
\keyword{datasets}

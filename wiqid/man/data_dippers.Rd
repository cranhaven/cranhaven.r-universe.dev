\name{dippers}
\alias{dippers}
\docType{data}
\title{
Capture-recapture data for European dippers
}
\description{
A data set that accompanies Program MARK and is included in the \code{RMark} package in a different format under the name \code{dipper}.
}
\usage{data(dippers)}
\format{
  A data frame with 294 observations on the following 8 variables.
  \describe{
    \item{Y1, Y2, Y3, Y4, Y5, Y6, Y7}{detection histories for 294 dippers over 7 years: '1' if captured, '0' if not captured.}
    \item{sex}{sex of each bird captured.}
  }
}
\source{
Lebreton, J-D; K P Burnham; J Clobert; D R Anderson. 1992. Modeling survival and testing biological hypotheses using marked animals: a unified approach with case studies. \emph{Ecological Monographs}, 62, 67-118.
}
\references{
Analysis given in many books and papers, notably:

Cooch, E; G White 2014 (13th edition, but constantly updated). \emph{Program MARK: a gentle introduction}. Available online in PDF format at:
http://www.phidot.org/software/mark/docs/book/

}
\examples{
data(dippers)

DH <- dippers[1:7]  # Extract the detection histories
survCJS(DH)  # the phi(.) p(.) model
survCJS(DH, phi ~ .time)  # the phi(t) p(.) model

# Floods affected the 2nd and 3rd intervals
df <- data.frame(flood = c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE))
survCJS(DH, phi ~ flood, data=df)

# Including a grouping factor:
survCJS(DH, phi ~ flood * group, data=df, group=dippers$sex)

# Bayesian estimation:

\donttest{
if(requireNamespace("rjags")) {
  Bdip <- BsurvCJS(DH, parallel=FALSE)
  plot(Bdip)
  BdipFlood <- BsurvCJS(DH, list(phi ~ flood, p ~ .time), data=df, parallel=FALSE)
  BdipFlood
  op <- par(mfrow=2:1)
  plot(BdipFlood, "phi[1]", xlim=c(0.3, 0.75), main="No flood")
  plot(BdipFlood, "phi[2]", xlim=c(0.3, 0.75), main="Flood")
  par(op)
  ratio <- BdipFlood['phi[2]'] / BdipFlood['phi[1]']
  postPlot(ratio, compVal=1)
}
}
}
\keyword{datasets}

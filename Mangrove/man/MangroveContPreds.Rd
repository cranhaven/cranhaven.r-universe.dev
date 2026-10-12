\name{MangroveContPreds}
\alias{MangroveContPreds}
\alias{calcBetas}
\alias{applyBetas}
\alias{print.MangroveContPreds}
\alias{summary.MangroveContPreds}
\alias{plot.MangroveContPreds}
\title{
The MangroveContPreds class
}
\description{
Methods to generate, summarise and apply MangroveContPreds objects
}
\usage{
calcBetas(ped,betas)
applyBetas(contpred,mu,sigma)

\method{print}{MangroveContPreds}(x, ...)
\method{summary}{MangroveContPreds}(object, ...)
\method{plot}{MangroveContPreds}(x, ...)
}
\arguments{
  \item{ped}{
	A \code{\link[Mangrove]{MangrovePed}} object
}
\item{betas}{
	A \code{\link[Mangrove]{MangroveBetas}} object
}
 \item{contpred}{
	A \code{MangroveContPreds} object
}
 \item{object}{
	A \code{MangroveContPreds} object
}
 \item{x}{
	A \code{MangroveContPreds} object
}
 \item{mu}{
	The population mean of the quantitative trait being predicted
}
 \item{sigma}{
	The population standard deviation of the quantitative trait being predicted
}

  \item{\dots}{
	Arguments to be passed on to downstream methods.
}
}
\details{
\code{MangroveContPreds} objects are used to hold quantitative (or continuous) trait predictions for a number of individuals. This object contains the beta value for the trait (i.e. the person's value of the trait on a scale with the population mean being zero and the population standard deviation being one). You can generate per-individual beta values from a pedigree object and some per-variant beta values using \code{calcBetas}, and you can transform these to quantitative trait predictions given a population mean and standard deviation using \code{applyBetas}. 

The \code{print} method writes the beta value for each individual to screen. The \code{summary} method gives some quantile information, and the \code{plot} method plots a histogram of predicted values, which should be approximately normal.
}
\value{
\code{calcBetas} returns a \code{MangroveContPreds} object, \code{applyBetas} returns a numeric vector containing posterior probabilities. The methods do not return anything.
}
\seealso{
\code{\link[Mangrove]{MangroveRiskPreds}} for doing risk prediction for binary traits.
}
\examples{
data(contped)
data(exampleBetas)
predbetas <- calcBetas(contped,exampleBetas)

summary(predbetas)
plot(predbetas)

contpreds <- applyBetas(predbetas,162,6.4)

}

\keyword{classes}
\keyword{methods}

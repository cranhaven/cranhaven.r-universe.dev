\name{B1Nmean}
\alias{B1Nmean}
\title{
Bayesian analysis of one sample from a Normal distribution with  imprecise priors.
}
\description{
This function performs a standard Bayesian analysis of a single sample
of a population presumably following a Normal distribution. Imprecise
priors for the mean and the standard deviation are used.
}
\usage{
B1Nmean(x, plotit = TRUE, hists = FALSE, pdf = FALSE)
}
\arguments{
  \item{x}{
a vector of sample values
}

  \item{plotit}{
should the function produce plots? Defaults to TRUE.
}

  \item{hists}{
should histograms of the posterior distribution for the data with
twenty posterior predictive histograms also be plotted? Defaults to FALSE.
}

  \item{pdf}{
should the histograms be saved as a pdf-file? Defaults to FALSE.
}

}
\value{
none produced: text and graphical output are produced
}
\references{
van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\author{
Robert van Hulst
}

\seealso{
\code{\link{B1Nsir}}
}
\examples{
\dontrun{
data(Fat)
B1Nmean(Fat$Height)
}
}

\keyword{ models }

\keyword{ htest }

\name{MangroveBetas}
\alias{MangroveBetas}
\alias{readBetas}
\alias{print.MangroveBetas}
\alias{summary.MangroveBetas}
\alias{plot.MangroveBetas}
\title{
The MangroveBetas class
}
\description{
Functions to read in and use MangroveBetas objects
}
\usage{
readBetas(betafile,header=TRUE)

\method{print}{MangroveBetas}(x, \dots)
\method{summary}{MangroveBetas}(object, \dots)
\method{plot}{MangroveBetas}(x, \dots)
}
\arguments{
  \item{betafile}{
	The location of a beta file to read in
}
  \item{header}{
	Logical. Indicates whether the betafile above has a header.
}
\item{x}{
	A \code{MangroveBetas} object
}
\item{object}{
	A \code{MangroveBetas} object
}
  \item{\dots}{
	Additional arguments to be passed to downstream methods.
}
}
\details{
A \code{MangroveBetas} object is used to hold the risk alleles, beta values and frequencies for a set of genetic variants used to predict a quantitative trait. You can read a beta file from disk using \code{readBetas}.

The format of the beta file is a text file with 4 columns. The first should be the variant ID, the second the risk allele, the third the additive beta value and the fourth the frequency. 

The \code{print} method shows the data that the object holds. The \code{summary} method gives some measures of how predictive the variants are, including the variance explained by the variants in the object. The \code{plot} method show the cumulative variance explained as the variants are added in one-at-a-time (in order of most-to-least variance explained). 

See \code{vignette("Mangrove")} for an example of a beta file, and usage of a beta object.
}
\value{
An object of class \code{MangroveBetas} for \code{readBetas}. Nothing is returned for the methods.
}

\seealso{
\code{\link[Mangrove]{MangroveContPreds}} for performing quantitative trait prediction using this object.
}
\examples{
data(exampleBetas)
summary(exampleBetas)
plot(exampleBetas)
}

\keyword{class}
\keyword{methods}

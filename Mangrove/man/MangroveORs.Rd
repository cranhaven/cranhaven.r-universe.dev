\name{MangroveORs}
\alias{MangroveORs}
\alias{readORs}
\alias{print.MangroveORs}
\alias{summary.MangroveORs}
\alias{plot.MangroveORs}
\title{
The MangroveORs class
}
\description{
Functions to read in and use MangroveORs objects
}
\usage{
readORs(ORfile,header=TRUE)

\method{print}{MangroveORs}(x, ...)
\method{summary}{MangroveORs}(object, K = NULL, ...)
\method{plot}{MangroveORs}(x, K = NULL, ...)
}
\arguments{
  \item{ORfile}{
	The location of an odds ratio file to read in
}
  \item{header}{
	Logical. Indicates whether the ORfile above has a header.
}
\item{x}{
	A \code{MangroveORs} object
}
\item{object}{
	A \code{MangroveORs} object
}
  \item{K}{
	The prevalence of the disease that the odds ratios predict. If NULL, data is returned for a few example prevalences.
}
  \item{\dots}{
	Additional arguments to be passed to downstream methods.
}
}
\details{
A \code{MangroveORs} object is used to hold the risk alleles, odds ratios and frequencies for a set of genetic variants used to predict disease. You can read an odds ratio file from disk using \code{readORs}.

The format of the odds ratio file is a text file with 4 or 5 columns. If the file has 4 columns, the first should be the variant ID, the second the risk allele, the third the additive odds ratio and the fourth the frequency. If 5 columns exist, column three is the heterozygous odds ratio, column four is the homozygous risk odds ratio, and five is the allele frequency.

The \code{print} method shows the data that the object holds. The \code{summary} method gives some measures of how predictive the variants are, including the  variance explained by the variants in the object on the liability scale. The \code{plot} method show the cumulative variance explained on the liability scale as the variants are added in one-at-a-time (in order of most-to-least variance explained). 

See \code{vignette("Mangrove")} for an example of an odds ratio file, and usage of an odds ratio object.
}
\value{
An object of class \code{MangroveORs} for \code{readORs}. Nothing is returned for the methods.
}

\seealso{
\code{\link[Mangrove]{MangroveRiskPreds}} for performing risk prediction using this object, and \code{\link[Mangrove]{getVarExp}} for more on assessing variance explained.
}
\examples{
data(exampleORs)
summary(exampleORs)
plot(exampleORs)
}

\keyword{class}
\keyword{methods}

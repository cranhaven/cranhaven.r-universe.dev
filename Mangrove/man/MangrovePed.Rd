\name{MangrovePed}
\alias{MangrovePed}
\alias{readPed}
\alias{summary.MangrovePed}
\alias{print.MangrovePed}
\alias{plot.MangrovePed}
\title{
The MangrovePed class
}
\description{
Functions to read in and summarise MangrovePed objects
}
\usage{

readPed(prefix)

\method{summary}{MangrovePed}(object, ...)
\method{print}{MangrovePed}(x, ...)
\method{plot}{MangrovePed}(x, ...)

}
\arguments{
  \item{prefix}{
	The prefix for the file locations of a pair pedigree and map files, such as produced by the program \code{Plink}. Requires prefix.ped and prefix.map to both exist.
}
  \item{x}{
	An object of class \code{MangrovePed}
} 
  \item{object}{
	An object of class \code{MangrovePed}
} 
 \item{\dots}{
	Arguments to be passed on to other methods.
}
}
\details{
A MangrovePed object holds genotypes and family relationships for a number of individuals. You can read in such data from a Plink pedigree file using \code{readPed}.

You can show the raw pedigree data using the \code{print} method and get summary information (such as number of samples, cases and variants) using the \code{summary} method. The \code{plot} function produces an error.
}
\value{
For \code{readPed}, an object of class \code{MangrovePed}. For methods, nothing is returned.
}
\seealso{
\code{\link[Mangrove]{MangroveORs}}, \code{\link[Mangrove]{MangroveTree}}.
}
\examples{
data(famped)
summary(famped)
print(famped)
}
\keyword{classes}
\keyword{methods}
\keyword{manip}

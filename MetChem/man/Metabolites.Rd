\docType{data}

\name{Metabolites}

\alias{Metabolites}

\title{Metabolomic Dataset}

\description{
This dataset consists of a list of the metabolites as returned by the function \code{readMet} and concentration value of each metabolites.
}

\usage{data(Metabolites)}

\value{
A list with the following elements in the variable \code{Metabolites}:
	\item{concentration}{A matrix containing the concentration of each metabolites.}
	\item{name}{A vector of metabolite's names.}
	\item{SMILES}{A vector of SMILES represenation of each metabolite.}
	\item{HMDB}{A vector containing HMDB IDs of each metabolite.}
	\item{readMet}{A list of metabolites information produced by \code{\link{readMet}} function.}
	}


\examples{
 data(Metabolites)

}

\keyword{datasets}

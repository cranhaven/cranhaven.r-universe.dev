\docType{data}

\name{ChemRICH}

\alias{ChemRICH}

\title{ChemRICH Dataset}

\description{
This dataset consists of a list of the metabolites names download from https://chemrich.fiehnlab.ucdavis.edu/. HMDB IDs were retrieved from PubChem Identifier Exchange Service (https://pubchem.ncbi.nlm.nih.gov/idexchange/idexchange.cgi) and manually curated. 
}

\usage{data(ChemRICH)}

\value{
A list with the following elements in the variable \code{ChemRICH}:
	\item{name}{A vector of metabolite's names.}
	\item{SMILES}{A vector of SMILES represenation of each metabolite.}
	\item{HMDB}{A vector containing HMDB IDs of each metabolite.}
}

\examples{
 data(ChemRICH)

}

\keyword{datasets}

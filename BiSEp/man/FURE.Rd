\name{FURE}
\alias{FURE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{FURE: Functional redundancy between synthetic lethal gene pairs}
\description{Utilises gene ontology information from the GO database bioconductor package.  Assesses gene pairs output from the SLinG and BEEM tools for gene ontology functional redundancy.  Performs semantic similarity scoring utilising the GOSemSim bioconductor package}
\usage{
FURE(
	data=data, 
	inputType=inputType)
}
\arguments{
  \item{data}{This should be the output matrix (or similar) from the SLinG and BEEM tools.  Columns 1 and 2 should be gene symbols.
}
  \item{inputType}{Either 'BIGEE' or 'BEEM' based on origin of the input matrix.}
}

\value{
A list of matrices containing gene pairs with associated synthetic lethal statistical significance values + gene ontology annotation/ scores. 
%%  \item{allPairsScored}{Outputs the a matrix containing ALL input gene pairs and scoring from the SLinG or BEEM tools.  Adds columns with details of functionally redundant GO ids and functionally redundant terms.  Adds 3 columns giving semantic similarity scores for GO Molecular Function, Biological Process and Cellular Component.}
%%  \item{funcRedundantPairs}{As above but only containing gene pairs where there is functional redundancy between gene ontology terms at any level.}
}
\author{
Mark Wappett
}

\keyword{ GeneOntology }
\keyword{ SyntheticLethal }

\name{build_data}
\alias{build_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to build the elements \code{aaList}, \code{geneticCode}, \code{peptideList}, used by function \code{\link{getConstraint}}.
}
\description{
To be run before the first use of function \code{\link{getConstraint}}.
This function generates the variables \code{aaList}, \code{geneticCode} and  \code{peptideList}, a list of all the peptides (up to length 5), used by function \code{\link{getConstraint}}. 
}
\usage{
build_data()
}
%- maybe also 'usage' for other objects documented here.

\value{
\item{aaList}{Vector of all amino acids.}
\item{geneticCode}{A list describing the standard genetic code. }
\item{peptideList}{A list of all the peptides (up to length 5).}
}
\references{
Lebre and Gascuel, The combinatorics of overlapping genes 
(freely available from arXiv at: 

http://arxiv.org/abs/1602.04971). 
}
\author{
Sophie Lebre <sophie.lebre@umontpellier.fr>
}

\seealso{
\code{\link{getConstraint}}
}
\examples{
	\dontrun{
# 1) Build peptideList, used by function getConstraint.
mydata <- build_data()
aaList <- mydata$aaList
geneticCode <- mydata$geneticCode
peptideList <- mydata$peptideList

# 2) Ask for the constraint induced on a chosen peptide in a chosen overlapping frame

## amino acid contraints 
getConstraint("C", frame = -2, code=geneticCode, pepList=peptideList) 
getConstraint("D", frame = 1, code=geneticCode, pepList=peptideList)

## 3-peptide contraints
getConstraint("CWC",frame = -2, code=geneticCode, pepList=peptideList)

## 5-peptide contraints
getConstraint("CWCCC",frame = -2, code=geneticCode, pepList=peptideList)
	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

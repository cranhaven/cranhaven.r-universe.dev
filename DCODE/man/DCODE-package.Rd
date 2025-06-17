\name{DCODE-package}
\alias{DCODE-package}
\alias{DCODE}
\docType{package}
\title{
List Linear n-Peptide Constraints for Overlapping Protein Regions
}
\description{
Traversal graph algorithm for listing linear n-peptide constraints for overlapping protein regions. 
}

\author{
S. Lebre

Maintainer: S. Lebre <sophie.lebre@umontpellier.fr>
}
\references{
Lebre and Gascuel, The combinatorics of overlapping genes 
(freely available from arXiv at: 

http://arxiv.org/abs/1602.04971). 
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
\keyword{double-coding sequences}
\keyword{genetic code}
\keyword{stop codons}
\keyword{codon usage}
\keyword{amino acid composition}
\keyword{linear and logical constraints}
\keyword{overlapping gene}

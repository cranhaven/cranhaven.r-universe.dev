\name{es}
\alias{es}
\docType{data}
\title{
	A Simulated Data Set
}
\description{
	A simulated data set based on the R code provided by Law et al.'s (2014) paper.
}
\usage{data("es")}
\format{
  The format is:
Formal class 'ExpressionSet' [package "Biobase"]
}
\details{
The simulated data set contains RNA-seq counts of 1000 genes for 6 samples (3 cases and 3 controls). 
The library sizes of the 6 samples are not equal.

}
\source{
The dataset was generated based on the R code Simulation_Full.R from the website \url{http://bioinf.wehi.edu.au/voom/}.
}
\references{
    Law CW, Chen Y, Shi W, Smyth GK.  
    voom: precision weights unlock linear model analysis tools for RNA-seq read counts.
    Genome Biology. 2014; 15:R29
}
\examples{
library(Biobase)

data(es)
print(es)

# expression set
ex = exprs(es)
print(dim(ex))
print(ex[1:3,1:2])

# phenotype data
pDat = pData(es)
print(dim(pDat))
print(pDat[1:2,])

# feature data
fDat = fData(es)
print(dim(fDat))
print(fDat[1:2,])

}
\keyword{datasets}

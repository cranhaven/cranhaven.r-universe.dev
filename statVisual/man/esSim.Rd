\name{esSim}
\alias{esSim}
\docType{data}
\title{
A Simulated Gene Expression Dataset
}
\description{
A simulated gene expression dataset for differential expression analysis.
}
\usage{data("esSim")}
\format{
  The format is:
Formal class 'ExpressionSet' [package "Biobase"] with expression levels
of 100 probes for 20 samples.

The phenotype data contain 2 phenotype variables:
sid (subject id) and grp (group indicator: 1 stands for case; 0 stands for control).

The feature data contain 4 feature variables:
probeid (probe id), gene (fake gene symbol), chr (fake chromosome number),
and memProbes (probe significance indicator: 1 stands for probes over-expressed (OE) in cases; -1 stands for probes under-expressed (UE) in cases; and 0 stands for non-differentially expressed (NE) probes). There are 3 OE probes,
2 UE probes, and 95 NE probes.
}
\details{
The dataset was generated based on the R code in the manual
of the function \code{lmFit} of the R Bioconductor package \code{limma}.
There are 100 probes and 20 samples (10 controls and 10 cases). The first 3 probes are over-expressed in cases. The 4-th and 5-th probes are under-expressed in cases. The remaining 95 probes are non-differentially expressed between cases and controls. Expression levels for 100 probes were first generated from normal distribution with mean 0 and standard deviation varying between probes (\eqn{sd=0.3\sqrt{4/\chi^2_4}}{sd=0.3*sqrt(4/chi^2_4)}). For the 3 OE probes, we add 2 to the expression levels of the 10 cases. For the 2 UE probes, we subtract 2 from the expression levels of the 10 cases.

}
%\source{
%}
\references{
Please see the example in the manual for the function \code{\link{lmFit}} in the R Bioconductor package \code{limma}.
}
\examples{
data(esSim)

print(esSim)

###
dat=exprs(esSim)
print(dim(dat))
print(dat[1:2,])

###
pDat=pData(esSim)
print(dim(pDat))
print(pDat)

# subject group status
print(table(esSim$grp))

###
fDat = fData(esSim)
print(dim(fDat))
print(fDat[1:2, ])

# probe's status of differential expression
print(table(fDat$memProbes))

}
\keyword{datasets}

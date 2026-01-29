\name{GhostKnockoff.GetCorStudy}
\alias{GhostKnockoff.GetCorStudy}
\title{
Calculate study correlation
}
\description{
This function computes correlation among studies given Z-scores and the output of GhostKnockoff.prelim.
}
\usage{
GhostKnockoff.GetCorStudy(Zscore_0, fit.prelim)
}
\arguments{
  \item{Zscore_0}{
A p*K Z-score matrix, where p is the number of variants and K is the number of studies. Variants not observed in the study should be coded as NA.
}
  \item{fit.prelim}{
The output of function "GhostKnockoff.prelim".
}
}
\value{
  \item{cor.study}{
The correlation among studies.
}
}
\examples{

# We use genetic data as an example
library(GhostKnockoff)

# load example vcf file from package "seqminer", this serves as the reference panel
vcf.filename = system.file("vcf/1000g.phase1.20110521.CFH.var.anno.vcf.gz", package = "seqminer")

## this is how the actual genotype matrix from package "seqminer" looks like
example.G <- t(readVCFToMatrixByRange(vcf.filename, "1:196621007-196716634",annoType='')[[1]])
example.G <- example.G[,apply(example.G,2,sd)!=0]
example.G <- example.G[,1:100]

# compute correlation among variants
cor.G<-matrix(as.numeric(corpcor::cor.shrink(example.G)), nrow=ncol(example.G))

# fit null model
fit.prelim<-GhostKnockoff.prelim(cor.G,M=5,method='asdp',max.size=500)

# compute study correlation
Zscore_0<-cbind(rnorm(nrow(cor.G)),rnorm(nrow(cor.G))) # hypothetical Z-scores
Zscore_0<-Zscore_0+rbinom(nrow(cor.G),size=2,0.1) # set causal
cor.study<-GhostKnockoff.GetCorStudy(Zscore_0,fit.prelim)

}
\keyword{Study correlation}

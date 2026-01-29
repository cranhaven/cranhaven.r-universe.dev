\name{GhostKnockoff.prelim}
\alias{GhostKnockoff.prelim}
\title{
Preliminary data management for GhostKnockoff
}
\description{
This function does the preliminary data management and pre-computes matrices for GhostKnockoff inference, given pre-computed correlation matrix of the variants (e.g. using reference panel in genetic studies). The output will be passed to the other functions.
}
\usage{
GhostKnockoff.prelim(cor.G,M=5,method='asdp',max.size=500)
}
\arguments{
  \item{cor.G}{
The pre-computed correlation matrix of the variants.
}
  \item{M}{
Hypothetical number of knockoffs per variant. The default is 5 for multiple knockoff inference.
}
  \item{method}{
Either "sdp" or "asdp" (default: "asdp"). This determines the method that will be used to minimize the correlation between the original variables and the knockoffs.
}
  \item{max.size}{
The maximum number in each cluster in the "asdp" method. The default is 500. It will be ignored for "sdp".
}

}
\value{
  It returns a list that will be passed to function GhostKnockoff.fit().
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
GhostKnockoff.prelim(cor.G,M=5,method='asdp',max.size=500)
}

\keyword{preliminary work}

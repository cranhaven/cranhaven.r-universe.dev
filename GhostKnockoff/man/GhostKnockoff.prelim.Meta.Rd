\name{GhostKnockoff.prelim.Meta}
\alias{GhostKnockoff.prelim.Meta}
\title{
Additional preliminary data management for GhostKnockoff if multiple studies are involved
}
\description{
This function compute study dependency gamma and the optimal weights to combine multiple studies.
}
\usage{
GhostKnockoff.prelim.Meta(cor.study, n.study)
}
\arguments{
  \item{cor.study}{
The correlation among studies.
}
  \item{n.study}{
A vector of length K, where each element is the study sample size.
}
}
\value{
  \item{w_opt}{
Optimal weights to combine multiple studies.
}
  \item{gamma}{
study dependency.
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

# compute optimal weights and study dependency
n.study<-c(5000,7500)
Meta.prelim<-GhostKnockoff.prelim.Meta(cor.study, n.study)

}
\keyword{Meta analysis}

\name{GhostKnockoff.fit}
\alias{GhostKnockoff.fit}
\title{
Feature importance score generator
}
\description{
Generate original and knockoff feature importance scores given original Z-scores from multiple studies.
}
\usage{
GhostKnockoff.fit(Zscore_0,n.study,fit.prelim,gamma=1,weight.study=NULL)
}
\arguments{
  \item{Zscore_0}{
A p*K Z-score matrix, where p is the number of variants and K is the number of studies. Variants not observed in the study should be coded as NA.
}
  \item{n.study}{
A vector of length K, where each element is the study sample size.
}
  \item{fit.prelim}{
The output of function "GhostKnockoff.prelim".
}
  \item{gamma}{
The estimated study dependency. See functon "GhostKnockoff.GetGamma".
}
  \item{weight.study}{
The weights to combine multiple studies. The default is based on sample size. The optimal weights can be estimated using function "GhostKnockoff.prelim.Meta".
}
}
\value{
  \item{GK.Zscore_0}{
A vector of length p, where each element is weighted combination of original Z-scores
}
  \item{GK.Zscore_k}{
A p*M matrix, where each column is a knockoff copy of GK.Zscore_0.
}
  \item{T_0}{
Feature importance score of original data: square of GK.Zscore_0.
}
  \item{T_k}{
Feature importance score of knockoff copies: square of GK.Zscore_k.
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

### if only one study is involved
Zscore_0<-as.matrix(rnorm(nrow(cor.G))) # hypothetical Z-scores
Zscore_0<-Zscore_0+rbinom(nrow(cor.G),size=2,0.1) # set causal
n.study<-c(5000)

# knockoff scores for each block, this can be run in parallel too
GK.stat<-GhostKnockoff.fit(Zscore_0,n.study,fit.prelim,gamma=1,weight.study=NULL)

### if multiple studies are involved, for a meta-analysis

# compute study correlation
Zscore_0<-cbind(rnorm(nrow(cor.G)),rnorm(nrow(cor.G))) # hypothetical Z-scores
Zscore_0<-Zscore_0+rbinom(nrow(cor.G),size=2,0.1) # set causal
cor.study<-GhostKnockoff.GetCorStudy(Zscore_0,fit.prelim)

# note that all steps above can be run in parallel for nonoverlapping blocks of the genome.
# Then the overall study correlation can be computed by averaging cor.study of all blocks.

# compute optimal weights and study dependency using overall study correlaton
n.study<-c(5000,7500)
Meta.prelim<-GhostKnockoff.prelim.Meta(cor.study, n.study)
gamma<-Meta.prelim$gamma
weight.study<-Meta.prelim$w_opt

# knockoff scores for each block, this can be run in parallel too
GK.stat<-GhostKnockoff.fit(Zscore_0,n.study,fit.prelim,gamma=gamma,weight.study=weight.study)

}
\keyword{Knockoff inference}

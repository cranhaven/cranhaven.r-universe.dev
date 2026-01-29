\name{GhostKnockoff.filter}
\alias{GhostKnockoff.filter}
\title{
GhostKnockoff filter
}
\description{
This function calculates Q-values to select variants associated with the outcome, given the feature importance scores
}
\usage{
GhostKnockoff.filter(T_0,T_k)
}
\arguments{
  \item{T_0}{
A p*1 matrix. The feature importance score for the original variants.
}
  \item{T_k}{
A p*M matrix where M is the number of hypothetical knockoffs. The knockoff feature importance scores.
}
}
\value{
  \item{q}{
A vector of length p, where each element is a Q-value. Variants with q <= target FDR will be selected.
}
  \item{kappa}{
A vector of length p. Multiple knockoff statistics kappa.
}
  \item{tau}{
A vector of length p. Multiple knockoff statistics tau.
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

# knockoff filter
GK.filter<-GhostKnockoff.filter(GK.stat$T_0,GK.stat$T_k)
GK.filter$q

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

# knockoff filter
GK.filter<-GhostKnockoff.filter(GK.stat$T_0,GK.stat$T_k)
GK.filter$q

}

\keyword{Feature selection}

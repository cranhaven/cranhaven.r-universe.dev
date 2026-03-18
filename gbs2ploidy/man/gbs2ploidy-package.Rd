\name{gbs2ploidy-package}
\alias{gbs2ploidy-package}
\alias{gbs2ploidy}
\docType{package}
\title{
\packageTitle{gbs2ploidy}
}
\description{
\packageDescription{gbs2ploidy}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{gbs2ploidy}
\packageIndices{gbs2ploidy}
A typical analysis will begin by estimating allelic proportions using the \code{estprops} function. This is done in a Bayesian framework and is the most computationally intensive part of the analysis (i.e., depending on the size of the data set, this might take a day or more). This function depends on \code{rjags}, which means the user needs to install the stand-alone program \code{JAGS} as well. Principal component analysis and discriminant analysis are then used to obtain cytotype assignment probabilities via the \code{estploidy} function. This can be done with or without a training set of individuals with known ploidies.
}
\author{
\packageAuthor{gbs2ploidy}

Maintainer: \packageMaintainer{gbs2ploidy}
}
\references{
Gompert Z. and Mock K. (XXXX) Detection of individual ploidy levels with genotyping-by-sequencing (GBS) analysis. Molecular Ecology Resources, submitted.
}
\keyword{ package }

\examples{
## load a simulated data set
data(dat)
\dontrun{
## obtain posterior estimates of allelic proportions; short chains are used for 
## the example, we recommend increasing this to at least 1000 MCMC steps with a
## 500 step burnin
props<-estprops(cov1=t(dat[[1]]),cov2=t(dat[[2]]),mcmc.steps=20,mcmc.burnin=5,
    mcmc.thin=2)

## calculate observed heterozygosity and depth of coverage from the allele count
## data
hx<-apply(is.na(dat[[1]]+dat[[2]])==FALSE,1,mean)
dx<-apply(dat[[1]]+dat[[2]],1,mean,na.rm=TRUE)

## run estploidy without using known ploidy data
pl<-estploidy(alphas=props,het=hx,depth=dx,train=FALSE,pl=NA,set=NA,nclasses=2,
    ids=dat[[3]],pcs=1:2)

## boxplots to visualize posterior assignment probabilities by true ploidy 
## (which is known because these are simulated data)
boxplot(pl$pp[,1] ~ dat[[3]],ylab="assignment probability",xlab="ploidy")

## run estploidy with a training data set with known ploidy; the data set is 
## split into 100 individuals with known ploidy and 100 that are used for 
## inference
truep<-dat[[3]]
trn<-sort(sample(1:200,100,replace=FALSE))
truep[-trn]<-NA
plt<-estploidy(alphas=props,het=hx,depth=dx,train=TRUE,pl=truep,set=trn,
    nclasses=2,ids=dat[[3]],pcs=1:2)

## boxplots to visualize posterior assignment probabilities for individuals that
## were not part of the training set by true ploidy (which is known because 
## these are simulated data)
boxplot(plt$pp[,1] ~ dat[[3]][-trn],ylab="assignment probability",xlab="ploidy")
}
}

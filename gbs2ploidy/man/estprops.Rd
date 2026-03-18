\name{estprops}
\alias{estprops}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate allelic proportions
}
\description{
This functions uses Markov chain Monte Carlo to obtain Bayesian estimates of allelic proportions, which denote that proportion of heterozygous GBS SNPs with different allelic ratios.
}
\usage{
estprops(cov1 = NA, cov2 = NA, props = c(0.25, 0.33, 0.5, 0.66, 0.75), 
    mcmc.nchain = 2, mcmc.steps = 10000, mcmc.burnin = 1000, mcmc.thin = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cov1}{
	a P (number of SNPs) by N (number of individuals) matrix with read counts for the first allele (e.g., the non-reference allele). Numeric values should be provided for heterygous SNPs only, homozygous SNPs should be coded as missing data (i.e., `NA').
}
  \item{cov2}{
	a P (number of SNPs) by N (number of individuals) matrix with read counts for second allele (e.g., the reference allele). Numeric values should be provided for heterygous SNPs only, homozygous SNPs should be coded as missing data (i.e., `NA').
}
  \item{props}{
	a vector containing valid allelic proportions given the expected cyotypes present in the sample.
}

  \item{mcmc.nchain}{
	number of chains for MCMC.
}
  \item{mcmc.steps}{
	number of post burnin iterations for each chain.
}
  \item{mcmc.burnin}{
	number of iterations to discard from each chain as a burnin.
}
  \item{mcmc.thin}{
	thinning interval for MCMC.
}
}
\details{
	Allelic proportions are inferred from the allele counts based on the Bayesian model described in Gompert \& Mock (XXXX). Please consult this publication for a detailed description of the model. Users can modify the vector of possible allelic proportions based on expectations for their data set. For example, true allelic proportions for diploids, triploids and tetraploids are 1:1 (0.5), 1:2 or 2:1 (0.33 or 0.66), and 1:3, 2:2, or 3:1 (0.25, 0.5, or 0.75), respectively.	

}
\value{
	\code{estprops} returns a list with one component per individual. Components summarize the posterior distributions for allelic proportions. Rows correspond to different allelic proportions (as defined by `props') and columns give the 2.5th, 25th, 50th, 75th, and 97.5th quantiles of the posterior distribution for each parameter.
}
\references{
Gompert Z. and Mock K. (XXXX) Detection of individual ploidy levels with genotyping-by-sequencing (GBS) analysis. Molecular Ecology Resources, submitted.
}
\author{
Zachariah Gompert
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{

## load a simulated data set
data(dat)
\dontrun{
## obtain posterior estimates of allelic proportions; short chains are used for 
## the example, we recommend increasing this to at least 1000 MCMC steps with a 
## 500 step burnin
props<-estprops(cov1=t(dat[[1]]),cov2=t(dat[[2]]),mcmc.steps=20,mcmc.burnin=5,
    mcmc.thin=1)

## plot point estimates and 95% CIs (equal-tail probability intervals) for 
## allelic proportions for the first nine individuals
par(mfrow=c(3,3))
for(i in 1:9){
    plot(props[[i]][,3],ylim=c(0,1),axes=FALSE,xlab="ratios",ylab="proportions")
	axis(1,at=1:5,c("1:3","1:2","1:1","2:1","3:1"))
	axis(2)
	box()
	segments(1:5,props[[i]][,1],1:5,props[[i]][,5])
	title(main=paste("true ploidy =",dat[[3]][i]))
}
}
}

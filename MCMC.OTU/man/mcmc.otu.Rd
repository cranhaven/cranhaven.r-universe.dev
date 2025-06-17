\name{mcmc.otu}
\alias{mcmc.otu}
\title{
Analyzes multivariate counts data using poisson-lognormal mixed model
}
\description{
Wrapper function for MCMCglmm by Jarrod Hadfield, designed for multivariate counts data
such as in sequence-based analysis of microbial communities ("metabarcoding", variables = 
operational taxonomic units, OTUs), or in ecological applications (variables = species). 
The function aims to infer the changes in relative proportions of individual variables.
The maximum number of variables that can be processed on a laptop computer is about 200; 
more memory is required for larger numbers.
}
\usage{
mcmc.otu(fixed=NULL, random=NULL, data, y.scale="proportion", 
globalMainEffects="remove", vprior="uninf",...)
}
\arguments{
  \item{fixed}{
combination of fixed effects, as a text string. Do not use "*" symbol, list
it fully, such as: 'factor1+factor2+factor1:factor2'. 
}
  \item{random}{
A vector of names for variable-specific scalar random effects, such as 'c("effect1","effect2")'.
}
  \item{data}{
output of the otuStack() function
}
  \item{y.scale}{
By default, the modeled abundances will be expressed relative to the total
counts in the sample, effectively corresponding to proportions of total. Specify 'y.scale="absolute"' to express the results as absolute abundances. 
}
  \item{globalMainEffects}{
By default, the model will assume that the samples can vary systematically in abundance among factor combinations (i.e., there is an effect of a factor combination applicable to all variables) and remove these effects; this is analogous to normalizing the samples to total counts. Specify 'globalMainEffects="keep"' to switch this off.  
}
  \item{vprior}{
Prior for variance of user-specified random effects. By default an inverse Wishart prior with assumed variance (V) at 1 and the degree of belief parameter (nu) at 0. With 'prior="iw"' and 'prior="iw01"' nu is the number of OTUs minus 0.998, resulting in a weakly informative prior that is commonly used in this type of inference. vprior="iw" will assume large prior variance (1), vprior="iw01" will assume small prior variance (0.1). If the model has trouble converging, specify vptior="iw".
}
  \item{\dots}{
other options for MCMCglmm function, such as nitt (number of iterations), thin (tinning interval), and burnin (number of initial iterations to disregard). For a more precise inference, specify 'nitt=50000, thin=25, burnin=5000'. See MCMCglmm documentation for more details.
}
}
\details{
This function constructs priors and runs an MCMC chain to fit a Poisson-lognormal generalized linear mixed model to the multivariate  counts data.

The fixed effects for the model by default include a variable-specific intercept, global 
(non-variable-specific) main effects of fixed factors, and variable-specific effect 
for each of the listed fixed factors. With globalMainEffects="keep" the model
will not include the global main effects, resulting in them being absorbed into
the variable-specific effects.

The user-specified random effects are all assumed to be variable-specific with no covariances. 

The model includes one universal random factor: the scalar random effect of sample,
which accounts for the unequal counting effort among samples. 

Residual variances are assumed to be variable-specific with no covariances, with weakly informative 
inverse Wishart prior with variance=1 and nu=(number of variables)-0.998. 

The priors for fixed effects are diffuse gaussians with a mean at 0 and very large variances (1e+8),  
}
\value{
An MCMCglmm object. OTUsummary() function within this package summaizes these data, 
calculates all variable-wise credible intervals and p-values, and plots the results either 
as line-point-whiskers graph or a bar-whiskers graph using ggplot2 functions. 

OTUsummary() only works for experiments with a single multilevel factor or two fully 
crossed multilevel factors. 

For more useful operations on MCMCglmm objects, such as posterior.mode(), HPDinterval(), and plot(), see documentation for MCMCglmm package.
}
\references{
Elizabeth A. Green, Sarah W. Davies, Mikhail V. Matz, Monica Medina Next-generation sequencing reveals cryptic Symbiodinium diversity within Orbicella faveolata and Orbicella franksi at the Flower Garden Banks, Gulf of Mexico. PeerJ 2014 https://peerj.com/preprints/246/ 
}
\author{
Mikhail V. Matz, University of Texas at Austin
<matz@utexas.edu>
}
\seealso{
OTUsummary(),MCMCglmm()
}
\examples{
	
# Symbiodinium sp diversity in two coral species at two reefs (banks)
data(green.data)

# removing outliers
goods=purgeOutliers(
	data=green.data,
	count.columns=c(4:length(green.data[1,])),
	zero.cut=0.25 # remove this line for real analysis
)

# stacking the data table
gs=otuStack(
	data=goods,
	count.columns=c(4:length(goods[1,])),
	condition.columns=c(1:3)
	)

# fitting the model
mm=mcmc.otu(
	fixed="bank+species+bank:species",
	data=gs,
	nitt=3000,burnin=2000 # remove this line for real analysis!
	)

# selecting the OTUs that were modeled reliably
acpass=otuByAutocorr(mm,gs)

# calculating effect sizes and p-values:
ss=OTUsummary(mm,gs,summ.plot=FALSE)

# correcting for mutliple comparisons (FDR)
ss=padjustOTU(ss)

# getting significatly changing OTUs (FDR<0.05)
sigs=signifOTU(ss)

# plotting them
ss2=OTUsummary(mm,gs,otus=sigs)

# bar-whiskers graph of relative changes:
# ssr=OTUsummary(mm,gs,otus=signifOTU(ss),relative=TRUE)

# displaying effect sizes and p-values for significant OTUs
ss$otuWise[sigs]

}

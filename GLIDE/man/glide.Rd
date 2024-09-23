\name{glide}
\alias{glide}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Evaluate individual and global pleiotropy among genotypes being used as instrumental variables
in Mendelian randomization studies.
}
\description{
Glide is a function to perform global and individual tests for assessing direct effects in
Mendelian randomization studies.
}
\usage{
glide(formula,exposure_coeff=NULL,genotype_columns=NULL,data,
np=100000,qcutoff=0.2,parallel=TRUE,corenumber=1,verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
An object of class "formula": outcome ~ adjusting covariates, specifying the logistic regression
of a binary study outcome with respect to covariates being adjusted for in addition to genotype
data.
}
  \item{exposure_coeff}{
A named numeric vector or a one-column dataframe for the external regression coefficients
for genotype-exposure associations. These coefficients are typically derived from previous
genome-wide association studies.
All the names or rownames must be found in "genotype_columns" (specified below) columns of the
input data.
}
  \item{genotype_columns}{
An integer vector that  contains column indices of data columns in which genotype data are stored.  
}
  \item{data}{
A data frame composed of formula-related variables and SNP genotype columns.
}
  \item{np}{
An integer for the number of observations in the parametric simulation being generated 
to compute expected p-values and q-values.
}
  \item{qcutoff}{
The q-value cutoff to declare significance of the direct effects of individual variants. 
The default value is set to be 0.2.
}
  \item{parallel}{
If TRUE, use parallel foreach to speed up computation.
}
  \item{corenumber}{
An integer to set the number of computer cores used in parallel computation. If the requested
number of cores is greater than number of cores available, GLIDE will user the latter number.
}
  \item{verbose}{
If TRUE, print on screen the computing time spent in each step of GLIDE.
}
}
\details{
This function fits a logistic regression for each SNP being evaluated for instrumental variables, 
regressing outcome on one genotype at a time and adjusting covariates as specified in the
formula (outcome ~ adjusting covariates). The p-values for the genotypes derived from these
regression models were compared to the distribution of simulation-based null p-values to
declare significance of individual variant and global pleiotropy. See Dai et al. (2017) for
details of the algorithm.
}
\value{
%%  ~Describe the value returned
A data frame containing observed p-value, expected null p-value, family wise error rate (FWER),
q-value, genetic association with exposure, and variance of the estimated association of 
each SNP.
}
\references{
J. Y. Dai, U. Peters, X. Wang, J. Kocarnik, J. et al. Diagnostics of pleiotropy in Mendelian
randomization studies: Global and individual tests for direct effects. Am J Epidemiol 
187(12):2672-80, 2018.

}
\author{
James Y. Dai and X. Wang
}

\examples{
##load an example data.
data(simdata)
#The example dataset is a list composed of two dataframes. 
#simat stores 20,000 observations of 81 variables, including outcome, 5 ajusting covariates, 
#and 75 SNPs.
simdat=simdata$simdat
#coeff stores the 75 external regression cofficients.
coeff=simdata$coeff
#We define the regression formula for outcome and adjusting covariates
formula=as.formula("outcome~age+sex+pc1+pc2+pc3")
#We next define the columns in simdat that contain genotype data
genotype_columns=which(grepl("^SNP",colnames(simdat)))

\donttest{
##run glide
out=glide(formula=formula,exposure_coeff=coeff,genotype_columns,data=simdat,np=100000,
qcutoff=0.2,parallel=TRUE,corenumber=1,verbose=TRUE)

##use more cores
out=glide(formula=formula,exposure_coeff=coeff,genotype_columns,data=simdat,np=100000,
qcutoff=0.2,parallel=TRUE,corenumber=2,verbose=TRUE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Causal Inference}
\keyword{Mendelian Randomization}
\keyword{Pleiotropy}

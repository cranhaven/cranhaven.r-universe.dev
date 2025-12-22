\name{gsfPEN}
\alias{gsfPEN}
\title{
SummaryLasso incorporating multiple traits and functional annotations of SNPs.
}
\description{
SummaryLasso to model pleiotropy by introducing a group-Lasso type penalty, which is 
sensitive to select SNPs modestly associated with multiple traits and to incorporate functional 
annotations of SNPs simultaneously.
}
\usage{
gsfPEN(summaryZ, Nvec, plinkLD, NumIter = 1000, RupperVal = NULL, 
breaking = 1, numChrs = 22, ChrIndexBeta = 0, Init_summaryBetas = 0, 
Zscale = 1, tuningMatrix = NULL, penalty = "mixLOG", funcIndex, 
numfunc, p.Threshold = NULL, p.Thresholdpara = c(0.5, 10^-4, 4), 
taufactor = c(1/25, 1, 3), llim_length = 4, subtuning = 4, 
Lambda_limit = c(0.5, 0.9), Lenlam = 4, lambdavec_func = NULL, 
lambdavec_func_limit_len = c(1.5, 3), dfMax = NULL, outputAll = 0, 
warmStart = 0, customed = 0, AllTuningMatrix = NULL, SDvec = NULL, 
IniBeta = 0)
}
\arguments{
  \item{summaryZ}{
The Z statistics of p SNPs from q GWA studies. A matrix with dimension p x q for p SNPs and q traits. 
The first column corresponds to the primary trait and the rest columns correspond to the secondary traits.
}
  \item{Nvec}{
A vector of length q for the sample sizes of q GWA studies.
}
  \item{plinkLD}{
.ld file obtained from the LD calculation from plink.
}
  \item{NumIter}{
The number of maximum iterations for the estimation procedure. 
}
  \item{RupperVal}{
The maximum tolerable magnitude of the estimates of coefficients during the iterations. 
This is to avoid certain estimates of coefficients to diverge during the iterations. This may happen 
when the signs of the correlation coefficients were estimated incorrectly. The default value is 50 
times the maximum of coefficients from the input in absolute values.}
  \item{breaking}{
A binary (0,1) variable to check if there are some certain estimates of coefficients to diverge 
during the iterations. This may happen when the signs of the correlation coefficients were 
estimated incorrectly. The default value is 1.
}
  \item{numChrs}{
The number of chromosomes used in the analysis. Current version of package does not use this argument. 
}
  \item{ChrIndexBeta}{
The chromosome index for each SNP. Current version of packge does not use this argument. 
}
  \item{Init_summaryBetas}{
Can be used to set the initial values of the coefficients for the iterative estimation. 
}
  \item{Zscale}{
A binary (0,1) variable to make the coefficients from different GWA studies with unequal sample sizes comparable.
 The default value is 1.}
  \item{tuningMatrix}{
Inputs for the tuning values of the tuning parameters. Default is null and it will be generated automatically.
}
  \item{penalty}{
Current version of pacakge does not use this argument.
}
  \item{funcIndex}{
Inputs for the functional annotations of SNPs. A p x k matrix with (0,1) entry; p is the number of SNPs and 
k is the number of functional annotations. For the element at i-th row, j-th column, the entry 0 means 
SNP i without j-th functional annotation; entry 1 means otherwise.
}
  \item{numfunc}{
The number of functional annotations.
}
  \item{p.Threshold}{
The p-values threshold to set up the tuning values of the baseline tuning parameter.
}
  \item{p.Thresholdpara}{
When p.Threshold is null, p.Threshold will be generated automatically based on the values of p.Thresholdpara.
The default values are c(0.5, 10^4, 5), where the first element is the maximum of the p-value threshold, 
the second element is the minimum, and the third element is total number of p-value thresholds to be 
generated from the minimum to the maximum.
}
  \item{taufactor}{
The weights to generate the tuning values for the tuning paramter "tau" and the default is c(1/25, 1, 10) 
times the median of the p summation of the coefficients for each SNP across q traits.
}
  \item{llim_length}{
The argument to set up the number of tuning values for lambdas between the lower and upper bound. 
The default value is 10.
}
  \item{subtuning}{
The argument to set up the number of tuning values for lambdas between the lower and upper bound. 
The default value is 50.
}
  \item{Lambda_limit}{
The quantiles to set up the tuning values of lambda. The default value is c(0.5, 0.9).
}
  \item{Lenlam}{
The number of tuning values for lambda parameter without using the Log penalty. In other words, the 
initial \code{Lenlam} rows of the tuningMatrix are for summayLasso single trait analysis.
}
  \item{lambdavec_func}{
The tuning values for the tuning parameters associated with the functional annotations.
}
  \item{lambdavec_func_limit_len}{
When lambdavec_func is null, lambdavec_func will be generated automatically based on the arguments of 
\code{lambdavec_func_limit_len}. The default values are c(1.5, 4). The first element is the maximum of the 
tuning value and the second element is the total number of the tuning values to be 
generated from 0 to the maximum.}
  \item{dfMax}{
The upper bound of the number of non-zero estimates of coefficients for the primary trait.
}
  \item{outputAll}{
For internal checking usage. The default value is 0.
}
  \item{warmStart}{
For internal checking usage. The default value is 0.
}
  \item{customed}{
For internal checking usage. The default value is 0.
}
  \item{AllTuningMatrix}{
For internal checking usage. The default value is NULL.
}
  \item{SDvec}{
The matrix of the standard error for regression coefficients. When the input of SummaryZ is at Z scale, 
let SDvec = NULL and it will be computed internally. 
}
  \item{IniBeta}{
A binary (0,1) variable to indicate if the regression coefficients need to be initialized or not. 1 is for yes.
}
}
\details{
Note that the tuning values for the tuning parameters may need to be modified manually when the selected 
optimal tuning parameters are at the boundary of the inputs.}
\value{
\item{BetaMatrix}{The output of the coefficients matrix with dimensions (total number of combinations 
	of the tuning values times (pq)). Each column represents the vectorization of the p x q coefficients 
	matrix given a particular combination of the tuning values (stacking its columns into a column vector). 
	} 
\item{Numitervec}{This vector shows the number of iterations to converge for each combination 
	of the tuning values.}
\item{AllTuningMatrix}{This matrix shows all combination of tuning values used in the estimation process. 
	Its dimension is that total number of combinations 
	of the tuning values times total number of tuning parameters.}
}
\references{
This R packages is based on the method introduced in the manuscript "A comprehensive statistical framework for building polygenic risk prediction models based on summary statistics of genome-wide association studies."
}
\author{
Ting-Huei Chen
}


\examples{
data("summaryZ")
data("Nvec")
data("plinkLD")
data("funcIndex")
output = gsfPEN(summaryZ=summaryZ, Nvec=Nvec, plinkLD=plinkLD,funcIndex=funcIndex, 
numfunc=ncol(funcIndex))
}


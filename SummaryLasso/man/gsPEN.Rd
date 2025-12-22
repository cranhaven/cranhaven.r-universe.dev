\name{gsPEN}
\alias{gsPEN}
\title{
SummaryLasso incorporating multiple traits
}
\description{
	SummaryLasso to model pleiotropy by introducing a group-Lasso type penalty, 
	which is sensitive to select SNPs modestly associated with multiple traits. 
}
\usage{
gsPEN(summaryZ, Nvec, plinkLD, NumIter = 100, breaking = 1, numChrs = 22, 
ChrIndexBeta = 0, Init_summaryBetas = 0, Zscale = 1, RupperVal = NULL, 
tuningMatrix = NULL, penalty = c("mixLOG"), taufactor = c(1/25, 1, 10), 
llim_length = 10, subtuning = 50, Lambda_limit = c(0.5, 0.9), 
Lenlam_singleTrait = 200, dfMax = NULL, IniBeta = 0, inverseTuning = 0, 
outputAll = 0, warmStart = 1)
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
.ld file of the LD calculation from plink.
}
  \item{NumIter}{
The number of maximum iteraions for the estimation procedure. 
}
  \item{breaking}{
A binary (0,1) variable to check if there are some certain estimates of coefficients to diverge during 
the iterations. This may happen when the signs of the correlation coefficinets were estimated incorrectly. 
The default value is 1.
}
  \item{numChrs}{
The number of chromosomes used in the analysis. Current version of pacakge does not use this argument.

}
  \item{ChrIndexBeta}{
The chromosome index for each SNP. Current version of pacakge does not use this argument.
}
  \item{Init_summaryBetas}{
Can be used to set the initial values of the coefficients for the iterative estimation. 

}
  \item{Zscale}{
A binary (0,1) variable to make the coefficients from different GWA studies with unequal sample sizes comparable.
 The default value is 1.
}
  \item{RupperVal}{
The maximum tolerable magnitude of the estimates of coefficients during the iterations. 
This is to avoid a certain estimates of coefficients to diverge during the iterations. This may happen when 
the signs of the correlation coefficinets were estimated incorrectly. The default value is 50 times the 
maximum of coeffcients from the input in absolute values.
}
  \item{tuningMatrix}{
Inputs for the tuning values of the tuning parameters. Default is null and it will be generated automatically. 
}
  \item{penalty}{
Current version of pacakge does not use this argument. 
}
  \item{taufactor}{
The weights to generate the tuning values for the tuning paramter "tau" and the default is c(1/25, 1, 10) 
times the median of the p summation of the coefficients for each SNP across q traits.
}
  \item{llim_length}{
The argument to set up the number of tuning values for lambdas between the lower and upper bound. The default 
value is 10.
}
  \item{subtuning}{
The argument to set up the number of tuning values for lambdas between the lower and upper bound. The default 
value is 50.
}
  \item{Lambda_limit}{
The quantiles to set up the tuning values of lambda. The default value is c(0.5, 0.9).
}
  \item{Lenlam_singleTrait}{
The quantiles to set up the tuning values of lambda for single trait analysis.
}
  \item{dfMax}{
The upper bound of the number of non-zero estimates of coefficients for the primary trait.
}
  \item{IniBeta}{
A binary (0,1) variable to indicate if the regression coefficients need to be initialized or not. 1 is for yes.
}
  \item{inverseTuning}{
For internal checking usage. The default value is 0.

}
  \item{outputAll}{
For internal checking usage. The default value is 0.

}
  \item{warmStart}{
For analysis with single trait or multiple traits without functional annotations, it is recommended to use 
warmStart = 1 to enhance computations. 
}
}
\details{
Note that the tuning values for the tuning parameters may need to be modified manually when the selected 
optimal tuning parameters are at the boundary of the inputs.
}
\value{
\item{BetaMatrix}{The output of the coefficients matrix with dimensions (total number of combinations 
	of the tuning values times (pq)). Each column represents the vectorization of the p x q coefficients 
	matrix given a particular combination of the tuning values (stacking its columns into a column vector). 
	} 
\item{Numitervec}{This vector shows the number of iterations to converge for each combination 
	of the tuning values.}
\item{AllTuningMatrix}{This matrix shows all combination of tuning values used in the estimation process. 
	Its dimension is that total number of combinations of the tuning values times total number of tuning parameters.}
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
output = gsPEN(summaryZ=summaryZ, Nvec=Nvec, plinkLD=plinkLD)

}


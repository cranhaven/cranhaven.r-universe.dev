\name{hct_method_MC_corr}
\alias{hct_method_MC_corr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  %%  ~~function to do ... ~~
Estimate PCC of HCT Classifiers constructed with correlated features using Monte Carlo simulations
}
\description{
	%%  ~~ A concise (1-5 lines) description of what the function does. ~~
		Determine the probability of correct classification (PCC) for studies employing high dimensional features for classification. 
		It is assumed that a Higher Criticism Threshold (HCT) is used to choose the p-value threshold for feature selection and that features meeting the threshold are important for classification. 
		In addition to the original HCT procedure by (Donoho and Jin 2009), two procedures to choose p-value threshold have been 
		implemented (See hct_empirical and hct_beta). This function is similar to hct_method_corr but this does not rely on the normal CDF to approximate the PCC. Instead training and testing datasets are generated at each iteration of the algorithm.
}
\usage{
	hct_method_MC_corr(mu0, p, m, n, hct, alpha0, nrep, p1=0.5, 
	ss=FALSE, ntest, pcorr, chol.rho,sampling.p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{mu0}{
		%%     ~~Describe \code{mu0} here~~
			The effect size of the important features. 
	}
	\item{p}{
		%%     ~~Describe \code{p} here~~
			The number of the features in total. 
	}
	\item{m}{
		%%     ~~Describe \code{m} here~~
			The number of the important features. 
	}
	\item{n}{
		%%     ~~Describe \code{n} here~~
			The total sample size for the two groups.
	}
	\item{hct}{
		%%     ~~Describe \code{hct} here~~
			The HCT procedure employed to choose the p-value threshold for feature selection. There are two valid choices (case sensitive): 
				1) hct_empirical, the HCT procedure originally proposed by (Donoho and Jin 2009); 
		2) hct_beta, an alternative HCT procedure which makes use of the beta distribution of the p-values under the null; 
	}
	\item{alpha0}{
		%%     ~~Describe \code{alpha0} here~~
			The proportion of the smallest p-values we will consider in the HCT algorithm. 
	}
	\item{nrep}{
		%%     ~~Describe \code{nrep} here~~
			The number of simulation replicates employed to compute the expected PCC and/or sensitivity and specificity. 
	}
	\item{p1}{
		%%     ~~Describe \code{p1} here~~
			The prevalence of the group 1 in the population, default to 0.5.
	}
	\item{ss}{
		%%     ~~Describe \code{ss} here~~
			Boolean variable, default to FALSE. The TRUE value instruct the program to compute the sensitivity and the specificity of the classifier. 
	}
    \item{ntest}{
    %%     ~~Describe \code{ss} here~~
			Sample size for the test dataset. 
	}
  \item{pcorr}{
    %%     ~~Describe \code{ss} here~~
			Number of correlated features.
	}
  \item{chol.rho}{
    %%     ~~Describe \code{ss} here~~
  		Cholesky decomposition of the covariance of the pcorr features that are correlated. It is assumed that the m important features are part of the pcorr correlated features.   
	}
   \item{sampling.p}{
		%%     ~~Describe \code{sampling.p} here~~
			The assumed proportion of group 1 samples in the training data; default of 0.5 assumes groups are equally represented regardless of p1.
	}
}

\value{
	%%  ~Describe the value returned
		%%  If it is a LIST, use
		%%  \item{comp1 }{Description of 'comp1'}
	%%  \item{comp2 }{Description of 'comp2'}
	%% ...
If ss=FALSE, the function returns the expected PCC. 
If ss=TRUE, the function returns a vector containing the expected PCC, sensitivity and specificity.

}
\references{
	%% ~put references to the literature/web site here ~
Donoho, D., and Jin J. (2009). "Feature Selection by Higher Criticism Thresholding Achieves the Optimal Phase Diagram." Philosophical Transactions. Series A, Mathematical, Physical, and Engineering Sciences 367 (1906) (November 13): 4449-4470.

Sanchez, B.N., Wu, M., Song, P.X.K., and Wang W. (2016). "Study design in high-dimensional classification analysis." Biostatistics, in press.
}
\author{
	%%  ~~who you are~~
Meihua Wu <meihuawu@umich.edu>
Brisa N. Sanchez <brisa@umich.edu>
Peter X.K. Song <pxsong@umich.edu> 
Raymond Luu <raluu@umich.edu>
Wen Wang <wangwen@umich.edu>
}

\examples{
## Sigma_1 in the paper
#first block is pcorr x pcorr of compound symmetry
#other diagonal block is Identity; off diagonal blocks are 0
pcorr=10  
p=500
rho.cs=.8
#create first block
rho=matrix(rep(0,p^2),nrow=p)
rho[1:pcorr,1:pcorr]=rho.cs
diag(rho)=rep(1,p)
chol.rho1.500=chol(rho[1:pcorr,1:pcorr])
set.seed(1)
hct_method_MC_corr(mu0=0.4, p=500, m=10, n=80, hct=hct_beta, alpha0=0.5, nrep=10, 
p1 = 0.5, ss=TRUE, ntest=100, pcorr=10, chol.rho=chol.rho1.500,sampling.p=0.5)
#return: 0.673 0.686 0.660
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

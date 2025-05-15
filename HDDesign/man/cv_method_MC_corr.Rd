\name{cv_method_MC_corr}
\alias{cv_method_MC_corr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  %%  ~~function to do ... ~~
	MC simulation-based method to calculate the PCC of a CV-based classifier when features are correlated; uses training and testing datasets.
}
\description{
	%%  ~~ A concise (1-5 lines) description of what the function does. ~~
	Determine the probability of correct classification (PCC) for a high dimensional classification study employing 
	Cross validation classifier. This is similar to cv_method_MC, but instead features generated are correlated. 
}
\usage{
	cv_method_MC_corr(mu0, p, m, n, alpha_list, nrep, p1 = 0.5, ss = F, ntest, 
	pcorr, chol.rho,sampling.p=0.5)
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
	\item{alpha_list}{
		%%     ~~Describe \code{alpha_list} here~~
		The search grid for the p-value threshold. 
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
\details{
	%%  ~~ If necessary, more details than the description above ~~
Refer to Sanchez, Wu, Song, Wang 2016, supplementary materials. This function is used to verify if a study using the sample sizes in Table 1 of the manuscript attains the PCC target via MC simulations. 
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
	cv_method_MC_corr(mu0=0.4,p=500,m=10,n=80,alpha_list=c(0.0000001,0.0001,0.01),
	nrep=10,p1=0.6,ss=TRUE,ntest=100,pcorr=10,chol.rho=chol.rho1.500)
	#return: 0.623 0.670 0.576
	#alpha_list should be a dense list of p-value cutoffs; 
	#here we only use a few values to ease computation of the example.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

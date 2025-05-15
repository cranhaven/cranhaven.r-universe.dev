\name{cv_method_MC}
\alias{cv_method_MC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  %%  ~~function to do ... ~~
	MC simulation-based method to calculate the PCC of a CV-based classifier; calculated using training and test datasets in MC simulations.
}
\description{
	%%  ~~ A concise (1-5 lines) description of what the function does. ~~
	Determine the probability of correct classification (PCC) for a high dimensional classification study employing 
	Cross validation classifier. In contrast to the cv_method this function also generates a test dataset so that the estimated PCC
  does not rely on the normal approximation for the PCC formula. 
}
\usage{
	cv_method_MC(mu0, p, m, n, alpha_list, nrep, p1 = 0.5, ss = F, ntest, 
	sampling.p=0.5)
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
			The total sample size for the two groups, that would be used to develop the classifier.
	}
	\item{alpha_list}{
		%%     ~~Describe \code{alpha_list} here~~
		The search grid for the p-value threshold. The examples below use only three values for the sake of giving examples that run quickly but this should ideally be a dense grid,
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
	\item{sampling.p}{
		%%     ~~Describe \code{sampling.p} here~~
			The assumed proportion of group 1 samples in the training data; default of 0.5 assumes groups are equally represented regardless of p1.
	}
}
\details{
	%%  ~~ If necessary, more details than the description above ~~
Refer to  Sanchez, Wu, Song, Wang 2016, Section 2.2. This function was used to verify that a given sample size achieves the target PCC in Table 1 of the manuscript. 
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
Meihua Wu <meihuawu@umich.edu>
Brisa N. Sanchez <brisa@umich.edu>
Peter X.K. Song <pxsong@umich.edu> 
Raymond Luu <raluu@umich.edu>
Wen Wang <wangwen@umich.edu>
}

\examples{
	set.seed(1)
	cv_method_MC(mu0=0.4,p=500,m=10,n=80,alpha_list=c(0.0000001,0.0001,0.01),
	nrep=10,p1=0.6,ss=TRUE,ntest=100)
#return: 0.818 0.882 0.754
#alpha_list should be a dense list of p-value cutoffs; 
#here we only use a few values to ease computation of the example.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

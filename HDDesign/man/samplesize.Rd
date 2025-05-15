\name{samplesize}
\alias{samplesize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	%%  ~~function to do ... ~~
Determine the Sample Size Requirement
}
\description{
	%%  ~~ A concise (1-5 lines) description of what the function does. ~~
		Determine the sample size to achieve the target probability of correct classification (PCC) using various design methods. Sample sizes are chosen using a binary search algorithm between the range nmin to nmax.
}
\usage{
	samplesize(target, nmin, nmax, f, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{target}{
		%%     ~~Describe \code{target} here~~
			Set the target probability of correct classifcation (PCC) for the study.
	}
	\item{nmin}{
		%%     ~~Describe \code{nmin} here~~
			The mimimum sample size for both groups combined. Typically 0.05 smaller than the ideal PCC. It must be an even number. 
	}
	\item{nmax}{
		%%     ~~Describe \code{nmax} here~~
			The maximum sample size for both groups combined. So it must be an even number. 
	}
	\item{f}{
		%%     ~~Describe \code{f} here~~
			Specify the PCC estimation function: ds_method, cv_method, or hct_method
	}
	\item{\dots}{
		%%     ~~Describe \code{\dots} here~~
			The design assumptions and other arguments for the PCC estimation function, f. 
	}
}

\value{
	%%  ~Describe the value returned
		%%  If it is a LIST, use
		%%  \item{comp1 }{Description of 'comp1'}
	%%  \item{comp2 }{Description of 'comp2'}
	%% ...
	The smallest sample size that achieves the target PCC. 
	
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
set.seed(1)
samplesize(target=0.8, nmin=20, nmax=100, hct_method, mu0=0.4, p=500, 
m=10, hct=hct_beta, alpha0=0.5, nrep=100) 
#return: 78.0000000  0.8043205
}
	% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.


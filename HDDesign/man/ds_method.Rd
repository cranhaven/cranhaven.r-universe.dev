\name{ds_method}
\alias{ds_method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate PCC by DS Method
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Determine the probability of correct classification (PCC) for studies employing high dimensional features for classification; 
uses the method proposed by (Dobbin and Simon 2007) to choose the p-value threshold for feature selection.
}
\usage{
	ds_method(mu0, p, m, n, p1=0.5, lmax=1, ss=F, sampling.p)
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
  \item{p1}{
%%     ~~Describe \code{p1} here~~
The prevalence of the group 1 in the population, default to 0.5.
}

  \item{lmax}{
%%     ~~Describe \code{lmax} here~~  
The maximum eigenvalue of the variance-covariance matrix of the p features. Defaults to 1 which implies that the features are assumed i.i.d.
  }
  \item{ss}{
%%     ~~Describe \code{ss} here~~
Boolean variable, default to FALSE. The TRUE value instruct the program to compute the sensitivity and the specificity of the classifier. 
}
\item{sampling.p}{
		%%     ~~Describe \code{sampling.p} here~~
			The assumed proportion of group 1 samples in the training data; default of 0.5 assumes groups are equally represented regardless of p1.
	}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Refer to Dobbin and Simon (2007)
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
Dobbin, K.K., and Simon R.M. (2007). "Sample Size Planning for Developing Classifiers Using High-dimensional DNA Microarray Data." Biostatistics 8 (1): 101-117. 
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
ds_method(mu0=0.6, p=500, m=10, n=38, p1=0.5, lmax=1, ss=TRUE)
#[1] 0.9252471 0.9252471 0.9252471
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

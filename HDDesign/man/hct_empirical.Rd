\name{hct_empirical}
\alias{hct_empirical}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Original HCT Procedure to Choose P-Value Threshold for Feature Selection
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This is the original Higher Criticism Threshold (HCT) procedure (Donoho and Jin 2009) to choose p-value threshold for feature selection. 
Only the features whose p-values are less than the thresold will be included in the classifier. 
}
\usage{
hct_empirical(pvalue, p, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pvalue}{
%%     ~~Describe \code{pvalue} here~~
A vector containing the p*alpha_0 smallest p-values.
}
  \item{p}{
%%     ~~Describe \code{p} here~~
The number of the features in total.
}
\item{n}{
		%%     ~~Describe \code{n} here~~
			The total sample size for the two groups.
	}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Refer to (Donoho and Jin 2009)
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
The p-value threshold for feature selection. Only the features whose p-values are less than the thresold will be included in the classifier. 
}
\references{
%% ~put references to the literature/web site here ~
Donoho, D. and Jin, J. 2009. "Feature Selection by Higher Criticism Thresholding Achieves the Optimal Phase Diagram." Philosophical Transactions. Series A, Mathematical, Physical, and Engineering Sciences 367 (1906): 4449-4470.
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
hct_empirical(pvalue=0.10,p=500,n=80)
# 0.1
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

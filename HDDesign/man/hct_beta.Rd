\name{hct_beta}
\alias{hct_beta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Alternative HCT Procedure to Choose P-Value Threshold Based on Beta Distribution of P-Values.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This procedure chooses the p-value threshold for feature selection in a similar fashion to hct_empirical. However, it is based on the Beta distribution of the p-values. 
Only the features whose p-values are less than the thresold will be included in the classifier. 

}
\usage{
hct_beta(pvalue, p, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pvalue}{
%%     ~~Describe \code{pvalue} here~~
A vector containing the p*alpha_0 smallest p-values; typically alpha_0=0.10
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
Refer to Sanchez, et al (2016), Section 3 and supplementary materials. 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
The p-value threshold for feature selection. Only the features whose p-values are less than the threshold will be included in the classifier. 
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
hct_beta(pvalue=0.10,p=500,n=80)
# 0.1
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

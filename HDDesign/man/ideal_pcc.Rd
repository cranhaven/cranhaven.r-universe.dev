\name{ideal_pcc}
\alias{ideal_pcc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Determine the Ideal PCC
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Determine the probability of correct classification (PCC) for a study employing the ideal classifier. The ideal classifier is constructed
assuming we know exactly the important features and their effect size. The ideal PCC is the uppper bound of the PCC of any linear classifiers. 
}
\usage{
ideal_pcc(mu0, m, p1 = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

	\item{mu0}{
		%%     ~~Describe \code{mu0} here~~
			The effect size of the important features. 
	}
	\item{m}{
		%%     ~~Describe \code{m} here~~
			The number of the important features. 
	}
	\item{p1}{
		%%     ~~Describe \code{p1} here~~
			The prevalence of the group 1 in the population, default to 0.5.
	}
}

\value{
	%%  ~Describe the value returned
		%%  If it is a LIST, use
		%%  \item{comp1 }{Description of 'comp1'}
	%%  \item{comp2 }{Description of 'comp2'}
	%% ...
	The PCC of the ideal classifier. 
}
\references{
	%% ~put references to the literature/web site here ~
	
Dobbin, Kevin K., and Richard M. Simon. 2007. "Sample Size Planning for Developing Classifiers Using High-dimensional DNA Microarray Data." Biostatistics 8 (1) (January 1): 101-117. 

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
ideal_pcc(mu0=0.4, m=10, p1 = 0.6) 
#return: 0.8999055
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

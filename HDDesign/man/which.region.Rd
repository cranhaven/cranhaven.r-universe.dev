\name{which.region}
\alias{which.region}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Determine the Feasibility Region
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Given the design assumption, determine which feasiblity region the design problem belongs to.
The feasibility region is constructed from the asymptotic properties of the rare-and-weak model (Donoho and Jin 2009). 
The two groups are assumed to be equally proportioned, i.e. p_+1=p_-1=0.5.
If the the problem is feasible, then the probability of correct classification (PCC) of the HCT classifer 
will approach 1 when the number of features goes to infinity. 
If the the problem is unfeasible, then the probability of correct classification (PCC) of any linear classifer 
will approach 0.5  when the number of features goes to infinity. 
}
\usage{
which.region(mu0, p, m, n)
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
}

\value{
	%%  ~Describe the value returned
		%%  If it is a LIST, use
		%%  \item{comp1 }{Description of 'comp1'}
	%%  \item{comp2 }{Description of 'comp2'}
	%% ...
	\item{0}{The classification problem belongs to the unfeasible region.}
	\item{1}{The classification problem belongs to the feasible region.}
	\item{2}{The classification problem belongs to the feasible region.}
	\item{3}{The classification problem belongs to the feasible region.}
	\item{4}{The classification problem belongs to the feasible region.}

	Region 1-4 are all feasible regions. Their difference is discussed in more details in ().
}
\references{
	%% ~put references to the literature/web site here ~
Donoho, David, and Jiashun Jin. 2009. "Feature Selection by Higher Criticism Thresholding Achieves the Optimal Phase Diagram." Philosophical Transactions. Series A, Mathematical, Physical, and Engineering Sciences 367 (1906) (November 13): 4449-4470.
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
	which.region(mu0=0.4, p=500, m=10, n=80) 
	#return: 4
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

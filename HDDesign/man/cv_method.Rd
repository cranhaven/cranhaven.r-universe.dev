\name{cv_method}
\alias{cv_method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	%%  ~~function to do ... ~~
	Formula-based PCC of a CV-based classifier
}
\description{
	%%  ~~ A concise (1-5 lines) description of what the function does. ~~
	Determine the probability of correct classification (PCC) for a high dimensional classification study employing 
	cross validation to determine an optimal p-value cutoff to select features that are included in a linear classifier. 
}
\usage{
	cv_method(mu0, p, m, n, alpha_list, nrep, p1 = 0.5, ss = F, sampling.p=0.5)
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
	\item{sampling.p}{
		%%     ~~Describe \code{sampling.p} here~~
			The assumed proportion of group 1 samples in the training data; default of 0.5 assumes groups are equally represented regardless of p1.
	}
}
\details{
	%%  ~~ If necessary, more details than the description above ~~
Refer to Sanchez, Wu, Song, Wang 2016, Section 2.2 for the details of the algorithm. This function was used to produce Figure 2 of the paper. 
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
	set.seed(1)
	cv_method(mu0=0.4, p=500, m=10, n=80, alpha_list=c(0.0000001, 0.0001, 0.01), 
	nrep=10, p1=0.6, ss=TRUE)
	#return: 0.8012142 0.8210082 0.7714031
	#alpha_list should be a dense list of p-value cutoffs; 
	#here we only use a few values to ease computation of the example.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.


\name{lalonde} 

\docType{data} 

\alias{lalonde} 

\title{Lalonde data set} 

\description{
	This is one of the data sets from the National Supported Work Demonstration used by Dehejia and Wahba (1999) to evaluate propensity score matching methods.  This and other related data sets are available at \url{https://users.nber.org/~rdehejia/nswdata2.html}.
} 

\usage{data(lalonde)} 

\format{A data frame with 445 observations, corresponding to 185 treated and 260 control subjects, and 10 variables.  
		The treatment assignment indicator is the first variable of the data frame: \code{treatment} (1 = treated; 0 = control).
		The next 7 columns are the covariates:
		
		\itemize{
			\item \code{age}, measured in years;
			
			\item \code{education}, measured in years;
			
			\item \code{black}, indicating race (1 if black, 0 otherwise);
			
			\item \code{hispanic}, indicating race (1 if Hispanic, 0 otherwise);
			
			\item \code{married}, indicating marital status (1 if married, 0 otherwise);
			
			\item \code{nodegree}, indicating high school diploma (1 if no degree, 0 otherwise);
			
			\item \code{re74}, real earnings in 1974;
			
			\item \code{re75}, real earnings in 1975.
		}	
		
		The last variable of the data frame is \code{re78}, the real the earnings in 1978.
}

\source{\url{https://users.nber.org/~rdehejia/nswdata2.html}} 

\references{
	Dehejia, R., and Wahba, S. (1999), "Causal Effects in Nonexperimental Studies: Reevaluating the Evaluation of Training Programs," \emph{Journal of the American Statistical Association}, 94, 1053-1062. 
	
	Lalonde, R. (1986), "Evaluating the Econometric Evaluations of Training Programs," \emph{American Economic Review}, 76, 604-620.
}

\keyword{datasets}

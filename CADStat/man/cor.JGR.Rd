\name{cor.JGR}
\alias{cor.JGR}
\title{Dialog box for computing a correlation matrix}
\description{
	Computes a correlation matrix and confidence intervals for the chosen
	variables. Possible computational methods are Pearson's r,
	Kendall's tau, and Spearman's rho.
}

\details{
	Correlation coefficients measure the strength of the linear relationship
	between two variables. This module produces a correlation matrix, which
	presents the correlation coefficients for several variables, along with
	confidence intervals.
	
	Correlation coefficients are unitless and range between -1 and 1. A
	positive correlation implies as one variable increases, the other
	increases. A negative correlation implies as one variable increases, the
	other decreases. The closer the correlation is to -1 or 1, the stronger the
	relationship. If the correlation is near 0, then the variables are not
	related \emph{linearly}. In all cases, a scatterplot should be inspected to
	help determine if a linear model is appropriate for the variables.
	
	\strong{Estimation}
	
	This module offers 3 methods for computing a correlation coefficient. One
	is parametric, Pearson's r, and two are nonparametric, Kendall's tau and 
	Spearman's rho.
	
	Pearson's r is equal to the covariance of the two variables	divided by the
	square root of the product of the variances. Since Pearson's r is based
	upon variances and covariances, it is sensitive to outliers. The
	nonparametric methods are based upon data ranks rather than data values so
	they are not sensitive to outliers. Also, Spearman's rho may be a better
	indicator of a non-linear relationship.
	
	
	\strong{Plots}
	
	The user may select to have a scatterplot matrix created. This is a matrix
	of scatterplots between all pairs of variables. These plots are useful in
	determining the type and strength of the relationship between two
	variables.
	
	\strong{User Interface}
	
	To perform a correlation analysis, select
	\emph{Analysis Tools -> Correlation Analysis} from the
	 menu. A dialog box will appear that allows the user to
	enter analysis parameters:
	
	Provide the variables (column names) of interest.
	
	Click the \bold{Method} radio button to indicate the desired method.

	Click the \bold{Create Scatterplot Matrix} check box to indicate
	whether a scatterplot matrix should be produced.
	
	Click the \bold{Compute Confidence Intervals} check box to indicate
	whether confidence intervals for the
	correlation between all 
	variable pairs should be computed.
				
	If confidence intervals are desired, then enter the confidence
	level into the \bold{Confidence Level} text box. The
	confidence level must be between 0 and 1.
		
}


\keyword{univar} 


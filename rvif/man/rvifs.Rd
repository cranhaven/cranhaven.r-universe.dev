\encoding{utf8} % \encoding{latin1}
\name{rvifs}
\alias{rvifs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
RVIF calculation
}
\description{
This function provides the values of the Redefined Variance Inflation Factor (RVIF) and the the percentage of near multicollinearity due to each independent variable.
}
\usage{
rvifs(x, ul = TRUE, intercept = TRUE, tol = 1e-30)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{x}{A numerical design matrix that should contain more than one regressor. If it has an intercept, this must be in the 
          first column of the matrix).}
  \item{ul}{A logical value that indicates if the variables in the design matrix \code{x} are transformed to unit length. 
          By default \code{ul=TRUE}.}
  \item{intercept}{A logical value that indicates if the design matrix \code{x} has an intercept. By default \code{intercept=TRUE}.}
  \item{tol}{Value determining whether the system is computationally singular. By default \code{tol=1e-30}.}
}

\details{
The Redefined Variation Inflation Factor (RVIF) is capable to detect both kind of multicollinearity: the essential 
(approximate linear relationship between at least two independent variables excluding the intercept) and non-essential 
(approximate linear relationship between the intercept and at least one of the remaining independent variables). 
This measure also quantifies the percentage of near multicollinearity due to each independent variable.
}
\value{
  \item{RVIF}{Redefined Variance Inflation Factor of each independent variable.}
  \item{\%}{Percentage of near multicollinearity due to each independent variable.}
}
\references{
R. Salmerón, C. García, and J. García. (2018). Variance inflation factor and condition number in multiple linear
regression. Journal of Statistical Computation and Simulation, 88:2365-2384, doi: https://doi.org/10.1080/00949655.2018.1463376.

Salmerón, R., Rodríguez, A. and García, C.B. (2020). Diagnosis and quantification of 
the non-essential collinearity. Computational Statistics, 35(2), 647-666, doi: https://doi.org/10.1007/s00180-019-00922-x.

Salmerón, R., García, C.B. y García, J. (2025). A redefined Variance Inflation Factor: overcoming the limitations of the Variance Inflation Factor. 
Computational Economics, 65, 337-363, doi: https://doi.org/10.1007/s10614-024-10575-8.
}
\author{R. Salmerón (\email{romansg@ugr.es}) and C. García (\email{cbgarcia@ugr.es}).}
\examples{
### Example 1
	
	library(multiColl)
	set.seed(2025)
	obs = 100
	cte = rep(1, obs)
	x2 = rnorm(obs, 5, 0.01)
	x3 = rnorm(obs, 5, 10)
	x4 = x3 + rnorm(obs, 5, 1)
	x5 = rnorm(obs, -1, 30)
	x = cbind(cte, x2, x3, x4, x5)
	rvifs(x)
	
### Example 2
### The special case of the simple linear regression model
	
	head(SLM1, n=5)
	x = SLM1[,2:3]
	rvifs(x)
	
### Example 3
### The intercept must be in the first column of the design matrix
	
	set.seed(2025)
	obs = 100
	cte = rep(1, obs)
	x2 = sample(1:500, obs)
	x3 = sample(1:500, obs)
	x4 = rep(4, obs)
	x = cbind(cte, x2, x3, x4)
	rvifs(x) # also: perfect multicollinearity between the intercept and the constant variable
	rvifs(x[,-1], intercept = FALSE) # removing the constant from the design matrix
	
### Example 4
### Cases of perfect multicollinearity or computationally singular systems
	
	head(soil, n=5)
	x = soil[,-16]
	rvifs(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{Variation Inflation Factor Redefined}
\keyword{Variation Inflation Factor}
\keyword{Coefficient of Variation}
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

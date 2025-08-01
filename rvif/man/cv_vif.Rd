\name{cv_vif}
\alias{cv_vif}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
VIF and CV calculation
}
\description{
This function provides the values for the Variance Inflation Factor (VIF) and the Coefficient of Variation (CV) for the independent variables (excluding the intercept) in a multiple linear regression model.
}
\usage{
cv_vif(x, tol = 1e-30)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A numerical design matrix containing more than one regressor, including the intercept in the first column.}
  \item{tol}{A real number that indicates the tolerance beyond which the system is considered computationally unique when calculating the VIF.
       The default value is \code{tol=1e-30}.}
}
\details{
It is interesting to note the distinction between essential and non-essential multicollinearity. Essential multicollinearity happens
when there is an approximate linear relationship between two or more independent variables (not including the intercept) while non-essential multicollinearity 
involves a linear relationship between the intercept and at least one independent variable. This distinction matters because the Variance Inflation Factor (VIF) only detects essential multicollinearity, 
while the Condition Value (CV) is useful for detecting only non-essential multicollinearity.
Understanding the distinction between essential and non-essential multicollinearity and the limitations of each detection measure, can be very useful for identifying whether there is a troubling degree of multicollinearity, 
and determining the kind of multicollinearity present and the variables causing it.
}
\value{
  \item{CV}{Coefficient of Variation of each independent variable.}
  \item{VIF}{Variance Inflation Factor of each independent variable.}
}
\references{
Salmerón, R., García, C.B. and García, J. (2018). Variance inflation factor and condition number in multiple linear
regression. Journal of Statistical Computation and Simulation, 88:2365-2384, doi: https://doi.org/10.1080/00949655.2018.1463376.

Salmerón, R., Rodríguez, A. and García, C.B. (2020). Diagnosis and quantification of 
the non-essential collinearity. Computational Statistics, 35(2), 647-666, doi: https://doi.org/10.1007/s00180-019-00922-x.

Salmerón, R., García, C.B., Rodríguez, A. and García, C. (2022). Limitations in detecting multicollinearity due to scaling issues in the 
mcvis package. R Journal, 14(4), 264-279, doi: https://doi.org/10.32614/RJ-2023-010.
}
\author{R. Salmerón (\email{romansg@ugr.es}) and C. García (\email{cbgarcia@ugr.es}).}
\seealso{\code{\link{cv_vif_plot}}}
\examples{
### Example 1 
### At least three independent variables, including the intercept, must be present

	head(SLM1, n=5)
	y = SLM1[,1]
	x = SLM1[,2:3]
	cv_vif(x)

### Example 2
### Creating the design matrix

	library(multiColl)
	set.seed(2025)
	obs = 100
	cte = rep(1, obs)
	x2 = rnorm(obs, 5, 0.01)
	x3 = rnorm(obs, 5, 10)
	x4 = x3 + rnorm(obs, 5, 1)
	x5 = rnorm(obs, -1, 30)
	x = cbind(cte, x2, x3, x4, x5)
	cv_vif(x)

### Example 3 
### Obtaining the design matrix after executing the command 'lm'

	library(multiColl)
	set.seed(2025)
	obs = 100
	cte = rep(1, obs)
	x2 = rnorm(obs, 5, 0.01)
	x3 = rnorm(obs, 5, 10)
	x4 = x3 + rnorm(obs, 5, 1)
	x5 = rnorm(obs, -1, 30)
	u = rnorm(obs, 0, 2)
	y = 5 + 4*x2 - 5*x3 + 2*x4 - x5 + u
	reg = lm(y~x2+x3+x4+x5)
	x = model.matrix(reg)
	cv_vif(x) # identical to Example 2

### Example 3 
### Computationally singular system

	head(soil, n=5)
	y = soil[,16]
	x = soil[,-16]
	cv_vif(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{Variance inflation factor}% use one of  RShowDoc("KEYWORDS")
\keyword{Coefficient of Variation}% __ONLY ONE__ keyword per line
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

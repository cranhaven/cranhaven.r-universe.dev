\name{cv_vif_plot}
\alias{cv_vif_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scatterplot of CV vs VIF
}
\description{
This function provides a graphical representation of a scatter plot showing the Coefficient of Variation (CV) 
and the Variance Inflation Factor (VIF) for the independent variables (excluding the intercept) of a multiple linear regression model.
}
\usage{
cv_vif_plot(x, limit = 40)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{This is the output of the function \code{cv_vif}.}
  \item{limit}{A real number that indicates the lower limit of the vertical axis. The default value is \code{limit=40}.}
}
\details{
The distinction between essential and non-essential multicollinearity and the limitations of each measure (CV and VIF)
  for detecting the different kinds of multicollinearity, can be very useful for identifying whether there is a troubling degree of multicollinearity, 
and determining the kind of multicollinearity present and the variables causing it.
  
For this purpose, it is important to include the lines corresponding to the established thresholds for each
measure in the representation of the scatter plot of the CV and VIF: a dashed vertical line for 0.1002506 (CV) and a dotted horizontal line for 10 (VIF). These
lines determine four regions (see Example 1), which can be interpreted as follows: A: existence of troubling non-essential
and non-troubling essential multicollinearity; B: existence of troubling essential and non-essential multicollinearity; C: existence of non-troubling non-essential and troubling essential multicollinearity;
D: non-troubling degree of existing multicollinearity (essential and non-essential).
}
\references{
Salmerón, R., García, C.B. and García, J. (2018). Variance inflation factor and condition number in multiple linear
regression. Journal of Statistical Computation and Simulation, 88:2365-2384, doi: https://doi.org/10.1080/00949655.2018.1463376.

Salmerón, R., Rodríguez, A. and García, C.B. (2020). Diagnosis and quantification of 
the non-essential collinearity. Computational Statistics, 35(2), 647-666, doi: https://doi.org/10.1007/s00180-019-00922-x.

Salmerón, R., García, C.B., Rodríguez, A. and García, C. (2022). Limitations in detecting multicollinearity due to scaling issues in the 
mcvis package. R Journal, 14(4), 264-279, doi: https://doi.org/10.32614/RJ-2023-010.
}
\author{R. Salmerón (\email{romansg@ugr.es}) and C.B. García (\email{cbgarcia@ugr.es}).}
\seealso{\code{\link{cv_vif}}}
\examples{
### Example 1

	plot(-2:20, -2:20, type = "n", xlab="Coefficient of Variation", 
	                              ylab="Variance Inflation Factor")
	abline(h=10, col="red", lwd=3, lty=2)
	abline(h=0, col="black", lwd=1)
	abline(v=0.1002506, col="red", lwd=3, lty=3)
	#abline(v=0, col="red", lwd=1)
	text(-1.25, 2, "A", pos=3, col="blue")
	text(-1.25, 12, "B", pos=3, col="blue")
	text(10, 12, "C", pos=3, col="blue")
	text(10, 2, "D", pos=3, col="blue")

### Example 2

	library(multiColl)
	set.seed(2025)
	obs = 100
	cte = rep(1, obs)
	x2 = rnorm(obs, 5, 0.01)
	x3 = rnorm(obs, 5, 10)
	x4 = x3 + rnorm(obs, 5, 1)
	x5 = rnorm(obs, -1, 30)
	x = cbind(cte, x2, x3, x4, x5)
	cv_vif_plot(cv_vif(x))
	cv_vif_plot(cv_vif(x), limit=0) # notes the effect of the 'limit' argument

### Example 3
### Graphical representation is not possible
	
	head(SLM2, n=5)
	x = SLM2[,2:3]
	cv_vif_plot(cv_vif(x))
	
### Example 4
### Computationally singular system
	
	head(soil, n=5)
	x = soil[,-16]
	cv_vif_plot(cv_vif(x))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{Variance inflation factor}% use one of  RShowDoc("KEYWORDS")
\keyword{Coefficient of Variation}% __ONLY ONE__ keyword per line
\keyword{Scatter plot}
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

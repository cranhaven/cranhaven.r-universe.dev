\encoding{utf8} % \encoding{latin1}
\name{multicollinearity}
\alias{multicollinearity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Decision Rule to Detect Troubling Multicollinearity
}
\description{
Given a multiple linear regression model with n observations and k independent variables, 
the degree of near-multicollinearity affects its statistical analysis (with a level of significance 
of alpha\%) if there is a variable i, with i = 1,...,k, that verifies that the null hypothesis is not 
rejected in the original model and is rejected in the orthogonal model of reference.
}
\usage{
multicollinearity(y, x, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{A numerical vector representing the dependent variable of the model.}
  \item{x}{A numerical design matrix that should contain more than one regressor (intercept included in the first column).}
  \item{alpha}{Significance level (by default, 5\%).}
}
\details{
This function compares the individual inference of the original model with that of the orthonormal model taken as reference. 

Thus, if the null hypothesis is rejected in the individual significance tests in the model where there are no linear 
relationships between the independent variables (orthonormal) and is not rejected in the original model, the reason 
for the non-rejection is due to the existing linear relationships between the independent variables (multicollinearity) 
in the original model.

The second model is obtained from the first model by performing a QR decomposition, which eliminates the initial 
linear relationships.
}
\value{
The function returns the value of the RVIF and the established thresholds, as well as indicating whether or not the individual
significance analysis is affected by multicollinearity at the chosen significance level.
}
\references{
Salmerón, R., García, C.B. and García, J. (2025). A Redefined Variance Inflation Factor: overcoming the limitations of 
the Variance Inflation Factor. Computational Economics, 65, 337-363, doi: https://doi.org/10.1007/s10614-024-10575-8.

Overcoming the inconsistences of the variance inflation factor: a redefined VIF and a test to detect statistical 
troubling multicollinearity by Salmerón, R., García, C.B and García, J. (working paper, https://arxiv.org/pdf/2005.02245).
}
\author{
  Román Salmerón Gómez (University of Granada) and Catalina B. García García (University of Granada).

  Maintainer: Román Salmerón Gómez (romansg@ugr.es)
}
\seealso{\code{\link{rvifs}}}
\examples{
### Example 1
	
  set.seed(2024)
  obs = 100
  cte = rep(1, obs)
  x2 = rnorm(obs, 5, 0.01)  # related to intercept: non essential
  x3 = rnorm(obs, 5, 10)
  x4 = x3 + rnorm(obs, 5, 0.5) # related to x3: essential
  x5 = rnorm(obs, -1, 3)
  x6 = rnorm(obs, 15, 0.5)
  y = 4 + 5*x2 - 9*x3 -2*x4 + 2*x5 + 7*x6 + rnorm(obs, 0, 2)
  x = cbind(cte, x2, x3, x4, x5, x6)
  multicollinearity(y, x)

### Example 2
### Effect of sample size
  
  obs = 25 # by decreasing the number of observations affected to x4 
  cte = rep(1, obs)
  x2 = rnorm(obs, 5, 0.01)  # related to intercept: non essential
  x3 = rnorm(obs, 5, 10)
  x4 = x3 + rnorm(obs, 5, 0.5) # related to x3: essential
  x5 = rnorm(obs, -1, 3)
  x6 = rnorm(obs, 15, 0.5)
  y = 4 + 5*x2 - 9*x3 -2*x4 + 2*x5 + 7*x6 + rnorm(obs, 0, 2)
  x = cbind(cte, x2, x3, x4, x5, x6)
  multicollinearity(y, x)

### Example 3
  
  y = 4 - 9*x3 - 2*x5 + rnorm(obs, 0, 2)
  x = cbind(cte, x3, x5) # independently generated
  multicollinearity(y, x)
  
### Example 4
### Detection of multicollinearity in Wissel data
  
  head(Wissel, n=5)
  y = Wissel[,2]
  x = Wissel[,3:6]
  multicollinearity(y, x)
  
### Example 5
### Detection of multicollinearity in euribor data
  
  head(euribor, n=5)
  y = euribor[,1]
  x = euribor[,2:5]
  multicollinearity(y, x)
  
### Example 6
### Detection of multicollinearity in Cobb-Douglas production function data

  head(CDpf, n=5)
  y = CDpf[,1]
  x = CDpf[,2:4]  
  multicollinearity(y, x)
  
### Example 7
### Detection of multicollinearity in number of employees of Spanish companies data
  
  head(employees, n=5)
  y = employees[,1]
  x = employees[,3:5]
  multicollinearity(y, x)
  
### Example 8
### Detection of multicollinearity in simple linear model simulated data
  
  head(SLM1, n=5)
  y = SLM1[,1]
  x = SLM1[,2:3]
  multicollinearity(y, x)

  head(SLM2, n=5)
  y = SLM2[,1]
  x = SLM2[,2:3]
  multicollinearity(y, x)
    
### Example 9
### Detection of multicollinearity in soil characteristics data

  head(soil, n=5)
  y = soil[,16]
  x = soil[,-16] 
  x = cbind(rep(1, length(y)), x) # the design matrix has to have the intercept in the first column
  multicollinearity(y, x)
  multicollinearity(y, x[,-3]) # eliminating the problematic variable (SumCation)
  
### Example 10
### The intercept must be in the first column of the design matrix
  
  set.seed(2025)
  obs = 100
  cte = rep(1, obs)
  x2 = sample(1:500, obs)
  x3 = sample(1:500, obs)
  x4 = rep(4, obs)
  x = cbind(cte, x2, x3, x4)
  u = rnorm(obs, 0, 2)
  y = 5 + 2*x2 - 3*x3 + 10*x4 + u
  multicollinearity(y, x)
  multicollinearity(y, x[,-4]) # the constant variable is removed
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{Variance inflation factor redefined}% use one of  RShowDoc("KEYWORDS")
\keyword{Individual significance tests}% __ONLY ONE__ keyword per line
\keyword{Orthonormal model}
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

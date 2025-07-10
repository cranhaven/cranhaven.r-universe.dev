\name{residuals.simplexreg}
\alias{residuals.simplexreg}
\title{Extract residuals for simplexreg Objects}
\description{Methods for extracting various types of residuals from simplex regression, from 
approximate Pearson residuals, standard Pearson residuals and standardise score residuals to 
adjusted dependent variable suggested by McCullagh and Nelder (1989). The first three can be 
used to examine mean-variance relation while the last aims to test the link function.}

\usage{
\method{residuals}{simplexreg}(object, type = c("appstdPerr", "stdPerr", "stdscor", "adjvar"), 
   	...)
}

\arguments{
	\item{object}{fitted model object of class "simplexreg"}
	\item{type}{character specifying types of residuals:approximate Pearson residual (\code{appstdPerr}),
	standard Pearson residual (\code{stdPerr}), adjusted dependent variable \eqn{s_i} (\code{adjvar}).
	Details are given in 'Details'}
	\item{...}{currently not used}
}

\details{The Pearson residual takes the form
\deqn{r_i^P=\frac{y_i-\hat{\mu}_i}{\hat{\tau}_i}}{%
	r_i^P=(y_i-\mu_i)/(\tau_i)}
where \eqn{\hat{\mu}_i}{\mu_i} is the fitted mean parameter and details about calculation of
\eqn{\tau} is given in Jorgensen (1997). When the dispersion parameter \eqn{\sigma^2}
(see \code{\link{simplex}}) is large the variance of response approaches to \eqn{\mu(1-\mu)}
and this leads to the approximate Pearson residual
\deqn{r_i^a=\frac{y_i-\hat{\mu}_i}{\sqrt{\hat{\mu}_i(1-\hat{\mu}_i)}}}{%
	r_i^P=(y_i-\mu_i)/\sqrt{(\mu_i(1-\mu_i))}}
Plot of the standardised score residuals,
\deqn{r_i^S=\frac{u_i}{\sqrt{var(u_i)}}}{%
	r_i^S=u_i/ \sqrt{var(u_i)}}
where \eqn{u_i} is the score function, can also detect model assumption violation. 
Details can be found in Song \emph{et al.} (2004). 	
The adjusted dependent variable suggested by McCullagh and Nelder (1989) could be employed
as an informal check for the link function,
\deqn{s_i = g(\mu_i) + (\frac{3\sigma^4}{\mu_i(1-\mu_i)}+\frac{\sigma^2}{V(\mu_i)})^{-1/2} u(y_i;\mu_i)}{%
s_i = g(\mu_i) + {3\sigma^4 / (\mu_i(1-\mu_i)) + \sqrt{\sigma^2 / V(\mu_i)}} u(y_i;\mu_i)}
where \eqn{u(y_i;\mu_i)} and \eqn{V(\mu_i)} are the score function and variance function. 
}

\references{Barndorff-Nielsen,  O.E. and  Jorgensen, B. (1991)
     Some parametric models on the simplex.
     \emph{ Journal of  Multivariate Analysis,} \bold{ 39:} 106--116
     
     Jorgensen, B. (1997)
   \emph{ The Theory of Dispersion Models}. London: Chapman and Hall
   
   McCullagh, P and Nelder J. (1989)
   \emph{ Generalized Linear Models}. London: Chapman and Hall
   
    Song, P. and Qiu, Z. and Tan, M. (2004) Modelling Heterogeneous Dispersion in 
   Marginal Models for Longitudinal Proportional Data. \emph{ Biometrical Journal,} 
   \bold{ 46:} 540--553
   
   Zhang, P. and Qiu, Z. and Shi, C. (2016) {simplexreg}: An {R} Package for Regression
	Analysis of Proportional Data Using the Simplex Distribution. \emph{ Journal of Statistical Software,}
	\bold{ 71:} 1--21
%% ~put references to the literature/web site here ~
}

\author{Chengchun Shi
}

\seealso{\code{\link{summary.simplexreg}}, \code{\link{simplex}}}

\examples{
## fit the model
data("sdac", package="simplexreg")
sim.glm2 <- simplexreg(rcd~ageadj+chemo|age,
  link = "logit", data = sdac)
	
data("retinal", package = "simplexreg")
sim.gee2 <- simplexreg(Gas~LogT+LogT2+Level|LogT+Level|Time,
  link = "logit", corr = "AR1", id = ID, data = retinal)  

## extract the residuals
res <- residuals(sim.glm2, type = "stdPerr")
res <- residuals(sim.gee2, type = "adjvar")
}
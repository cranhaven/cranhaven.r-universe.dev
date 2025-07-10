\name{plot.simplexreg}
\alias{plot.simplexreg}
\title{Plots for simplexreg Objects}
\description{
  Various types of plots could be produced for simplexreg Objects, including plots of correlation 
  structure, plots of different types of residuals and plots of partial deviance. 
}

\usage{
\method{plot}{simplexreg}(x, type = c("residuals", "corr", "GOF"), res = "adjvar", lag = 1, ...)
}

\arguments{
	\item{x}{fitted model object of class "simplexreg"}
	\item{type}{character specifying types of plots: the correlation (\code{corr}),
	residuals (\code{residuals}), partial deviances (\code{GOF}). See 'Details'}
	\item{res}{character specifying types of residuals:approximate Pearson residual (\code{appstdPerr}),
	standard Pearson residual (\code{stdPerr}), adjusted dependent variable \eqn{s_i} (\code{adjvar}).
	See \code{\link{residuals.simplexreg}}}
	\item{lag}{when \code{type = corr}, this function examine the autocorrelation at lag \code{lag}}
	\item{...}{other parameters to be passed through to the plot function}
}

\details{
This function provides graphical presentations for simplexreg objects. The plot of correlation aims
examine the correlation structure of the longitudinal data set. Let \eqn{r_{ij}} be the standardised 
score residuals of the \code{i}th observation at time \eqn{t_{ij}}, and \code{lag = k}, then \eqn{r_{ij}} 
are plotted against \eqn{r_{ik}} for all \eqn{i} and \eqn{j < k}, if \eqn{|t_{ij} - t_{ik}| = k}.

Residuals can be plotted when specifying \code{type = "residuals"}, The upper and lower 95% confidence limits 
(1.96) are also lined.

Plots of partial deviance are for the goodness-of-fit test in the presence of within-subject dependence for 
longitudinal data. The partial deviances are defined as
\deqn{D_j^P=\sum_{i=1}^{m_j}d(y_{ij}-\hat{\mu}_{ij}) / \sigma_{ij}^2, j \in T}{%
	D_j^P=sum d(y_{ij}-mu_{ij}) / \sigma_{ij}^2, j in T}
where T denotes a collection of all distinct times on which observation are made. Cross-sectionally, 
\eqn{y_{ij}}'s are independent and hence \eqn{D_j^P} follows approximately \eqn{\chi^2}, with \eqn{m_j} 
being the total number of \eqn{y_{ij}}'s observed cross-sectionally at time \eqn{t_j}. Both observed partial 
deviance \eqn{D_j^P} statistics and the corresponding critical values are depicted and compared at each 
time point.
}

\references{
    Song, P. and Qiu, Z. and Tan, M. (2004) Modelling Heterogeneous Dispersion in 
   Marginal Models for Longitudinal Proportional Data. \emph{ Biometrical Journal,} 
   \bold{ 46:} 540--553
   
   Qiu Z. (2001) \emph{ Simplex Mixed Models for Longitudinal Proportional Data.} 
   Ph.D. Dissertation, York University
   
   Zhang, P. and Qiu, Z. and Shi, C. (2016) {simplexreg}: An {R} Package for Regression
	Analysis of Proportional Data Using the Simplex Distribution. \emph{ Journal of Statistical Software,}
	\bold{ 71:} 1--21
%% ~put references to the literature/web site here ~
}

\author{Chengchun Shi
}

\seealso{\code{\link{summary.simplexreg}}, \code{\link{residuals.simplexreg}}}

\examples{
## fit the model
data("sdac", package="simplexreg")
sim.glm2 <- simplexreg(rcd~ageadj+chemo|age, 
  link = "logit", data = sdac)
	
data("retinal", package = "simplexreg")
sim.gee2 <- simplexreg(Gas~LogT+LogT2+Level|LogT+Level|Time, 
  link = "logit", corr = "AR1", id = ID, data = retinal)  

## produce the plots
plot(sim.glm2, type = "residuals", res = "stdPerr", ylim = c(-3, 3))
plot(sim.gee2, type = "corr", xlab = "", ylab = "")
plot(sim.gee2, type = "GOF", xlab = "", ylab = "")
}
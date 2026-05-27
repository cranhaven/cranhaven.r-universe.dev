\name{g2l.proc}
\alias{g2l.proc}
\alias{g2l.infer}
\alias{g2l.infer.boot}
\alias{fdr.thresh}
\alias{get_bh_threshold}
\alias{getNullProb}

\title{
Procedures for global and local inference.
}
\description{
This function performs customized fdr analyses tailored to each individual cases.
}
\usage{
g2l.proc(X, z, X.target = NULL, z.target = NULL, m = c(4, 6), alpha = 0.1,
	nbag = NULL, nsample = length(z), lp.reg.method = "lm",
	null.scale = "QQ", approx.method = "direct", ngrid = 2000,
	centering = TRUE, coef.smooth = "BIC", fdr.method = "locfdr",
	plot = TRUE, rel.null = "custom", locfdr.df = 10,
	fdr.th.fixed = NULL, parallel = FALSE, ...)
}
\arguments{
  \item{X}{A \eqn{n}-by-\eqn{d} matrix of covariate values}
  \item{z}{A length \eqn{n} vector containing observations of z values.}
  \item{X.target}{A \eqn{k}-by-\eqn{d} matrix providing \eqn{k} sets of covariates for target cases to investigate. Set to NULL to investigate all cases and provide global inference results.}
  \item{z.target}{A vector of length \eqn{k}, providing the target \eqn{z} values to investigate}
  \item{m}{An ordered pair. First number indicates how many LP-nonparametric basis to construct for each \eqn{X}, second number indicates how many to construct for \eqn{z}. Default: \code{m=c(4,6)}.}
  \item{alpha}{Confidence level for determining signals.}
  \item{nbag}{Number of bags of parametric bootstrapped samples to use for each target case, each time a new set of relevance samples will be generated for analysis, and the resulting fdr curves are aggregated together by taking the mean values. Set to \code{NULL} to disable.}
  \item{nsample}{Number of relevance samples generated for each case. The default is the size of the input z-statistic.}
  \item{lp.reg.method}{Method for estimating the relevance function and its conditional LP-Fourier coefficients. We currently support three options: lm (inbuilt with subset selection), glmnet, and knn.}
  \item{null.scale}{Method of estimating null standard deviation from the laser samples. Available options: "IQR", "QQ" and "locfdr"}
  \item{approx.method}{Method used to approximate customized fdr curve, default is "direct".When set to "indirect", the customized fdr is computed by modifying pooled fdr using relevant density function.}
  \item{ngrid}{Number of gridpoints to use for computing customized fdr curve.}
  \item{centering}{Whether to perform regression-adjustment to center the data, default is TRUE.}
  \item{coef.smooth}{Specifies the method to use for LP coefficient smoothing (AIC or BIC). Uses BIC by default.}
  \item{fdr.method}{Method for controlling false discoveries (either "locfdr" or "BH"), default choice is "locfdr".}
  \item{plot}{Whether to include plots in the results, default is \code{TRUE}.}
  \item{rel.null}{How the relevant null changes with x: "custom" denotes we allow it to vary with x, and "th" denotes fixed.}
  \item{locfdr.df}{Degrees of freedom to use for \code{locfdr()}}
  \item{fdr.th.fixed}{Use fixed fdr threshold for finding signals. Default set to \code{NULL}, which finds different thresholds for different cases. }
  \item{parallel}{Use parallel computing for obtaining the relevance samples, mainly used for very huge \code{nsample}, default is FALSE.}
  \item{...}{Extra parameters to pass to other functions. Currently only supports the arguments for \code{knn()}.}
}

\value{
A list containing the following items:
	\item{macro}{Available when \code{X.target} set to \code{NULL}, contains the following items:}
		\item{$result}{A list of global inference results:}
		\item{$X}{Matrix of covariates, same as input \code{X}.}
		\item{$z}{Vector of observations, same as input \code{z}.}
		\item{$probnull}{A vector of length \eqn{n}, indicating how likely the observed z belongs to local null.}
		\item{$signal}{A binary vector of length \eqn{n}, discoveries are indicated by \eqn{1}.}

	\item{plots}{A list of plots for global inference:}
		\item{$signal_x}{A plot of signals discovered, marked in red}
		\item{$dps_xz}{A scatterplot of z on x, colored based on the discovery propensity scores, only available when \code{fdr.method = "locfdr"}.}
		\item{$dps_x}{A scatterplot of discovery propensity scores on x, only available when \code{fdr.method = "locfdr"}.}

	\item{micro}{Available when \code{X.target} are provided with values, contains the following items:}
		\item{$result}{Customized estimates for null probabilities for target \eqn{X} and \eqn{z}}
		\item{$result$signal}{A binary vector of length \eqn{k}, discoveries in the target cases are indicated by \eqn{1}}
		\item{$global}{Pooled global estimates for null probabilities for target \eqn{X} and \eqn{z}}
		\item{$plots}{Customized fdr plots for the target cases.}

	\item{\code{m.lp}}{Same as input \code{m}}
}

\references{
Mukhopadhyay, S., and Wang, K (2021) "On The Problem of Relevance in Statistical Inference". <arXiv:2004.09588>
}
\author{
\packageAuthor{LPRelevance}

Maintainer: \packageMaintainer{LPRelevance}
}

\examples{
\donttest{
data(funnel)
X<-funnel$x
z<-funnel$z
##macro-inference using locfdr and LASER:
g2l_macro<-g2l.proc(X,z)
g2l_macro$macro$plots

#Microinference for the DTI data: case A with x=(18,55) and z=3.95
data(data.dti)
X<- cbind(data.dti$coordx,data.dti$coordy)
z<-data.dti$z
g2l_x<-g2l.proc(X,z,X.target=c(18,55),z.target=3.95,nsample =3000)
g2l_x$micro$plots$fdr.1+ggplot2::coord_cartesian(xlim=c(0,4))
g2l_x$micro$result[4]
}
}

\keyword{ Main Functions }

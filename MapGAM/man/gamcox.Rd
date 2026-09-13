\name{gamcox}
\alias{gamcox}
\alias{gamcox.fit}
\title{
Fit a Cox Additive Model with a Two-Dimensional Smooth}
\description{
Fits a Cox proportional hazard additive model with a two-dimensional LOESS smooth for geolocation (or any other two-dimensional predictor). \code{gamcox} uses the backfitting algorithm to combine the smoothing and fitting methods. The smoothing method currently supported is local regression (LOESS).
}
\usage{
gamcox(formula, data, subset, weights, span=0.5, I.span=0.2, degree = 1, 
       loess.trace = "exact", Maxiter = 40, tol = 1e-07)

gamcox.fit(Y, X, smooth.frame, weights, span=0.5, I.span=0.2, degree = 1, 
           loess.trace = "exact", Maxiter = 40, tol = 1e-07)
}
\arguments{
  \item{formula}{
a formula expression (required), with the response on the left of a ~ oprator, and the predictors on the right. The response must be a survival object as returned by the Surv function. A built-in nonparametric smoothing term is indicated by \code{lo} for loess smooth terms. The two-dimensional predictor (e.g.,geolocation) should be specified as auguments in \code{lo()}.
}
  \item{data}{
a data frame containing the variables in the model.If not found in \code{data}, the variables are taken from \code{environment(formula)}.
}
  \item{Y, X}{
for \code{gamcox.fit}: \code{Y} is a list including two elements: \code{time} for survival times and \code{event} for censoring status. \code{X} is a data frame containing the variables in the model. The data must be structured so that the X and Y coordinates for two-dimensional predictor (e.g., geolocation) are in the 1st and 2nd columns, respectively. 
}
  \item{smooth.frame}{
the model matrix for the smooth term.  
}
  \item{subset}{
an optional vector specifying a subset of observations to be used in the fitting process.
}
  \item{weights}{
an optional vector of weights to be used in the fitting process.
}
  \item{span}{
the span size for the LOESS smooth of the two-dimensional predictor, which controls the degree of smoothing. 
}
  \item{I.span}{
the span size for the LOESS smooth of the Fisher information. 
}
  \item{degree}{
the degree of the polynomials to be used for LOESS smooth, normally 1 or 2.
}
  \item{loess.trace}{
whether the trace of the smoother matrix be computed exactly (\code{"exact"}) or approximately (\code{"approximate"}). It is recommended to use \code{"approximate"} for more than about 1000 data points.
}
  \item{Maxiter}{
the maximum number of iterations in backfitting algorithm.  
}
  \item{tol}{
the tolerence threshold for convergence.  
}
}

\details{
The model used to fit the data is a Cox proportional hazard additive model with a LOESS smooth for a two-dimensional predictor such as geolocation (Hastie and Tibshirani, 1990):
\deqn{\boldsymbol{\lambda}(t)=\boldsymbol{\lambda}_{0}(t)\exp\left\{ S(x_{i},y_{i}) + \mathbf{Z_{i}} \boldsymbol{\beta}\right\}}{
	\lambda(t)= \lambda0(t)exp\{ S(x,y) + Z \beta \}}	
where \eqn{\boldsymbol{\lambda}(t)}{\lambda(t)} is the hazard at time \emph{t} for participant \emph{i}, \eqn{x_{i}}{x} and \eqn{y_{i}}{y} are predictor coordinates for participant \emph{i} (i.e., projected distance east and north, respectively, from an arbitrarily defined origin location), \emph{S(.,.)} is a 2-dimensional smoothing function (currently LOESS), \eqn{\mathbf{Z_{i}}}{Z} is a row vector of covariate values for participant \emph{i}, and \eqn{\boldsymbol{\beta}}{\beta} is a vector of unknown regression parameters. See the references for more details.              
}

\value{

\code{gamcox} returns an object of class \code{gamcox}. It can be examined by \code{print} and \code{predict}.


\item{coefficients}{a named vector of coefficients for the parametric part of the additive predictors, which multiply the columns of the model matrix. The names of the coefficients are the names of the single-degree-of-freedom effects (the columns of the model matrix). If the model is overdetermined there will be missing values in the coefficients corresponding to inestimable coefficients.}

\item{additive.predictors}{the additive fit, given by the product of the model matrix and the coefficients, plus the columns of the \code{$smooth} component.}

\item{smooth}{
estimated smoothing term. Nonlinear part of the spatial effect on survival rates.
}
\item{var}{
the approximate pointwise variances for the columns of smooth.}

\item{residuals}{the residuals from the final weighted additive fit; also known as residuals, these are typically not interpretable without rescaling by the weights.}
\item{deviance}{up to a constant, minus twice the maximized log-likelihood. Similar to the residual sum of squares. Where sensible, the constant is chosen so that a saturated model has deviance zero.}
\item{df.residual}{the residual degrees of freedom.}
\item{aic}{AIC of the fitted model.}
}

\references{
Hastie TJ, Tibshirani RJ.  Generalized Additive Models.  (Chapman & Hall/CRC Monographs on Statistics & Applied Probability, Boca Raton, Florida, 1990). 

Bristow RE, Chang J, Ziogas A, Gillen DL, Bai L, Vieira VM. Spatial Analysis of Advanced-stage Ovarian Cancer Mortality in California. American Journal of Obstetrics and Gynecology 2015, 213(1), e1-43)
}

\author{
Lu Bai

Send bug reports to \email{sbartell@uci.edu}.
}
 

\seealso{
\code{\link{modgam}}, 
\code{\link{predict.gamcox}}.
}
\examples{
\donttest{
data(CAdata)
data(CAmap)

fit <- gamcox(Surv(time,event)~AGE + factor(INS) + lo(X,Y),data=CAdata,
span=0.2,loess.trace="approximate")
fit
pred = predict(fit)
colormap(list(fit=pred$pred,grid=data.frame(X=CAdata$X,Y=CAdata$Y)),map=CAmap,
border.gray=0.5)

}
}

\keyword{misc}
\keyword{smooth}

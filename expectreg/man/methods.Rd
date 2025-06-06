\name{methods}
\Rdversion{1.1}
\alias{print.expectreg}
\alias{predict.expectreg}
\alias{residuals.expectreg}
\alias{resid.expectreg}
\alias{coef.expectreg}
\alias{coefficients.expectreg}
\alias{fitted.expectreg}
\alias{fitted.values.expectreg}
\alias{effects.expectreg}
\alias{[.expectreg}
\alias{summary.expectreg}
\alias{confint.expectreg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Methods for expectile regression objects
}
\description{
Methods for objects returned by expectile regression functions.
}
\usage{
%\method{plot}{expectreg}(x,rug = TRUE, xlab = NULL, ylab = NULL, ylim = NULL, legend = TRUE, ci = FALSE, ...)

\method{print}{expectreg}(x, ...)

\method{summary}{expectreg}(object,...)

\method{predict}{expectreg}(object, newdata = NULL, with_intercept = T, ...)

\method{[}{expectreg}(x, i)

\method{residuals}{expectreg}(object, ...)
\method{resid}{expectreg}(object, ...)

\method{fitted}{expectreg}(object, ...)
\method{fitted.values}{expectreg}(object, ...)

\method{effects}{expectreg}(object, ...)

\method{coef}{expectreg}(object, ...)
\method{coefficients}{expectreg}(object, ...)

\method{confint}{expectreg}(object, parm = NULL, level = 0.95, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x,object}{
An object of class \code{expectreg} as returned e.g. by the function \code{\link{expectreg.ls}}.
}
%  \item{rug}{ Boolean. Whether nonlinear effects are displayed in a rug plot. }
%  \item{xlab,ylab,ylim}{ Graphic parameters. \code{xlab} should match the number of covariates. }
%  \item{legend}{ Boolean. Decides whether a legend is added to the plots. }
%  \item{ci}{ Boolean. Whether confidence intervals and significances should be plotted.}
  \item{newdata}{ 
Optionally, a data frame in which to look for variables with which to predict.
}
  \item{with_intercept}{
Should the intercept be added to the prediction of splines?
}
 \item{i}{
Covariate numbers to be kept in subset.
}
  \item{level}{
Coverage probability of the generated confidence intervals.
}
  \item{parm}{
Optionally the confidence intervals may be restricted to certain covariates, to be named in a vector. Otherwise the confidence intervals for the fit are returned.  
}
  \item{\dots}{ additional arguments passed over. }
}
\details{
These functions can be used to extract details from fitted models.
  \code{print} shows a dense representation of the model fit.

%The \code{plot} function gives a visual representation of the fitted expectiles separately for each covariate.

\code{[} can be used to define a new object with a subset of covariates from the original fit.

The function \code{coef} extracts the regression coefficients for each covariate listed separately.
For the function \code{\link{expectreg.boost}} this is not possible.
}
\value{
\code{[} returns a new object of class expectreg with a subset of covariates from the original fit.

\code{resid} returns the residuals in order of the response.

\code{fitted} returns the overall fitted values \eqn{\hat{y}} while \code{effects} returns the values
for each covariate in a list.

\code{coef} returns a list of all regression coefficients separately for each covariate.
}

\references{
Schnabel S and Eilers P (2009)
\emph{ Optimal expectile smoothing }
Computational Statistics and Data Analysis, 53:4168-4177

Sobotka F and Kneib T (2010)
\emph{ Geoadditive Expectile Regression }
Computational Statistics and Data Analysis,
doi: 10.1016/j.csda.2010.11.015.

}
\author{
Fabian Otto- Sobotka \cr
Carl von Ossietzky University Oldenburg \cr
\url{https://uol.de} \cr

Elmar Spiegel \cr
Georg August University Goettingen
\url{https://www.uni-goettingen.de} \cr

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{expectreg.ls}}, \code{\link{expectreg.boost}}, \code{\link{expectreg.qp}}
}
\examples{
data(dutchboys)

expreg <- expectreg.ls(hgt ~ rb(age,"pspline"),data=dutchboys,smooth="f",
                       expectiles=c(0.05,0.2,0.8,0.95))

print(expreg)

coef(expreg)

new.d = dutchboys[1:10,]
new.d[,2] = 1:10

predict(expreg,newdata=new.d)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ methods }

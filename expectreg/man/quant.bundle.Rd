\name{quant.bundle}
\Rdversion{1.1}
\alias{quant.bundle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Restricted expectile regression of additive models
}
\description{
A location-scale model to fit generalized additive models with least asymmetrically weighted squares
to obtain the graphs of different expectiles or quantiles
for continuous, spatial or random effects.
}
\usage{
quant.bundle(formula, data = NULL, smooth = c("schall", "acv", "fixed"), 
             lambda = 1, quantiles = NA, simple = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
An R formula object consisting of the response variable, '~'
and the sum of all effects that should be taken into consideration.
Each effect has to be given through the function \code{\link{rb}}.
}
  \item{data}{
Optional data frame containing the variables used in the model, if the data is not explicitely given in the
formula.  
}
  \item{smooth}{
There are different smoothing algorithms that should prevent overfitting.
The 'schall' algorithm iterates the smoothing penalty \code{lambda} until it converges,
the asymmetric cross-validation 'acv' minimizes a score-function using \code{\link[stats]{nlm}}
or the function uses a fixed penalty.
}
  \item{lambda}{
The fixed penalty can be adjusted. Also serves as starting value for
the smoothing algorithms.
}
  \item{quantiles}{
In default setting, the quantiles (0.01,0.02,0.05,0.1,0.2,0.5,0.8,0.9,0.95,0.98,0.99) are calculated.
You may specify your own set of expectiles in a vector. 
}
  \item{simple}{
A binary variable depicting if the restricted expectiles (\code{TRUE}) or the bundle is used as basis for the quantile bundle.
}
}
\details{
In least asymmetrically weighted squares (LAWS) each expectile is fitted by minimizing:

\eqn{ S = \sum_{i=1}^{n}{ w_i(p)(y_i - \mu_i(p))^2} }

with

\eqn{ w_i(p) = p 1_{(y_i > \mu_i(p))} + (1-p) 1_{(y_i < \mu_i(p))} }.

The restricted version fits the 0.5 expectile at first and then the residuals.
Afterwards the other expectiles are fitted as deviation by a factor of the residuals from the mean expectile.
This algorithm is based on He(1997). The advantage is that expectile crossing cannot occur,
the disadvantage is a suboptimal fit in certain heteroscedastic settings.
Also, since the number of fits is significantly decreased, the restricted version is much faster.

The expectile bundle has a resemblence to the restricted regression. At first, a trend curve is fitted
and then an iteration is performed between fitting the residuals and calculating the deviation factors
for all the expectiles until the results are stable. Therefore this function shares the (dis)advantages
of the restricted.

The quantile bundle uses either the restricted expectiles or the bundle to estimate a dense set of expectiles. Next 
this set is used to estimate a density with the function \code{\link{cdf.bundle}}. From this density quantiles 
are determined and inserted to the calculated bundle model. This results in an estimated location-scale model for 
quantile regression. 
} 

\value{ An object of class 'expectreg', which is basically a list consisting of: 
    \item{lambda }{The final smoothing parameters for all expectiles and for all effects in a list.
                   For the restricted and the bundle regression there are only the mean and the residual lambda.}
    \item{intercepts }{The intercept for each expectile.}
    \item{coefficients}{ A matrix of all the coefficients, for each base element
                         a row and for each expectile a column. }
    \item{values}{ The fitted values for each observation and all expectiles,
                   separately in a list for each effect in the model,
                   sorted in order of ascending covariate values. }
    \item{response}{ Vector of the response variable. }
    \item{covariates}{ List with the values of the covariates. }
    \item{formula}{ The formula object that was given to the function. }
    \item{asymmetries}{ Vector of fitted expectile asymmetries as given by argument \code{expectiles}. }
    \item{effects}{ List of characters giving the types of covariates. }
    \item{helper}{ List of additional parameters like neighbourhood structure for spatial effects or 'phi' for kriging. }
    \item{trend.coef}{ Coefficients of the trend function. }
    \item{residual.coef}{ Vector of the coefficients the residual curve was fitted with. }
    \item{asymmetry}{ Vector of the asymmetry factors for all expectiles. }
    \item{design}{ Complete design matrix. }
    \item{fitted}{ Fitted values \eqn{ \hat{y} }. }
\code{\link[=plot.expectreg]{plot}}, \code{\link[=predict.expectreg]{predict}}, \code{\link[=resid.expectreg]{resid}}, \code{\link[=fitted.expectreg]{fitted}} and \code{\link[=effects.expectreg]{effects}}
methods are available for class 'expectreg'.
}
\references{
Schnabel S and Eilers P (2009)
\emph{ Optimal expectile smoothing }
Computational Statistics and Data Analysis, 53:4168-4177

He X (1997)
\emph{ Quantile Curves without Crossing }
The American Statistician, 51(2):186-192

Schnabel S and Eilers P (2011)
\emph{ A location scale model for non-crossing expectile curves}
(working paper)

Sobotka F and Kneib T (2010)
\emph{ Geoadditive Expectile Regression }
Computational Statistics and Data Analysis,
doi: 10.1016/j.csda.2010.11.015.
}
\author{
Fabian Otto- Sobotka \cr
Carl von Ossietzky University Oldenburg \cr
\url{https://uol.de} \cr

Thomas Kneib \cr
Georg August University Goettingen \cr
\url{https://www.uni-goettingen.de} \cr

Sabine Schnabel \cr
Wageningen University and Research Centre \cr
\url{https://www.wur.nl}

Paul Eilers \cr
Erasmus Medical Center Rotterdam \cr
\url{https://www.erasmusmc.nl}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rb}}, \code{\link{expectreg.boost}}
}
\examples{
qb = quant.bundle(dist ~ rb(speed),data=cars,smooth="f",lambda=5)
plot(qb)

qbund <- quant.bundle(dist ~ rb(speed),data=cars,smooth="f",lambda=50000,simple=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ nonparametric }
\keyword{ smooth }% __ONLY ONE__ keyword per line
\keyword{ multivariate }
\keyword{ regression }
\keyword{ nonlinear }
\keyword{ models }

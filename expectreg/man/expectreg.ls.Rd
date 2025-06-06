\name{expectreg.ls}
\Rdversion{1.1}
\alias{expectreg.ls}
\alias{expectreg.qp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Expectile regression of additive models
}
\description{
Additive models are fitted with least asymmetrically weighted squares
or quadratic programming to obtain expectiles
for parametric, continuous, spatial and random effects.
}
\usage{
expectreg.ls(formula, data = NULL, estimate = c("laws", "restricted", "bundle", "sheets"),
smooth = c("schall", "ocv", "gcv", "cvgrid", "aic", "bic", "lcurve", "fixed"), 
lambda = 1, expectiles = NA, ci = FALSE, LAWSmaxCores = 1, ...)

expectreg.qp(formula, data  = NULL, id = NA, smooth = c("schall", "acv", "fixed"), 
             lambda = 1, expectiles = NA) 
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
  \item{id}{
Potential additional variable identifying individuals in a longitudinal data set. Allows for a random intercept estimation.
  }
  \item{estimate}{
Character string defining the estimation method that is used to fit the expectiles. Further detail on all available methods is given below.
}
  \item{smooth}{
There are different smoothing algorithms that should prevent overfitting.
The 'schall' algorithm iterates the smoothing penalty \code{lambda} until it converges (REML).
The generalised cross-validation 'gcv',similar to the ordinary cross- validation 'ocv' minimizes a score-function using \code{\link[stats]{nlminb}} or with a grid search by 'cvgrid' or the function uses a fixed penalty. The numerical minimisatioin is also possible with AIC or BIC as score. The L-curve is a new experimental grid search by Frasso and Eilers.
}
  \item{lambda}{
The fixed penalty can be adjusted. Also serves as starting value for
the smoothing algorithms.
}
  \item{expectiles}{
In default setting, the expectiles (0.01,0.02,0.05,0.1,0.2,0.5,0.8,0.9,0.95,0.98,0.99) are calculated.
You may specify your own set of expectiles in a vector. The option may be set to 'density' for the calculation
of a dense set of expectiles that enhances the use of \code{\link{cdf.qp}} and \code{\link{cdf.bundle}} afterwards.
}
  \item{ci}{
Whether a covariance matrix for confidence intervals and a \code{\link[=summary.expectreg]{summary}} is calculated.
}
 \item{LAWSmaxCores}{
 How many cores should maximal be used by parallelization
}
 \item{...}{
Optional value for re-weight the model with estimate weights and combine selected models to one model. 
}
}
\details{
In least asymmetrically weighted squares (LAWS) each expectile is fitted independently from the others.
LAWS minimizes:

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

The expectile sheets construct a p-spline basis for the expectiles and perform a continuous fit over all expectiles
by fitting the tensor product of the expectile spline basis and the basis of the covariates.
In consequence there will be most likely no crossing of expectiles but also a good fit in heteroscedastic scenarios.

The function \code{expectreg.qp} also fits a sheet over all expectiles, but it uses quadratic programming with constraints,
so crossing of expectiles will definitely not happen. So far the function is implemented for one nonlinear or spatial covariate
and further parametric covariates. It works with all smoothing methods.
}
\value{
An object of class 'expectreg', which is basically a list consisting of:
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
    \item{design}{ Complete design matrix. }
    \item{bases}{ Bases components of each covariate. }
    \item{fitted}{ Fitted values \eqn{ \hat{y} }. }
   \item{covmat}{ Covariance matrix, estimated when \code{ci = TRUE}. }
   \item{diag.hatma}{ Diagonal of the hat matrix. Used for model selection criteria. }
   \item{data}{ Original data }
   \item{smooth_orig}{ Unchanged original type of smoothing. }
   %\item{delta_garrote}{ Values of extra weights used for non-negative garrote }
\code{\link[=plot.expectreg]{plot}}, \code{\link[=predict.expectreg]{predict}}, \code{\link[=resid.expectreg]{resid}},
\code{\link[=fitted.expectreg]{fitted}}, \code{\link[=effects.expectreg]{effects}}
and further convenient methods are available for class 'expectreg'.
}
\references{
Schnabel S and Eilers P (2009)
\emph{ Optimal expectile smoothing }
Computational Statistics and Data Analysis, 53:4168-4177

Sobotka F and Kneib T (2010)
\emph{ Geoadditive Expectile Regression }
Computational Statistics and Data Analysis,
doi: 10.1016/j.csda.2010.11.015.

Schnabel S and Eilers P (2011)
\emph{ Expectile sheets for joint estimation of expectile curves }
(under review at Statistical Modelling)

Frasso G and Eilers P (2013)
\emph{ Smoothing parameter selection using the L-curve}
(under review)
}
\author{
Fabian Otto-Sobotka \cr
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

Linda Schulze Waltrup, Goeran Kauermann \cr
Ludwig Maximilians University Muenchen \cr
\url{https://www.lmu.de} \cr
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rb}}, \code{\link{expectreg.boost}}
}
\examples{
library(expectreg)
ex = expectreg.ls(dist ~ rb(speed),data=cars,smooth="b",lambda=5,expectiles=c(0.01,0.2,0.8,0.99))
ex = expectreg.ls(dist ~ rb(speed),data=cars,smooth="f",lambda=5,estimate="restricted")
plot(ex)


data("lidar", package = "SemiPar")

explaws <- expectreg.ls(logratio~rb(range,"pspline"),data=lidar,smooth="gcv",
                        expectiles=c(0.05,0.5,0.95))
print(explaws)
plot(explaws)

###expectile regression using a fixed penalty
plot(expectreg.ls(logratio~rb(range,"pspline"),data=lidar,smooth="fixed",
     lambda=1,expectiles=c(0.05,0.25,0.75,0.95)))
plot(expectreg.ls(logratio~rb(range,"pspline"),data=lidar,smooth="fixed",
     lambda=0.0000001,expectiles=c(0.05,0.25,0.75,0.95)))
    #As can be seen in the plot, a too small penalty causes overfitting of the data.
plot(expectreg.ls(logratio~rb(range,"pspline"),data=lidar,smooth="fixed",
     lambda=50,expectiles=c(0.05,0.25,0.75,0.95)))
    #If the penalty parameter is chosen too large, 
    #the expectile curves are smooth but don't represent the data anymore.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ nonparametric }
\keyword{ smooth }% __ONLY ONE__ keyword per line
\keyword{ multivariate }
\keyword{ regression }
\keyword{ nonlinear }
\keyword{ models }

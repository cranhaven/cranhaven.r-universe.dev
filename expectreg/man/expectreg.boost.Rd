\name{expectreg.boost}
\Rdversion{1.1}
\alias{expectreg.boost}
\alias{quant.boost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quantile and expectile regression using boosting
}
\description{
Generalized additive models are fitted with gradient boosting for optimizing arbitrary 
loss functions to obtain the graphs of 11 different expectiles
for continuous, spatial or random effects.
}
\usage{
expectreg.boost(formula, data, mstop = NA, expectiles = NA, cv = TRUE, 
BoostmaxCores = 1, quietly = FALSE)

quant.boost(formula, data, mstop = NA, quantiles = NA, cv = TRUE, 
BoostmaxCores = 1, quietly = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
An R formula object consisting of the response variable, '~'
and the sum of all effects that should be taken into consideration (see \code{\link[mboost]{gamboost}}).
Each effect can be linear or represented through a nonlinear or spatial base
(see \code{\link[mboost]{bbs}}). Each variable has to be named consistently with \code{data}.
}
  \item{data}{
data frame (is required).
}
  \item{mstop}{
vector, number of bootstrap iterations for each of the 11 quantiles/expectiles that are fitted.
Default is 4000.
}
  \item{expectiles, quantiles}{
In default setting, the expectiles (0.01,0.02,0.05,0.1,0.2,0.5,0.8,0.9,0.95,0.98,0.99) are calculated.
You may specify your own set of expectiles in a vector.
}
  \item{cv}{
A cross-validation can determine the optimal amount of boosting iterations between 1 and \code{mstop}.
Uses \code{\link[mboost]{cvrisk}}. If set to \code{FALSE}, the results from \code{mstop} iterations are used.
}
  \item{BoostmaxCores}{
Maximum number of used cores for the different asymmetry parameters
}
  \item{quietly}{
If programm should run quietly.
}
}
\details{
A (generalized) additive model is fitted using a boosting algorithm based on component-wise univariate base learners. 
The base learner can be specified via the formula object. After fitting the model a cross-validation is done using
\code{\link[mboost]{cvrisk}} to determine the optimal stopping point for the boosting which results in the best fit.
}
\value{
An object of class 'expectreg', which is basically a list consisting of:
    \item{values}{ The fitted values for each observation and all expectiles,
                   separately in a list for each effect in the model,
                   sorted in order of ascending covariate values. }
    \item{response}{ Vector of the response variable. }
    \item{formula}{ The formula object that was given to the function. }
    \item{asymmetries}{ Vector of fitted expectile asymmetries as given by argument \code{expectiles}. }
    \item{effects}{ List of characters giving the types of covariates. }
    \item{helper}{ List of additional parameters like neighbourhood structure for spatial effects or 'phi' for kriging. }
    \item{fitted}{ Fitted values \eqn{ \hat{y} }. }
\code{\link[=plot.expectreg]{plot}}, \code{\link[=predict.expectreg]{predict}}, \code{\link[=resid.expectreg]{resid}}, \code{\link[=fitted.expectreg]{fitted}} and \code{\link[=effects.expectreg]{effects}}
methods are available for class 'expectreg'.
}
\references{
Fenske N and Kneib T and Hothorn T (2009)
\emph{ Identifying Risk Factors for Severe Childhood Malnutrition
by Boosting Additive Quantile Regression}
Technical Report 052, University of Munich

Sobotka F and Kneib T (2010)
\emph{ Geoadditive Expectile Regression }
Computational Statistics and Data Analysis,
doi: 10.1016/j.csda.2010.11.015.
}
\author{
Fabian Otto- Sobotka \cr
Carl von Ossietzky University Oldenburg \cr
\url{https://uol.de} \cr

Thomas Kneib, Elmar Spiegel \cr
Georg August University Goettingen \cr
\url{https://www.uni-goettingen.de} \cr
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{expectreg.ls}}, \code{\link[mboost]{gamboost}}, \code{\link[mboost]{bbs}}, \code{\link[mboost]{cvrisk}}
}
\examples{
data("lidar", package = "SemiPar")
ex <- expectreg.boost(logratio ~ bbs(range),lidar, mstop=200, 
                      expectiles=c(0.1,0.5,0.95),quietly=TRUE)
plot(ex)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ nonparametric }
\keyword{ smooth }% __ONLY ONE__ keyword per line
\keyword{ multivariate }
\keyword{ regression }
\keyword{ nonlinear }
\keyword{ models }

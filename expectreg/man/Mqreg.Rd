\name{Mqreg}
\alias{Mqreg}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Semiparametric M-Quantile Regression
}
\description{
Robust M-quantiles are estimated using an iterative penalised reweighted least squares approach.
Effects using quadratic penalties can be included, such as P-splines, Markov random fields or Kriging.
}
\usage{
Mqreg(formula, data = NULL, smooth = c("schall", "acv", "fixed"), 
      estimate = c("iprls", "restricted"),lambda = 1, tau = NA, robust = 1.345,
      adaptive = FALSE, ci = FALSE, LSMaxCores = 1)
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
  \item{estimate}{
Character string defining the estimation method that is used to fit the expectiles. Further detail on all available methods is given below.
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
  \item{tau}{
In default setting, the expectiles (0.01,0.02,0.05,0.1,0.2,0.5,0.8,0.9,0.95,0.98,0.99) are calculated.
You may specify your own set of expectiles in a vector. The option may be set to 'density' for the calculation
of a dense set of expectiles that enhances the use of \code{\link[expectreg]{cdf.qp}} and \code{\link[expectreg]{cdf.bundle}} afterwards.
}
  \item{robust}{
Robustness constant in M-estimation. See \code{Details} for definition.
}
  \item{adaptive}{
Logical. Whether the robustness constant is adapted along the covariates.  	
}
  \item{ci}{
Whether a covariance matrix for confidence intervals and the summary function is calculated.
}
 \item{LSMaxCores}{
 How many cores should maximal be used by parallelization
}
}
\details{
In the least squares approach the following loss function is minimised:

\eqn{ S = \sum_{i=1}^{n}{ w_p(y_i - m_i(p))^2} }

with weights

\eqn{ w_p(u) = (-(1-p)*c*(u_i< -c)+(1-p)*u_i*(u_i<0 \& u_i>=-c)+p*u_i*(u_i>=0 \& u_i<c)+p*c*(u_i>=c)) / u_i }

for quantiles and

\eqn{ w_p(u) = -(1-p)*c*(u_i< -c)+(1-p)*u_i*(u_i<0 \& u_i>=-c)+p*u_i*(u_i>=0 \& u_i<c)+p*c*(u_i>=c) }

for expectiles, with standardised residuals \eqn{u_i = 0.6745*(y_i - m_i(p)) / median(y-m(p))} and robustness constant c.

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
    \item{fitted}{ Fitted values \eqn{ \hat{y} }. }
\code{\link[=plot.expectreg]{plot}}, \code{\link[=predict.expectreg]{predict}}, \code{\link[=resid.expectreg]{resid}},
\code{\link[=fitted.expectreg]{fitted}}, \code{\link[=effects.expectreg]{effects}}
and further convenient methods are available for class 'expectreg'.
}
\references{
Pratesi M, Ranalli G and Salvati N (2009)
\emph{Nonparametric M-quantile regression using penalised splines}
Journal of Nonparametric Statistics, 21:3, 287-304.

Otto-Sobotka F, Ranalli G, Salvati N, Kneib T (2019)
\emph{Adaptive Semiparametric M-quantile Regression}
Econometrics and Statistics 11, 116-129.
}
\author{
Monica Pratesi \cr
University Pisa \cr
\url{https://www.unipi.it} \cr

M. Giovanna Ranalli \cr
University Perugia \cr
\url{https://www.unipg.it} \cr

Nicola Salvati \cr
University Perugia \cr
\url{https://www.unipg.it} \cr

Fabian Otto-Sobotka \cr
University Oldenburg \cr
\url{https://uol.de} \cr
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link[expectreg]{expectreg.ls}}, \code{\link[quantreg]{rqss}}
}
\examples{

data("lidar", package = "SemiPar")

m <- Mqreg(logratio~rb(range,"pspline"),data=lidar,smooth="f",
                        tau=c(0.05,0.5,0.95),lambda=10)
plot(m,rug=FALSE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ nonparametric }
\keyword{ smooth }% __ONLY ONE__ keyword per line
\keyword{ multivariate }
\keyword{ regression }
\keyword{ nonlinear }
\keyword{ models }

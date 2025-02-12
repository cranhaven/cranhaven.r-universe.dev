
\name{4. Least Angle Regression (LARS)}
\alias{LARS}
\title{Least Angle Regression to solve LASSO-type problems}
\usage{
LARS(Sigma, Gamma, method = c("LAR","LASSO"),
     nsup.max = NULL, steps.max = NULL, 
     eps = .Machine$double.eps*100, scale = TRUE, 
     sdx = NULL, mc.cores = 1L, save.at = NULL,
     precision.format = c("double","single"),
     fileID = NULL, verbose = 1)

}
\arguments{
\item{Sigma}{(numeric matrix) Variance-covariance matrix of predictors}

\item{Gamma}{(numeric matrix) Covariance between response variable and predictors. If it contains more than one column, the algorithm is applied to each column separately as different response variables}

\item{method}{(character) Either:
\itemize{
 \item \code{'LAR'}: Computes the entire sequence of all coefficients. Values of lambdas are calculated at each step.
 \item \code{'LASSO'}: Similar to \code{'LAR'} but solutions when a predictor leaves the solution are also returned.
}
Default is \code{method = 'LAR'}}

\item{nsup.max}{(integer) Maximum number of non-zero coefficients in the last LARS solution.
Default \code{nsup.max = NULL} will calculate solutions for the entire lambda sequence}

\item{steps.max}{(integer) Maximum number of steps (i.e., solutions) to be computed. Default \code{steps.max = NULL} will calculate solutions for the entire lambda sequence}

\item{eps}{(numeric) A numerical zero. Default is the machine precision}

\item{scale}{\code{TRUE} or \code{FALSE} to scale matrix \code{Sigma} for variables with unit variance and scale \code{Gamma} by the standard deviation (\code{sdx}) of the corresponding predictor
taken from the diagonal of \code{Sigma}}

\item{sdx}{(numeric vector) Scaling factor that will be used to scale the regression coefficients. When \code{scale = TRUE} this scaling factor vector is set to the squared root of the diagonal of \code{Sigma}, otherwise a provided value is used assuming that \code{Sigma} and \code{Gamma} are scaled}

\item{mc.cores}{(integer) Number of cores used. When \code{mc.cores} > 1, the analysis is run in parallel for each column of \code{Gamma}. Default is \code{mc.cores = 1}}

\item{save.at}{(character) Path where regression coefficients are to be saved (this may include a prefix added to the files). Default \code{save.at = NULL} will no save the regression coefficients and they are returned in the output object}

\item{fileID}{(character) Suffix added to the file name where regression coefficients are to be saved. Default \code{fileID = NULL} will automatically add sequential integers from 1 to the number of columns of \code{Gamma}}

\item{precision.format}{(character) Either 'single' or 'double' for numeric precision and memory occupancy (4 or 8 bytes, respectively) of the regression coefficients. This is only used when \code{save.at} is not \code{NULL}}

\item{verbose}{If numeric greater than zero details on each LARS step will be printed}
}
\value{
Returns a list object with the following elements:
\itemize{
  \item \code{lambda}: (vector) all the sequence of values of the LASSO penalty.
  \item \code{beta}: (matrix) regression coefficients for each predictor (in rows) associated to each value of the penalization parameter lambda (in columns).
  \item \code{nsup}: (vector) number of non-zero predictors associated to each value of lambda.
}
The returned object is of the class 'LASSO' for which methods \code{coef} and \code{predict} exist. Function 'path.plot' can be also used
}
\description{
Computes the entire LASSO solution for the regression coefficients, starting from zero, to the
least-squares estimates, via the Least Angle Regression (LARS) algorithm (Efron, 2004). It uses as inputs
a variance matrix among predictors and a covariance vector between response and predictors.
}
\details{
Finds solutions for the regression coefficients in a linear model

\ifelse{html}{\out{<p style='text-align:center'>y<sub>i</sub> = <b>x</b>'<sub>i</sub> <b>&beta;</b> + e<sub>i</sub></p>}}{\deqn{y_i=\textbf{x}_i'\boldsymbol{\beta}+e_i}{y_i = x'_i beta + e_i}}

where
\ifelse{html}{\out{y<sub>i</sub>}}{\eqn{y_i}{y_i}} is the response for the \ifelse{html}{\out{i<sup>th</sup>}}{\eqn{i^{th}}{i^th}} observation,
\ifelse{html}{\out{<b>x</b><sub>i</sub> = (x<sub>i1</sub>,...,x<sub>ip</sub>)'}}{\eqn{\textbf{x}_i=(x_{i1},...,x_{ip})'}{x_i=(x_i1,...,x_ip)'}}
is a vector of \eqn{p}{p} predictors assumed to have unit variance,
\ifelse{html}{\out{<b>&beta;</b> = (&beta;<sub>1</sub>,...,&beta;<sub>p</sub>)'}}{\eqn{\boldsymbol{\beta}=(\beta_1,...,\beta_p)'}{beta=(beta_1,...,beta_p)'}}
is a vector of regression coefficients, and
\ifelse{html}{\out{e<sub>i</sub>}}{\eqn{e_i}{e[i]}}
is a residual.

The regression coefficients
\ifelse{html}{\out{<b>&beta;</b>}}{\eqn{\boldsymbol{\beta}}{beta}}
are estimated as function of the variance matrix among
predictors (\ifelse{html}{\out{<b>&Sigma;</b>}}{\eqn{\boldsymbol{\Sigma}}{Sigma}}) and the covariance vector between response and predictors (\ifelse{html}{\out{<b>&Gamma;</b>}}{\eqn{\boldsymbol{\Gamma}}{Gamma}}) by minimizing the penalized mean squared error function

\ifelse{html}{\out{<p style='text-align:center'>-<b>&Gamma;</b>' <b>&beta;</b> + 1/2 <b>&beta;</b>'<b>&Sigma;</b><b>&beta;</b> + 1/2 &lambda; ||<b>&beta;</b>||<sub>1</sub></p>}}{\deqn{-\boldsymbol{\Gamma}' \boldsymbol{\beta} + 1/2 \boldsymbol{\beta}'\boldsymbol{\Sigma} \boldsymbol{\beta} + 1/2\lambda||\boldsymbol{\beta}||_1}{-Gamma' beta + 1/2 beta' Sigma beta + 1/2 lambda ||beta||_1}}

where \ifelse{html}{\out{&lambda;}}{\eqn{\lambda}{lambda}}
is the penalization parameter and
\ifelse{html}{\out{||<b>&beta;</b>||<sub>1</sub> = &sum;<sub>j=1</sub>|&beta;<sub>j</sub>|}}{\eqn{||\boldsymbol{\beta}||_1=\sum_{j=1}^p{|\beta_j|}}{||beta||_1=sum(|beta_j|)}}
is the L1-norm.

The algorithm to find solutions for each \ifelse{html}{\out{&beta;<sub>j</sub>}}{\eqn{\beta_j}{beta_j}} is fully described in Efron (2004) in which the "current correlation" between the predictor
\ifelse{html}{\out{x<sub>ij</sub>}}{\eqn{x_{ij}}{xij}}
and the residual
\ifelse{html}{\out{e<sub>i</sub> = y<sub>i</sub> - <b>x</b>'<sub>i</sub> <b>&beta;</b>}}{\eqn{e_i=y_i-\textbf{x}_i'\boldsymbol{\beta}}{e_i = y_i - x'_i beta}}
is expressed (up-to a constant) as

\ifelse{html}{\out{<p style='text-align:center'>r<sub>j</sub> = &Gamma;<sub>j</sub> - <b>&Sigma;</b>'<sub>j</sub> <b>&beta;</b></p>}}{\deqn{r_j=\Gamma_j-\boldsymbol{\Sigma}_j'\boldsymbol{\beta}}{r_j = Gamma_j - Sigma'_j beta}}

where
\ifelse{html}{\out{&Gamma;<sub>j</sub>}}{\eqn{\Gamma_j}{Gamma_j}}
is the \ifelse{html}{\out{j<sup>th</sup>}}{\eqn{j^{th}}{j^th}} element of
\ifelse{html}{\out{<b>&Gamma;</b>}}{\eqn{\boldsymbol{\Gamma}}{Gamma}} and
\ifelse{html}{\out{<b>&Sigma;</b><sub>j</sub>}}{\eqn{\boldsymbol{\Sigma}_j}{Sigma_j}}
is the \ifelse{html}{\out{j<sup>th</sup>}}{\eqn{j^{th}}{j^th}} column of the matrix
\ifelse{html}{\out{<b>&Sigma;</b>}}{\eqn{\boldsymbol{\Sigma}}{Sigma}}
}
\examples{
  require(SFSI)
  data(wheatHTP)
  
  y = as.vector(Y[,"E1"])   # Response variable
  X = scale(X_E1)           # Predictors

  # Training and testing sets
  tst = which(Y$trial \%in\% 1:10)
  trn = seq_along(y)[-tst]

  # Calculate covariances in training set
  XtX = var(X[trn,])
  Xty = cov(X[trn,],y[trn])
  
  # Run the penalized regression
  fm = LARS(XtX, Xty, method="LASSO")  
  
  # Regression coefficients
  dim(coef(fm))
  dim(coef(fm, ilambda=50)) # Coefficients associated to the 50th lambda
  dim(coef(fm, nsup=25))    # Coefficients with around nsup=25 are non-zero

  # Predicted values
  yHat1 = predict(fm, X=X[trn,])  # training data
  yHat2 = predict(fm, X=X[tst,])  # testing data
  
  # Penalization vs correlation
  plot(-log(fm$lambda[-1]),cor(y[trn],yHat1[,-1]), main="Training", type="l")
  plot(-log(fm$lambda[-1]),cor(y[tst],yHat2[,-1]), main="Testing", type="l")
}
\references{
Efron B, Hastie T, Johnstone I, Tibshirani R (2004). Least angle regression. \emph{The Annals of Statistics}, \bold{32}(2), 407–499.

Hastie T, Efron B (2013). lars: least angle regression, Lasso and forward stagewise. \url{https://cran.r-project.org/package=lars}.
}
\author{
Adapted from the 'lars' function in package 'lars' (Hastie & Efron, 2013)
}

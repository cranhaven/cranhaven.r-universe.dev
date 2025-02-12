
\name{3. Solve an Elastic-Net problem}
\alias{solveEN}
\title{Coordinate Descent algorithm to solve Elastic-Net-type problems}
\usage{
solveEN(Sigma, Gamma, alpha = 1, lambda = NULL, nlambda = 100,
        lambda.min = .Machine$double.eps^0.5, lambda.max = NULL,
        common.lambda = TRUE, beta0 = NULL, nsup.max = NULL,
        scale = TRUE, sdx = NULL, tol = 1E-5, maxiter = 1000,
        mc.cores = 1L, save.at = NULL, fileID = NULL,
        precision.format = c("double","single"), sparse = FALSE,
        eps = .Machine$double.eps*100, verbose = FALSE)
        
}
\arguments{
\item{Sigma}{(numeric matrix) Variance-covariance matrix of predictors}

\item{Gamma}{(numeric matrix) Covariance between response variable and predictors. If it contains more than one column, the algorithm is applied to each column separately as different response variables}

\item{lambda}{(numeric vector) Penalization parameter sequence. Default is \code{lambda = NULL}, in this case a decreasing grid of
\code{'nlambda'} lambdas will be generated starting from a maximum equal to

\ifelse{html}{\out{<p style='text-align:center;font-family:courier'>max(abs(Gamma)/alpha)</p>}}{\deqn{\code{max(abs(Gamma)/alpha)}}{max(abs(Gamma)/alpha)}}

to a minimum equal to zero. If \code{alpha = 0} the grid is generated starting from a maximum equal to 5}

\item{nlambda}{(integer) Number of lambdas generated when \code{lambda = NULL}}

\item{lambda.min, lambda.max}{(numeric) Minimum and maximum value of lambda that are generated when \code{lambda = NULL}}

\item{common.lambda}{\code{TRUE} or \code{FALSE} to whether computing the coefficients for a grid of lambdas common to all columns of \code{Gamma}
 or for a grid of lambdas specific to each column of \code{Gamma}. Default is \code{common.lambda = TRUE}}
 
\item{beta0}{(numeric vector) Initial value for the regression coefficients that will be updated. If \code{beta0 = NULL} a vector of zeros will be considered. These values will be used as starting values for the first lambda value}

\item{alpha}{(numeric) Value between 0 and 1 for the weights given to the L1 and L2-penalties}

\item{scale}{\code{TRUE} or \code{FALSE} to scale matrix \code{Sigma} for variables with unit variance and scale \code{Gamma} by the standard deviation (\code{sdx}) of the corresponding predictor
taken from the diagonal of \code{Sigma}}

\item{sdx}{(numeric vector) Scaling factor that will be used to scale the regression coefficients. When \code{scale = TRUE} this scaling factor vector is set to the squared root of the diagonal of \code{Sigma}, otherwise a provided value is used assuming that \code{Sigma} and \code{Gamma} are scaled}

\item{tol}{(numeric) Maximum error between two consecutive solutions of the CD algorithm to declare convergence}

\item{maxiter}{(integer) Maximum number of iterations to run the CD algorithm at each lambda step before convergence is reached}

\item{nsup.max}{(integer) Maximum number of non-zero coefficients in the last solution.
Default \code{nsup.max = NULL} will calculate solutions for the entire lambda grid}

\item{mc.cores}{(integer) Number of cores used. When \code{mc.cores} > 1, the analysis is run in parallel for each column of \code{Gamma}. Default is \code{mc.cores = 1}}

\item{save.at}{(character) Path where regression coefficients are to be saved (this may include a prefix added to the files). Default \code{save.at = NULL} will no save the regression coefficients and they are returned in the output object}

\item{fileID}{(character) Suffix added to the file name where regression coefficients are to be saved. Default \code{fileID = NULL} will automatically add sequential integers from 1 to the number of columns of \code{Gamma}}

\item{precision.format}{(character) Either 'single' or 'double' for numeric precision and memory occupancy (4 or 8 bytes, respectively) of the regression coefficients. This is only used when \code{save.at} is not \code{NULL}}

\item{sparse}{\code{TRUE} or \code{FALSE} to whether matrix \code{Sigma} is sparse with entries being zero or near-zero}

\item{eps}{(numeric) A numerical zero to determine if entries are near-zero. Default is the machine precision}

\item{verbose}{\code{TRUE} or \code{FALSE} to whether printing progress}
}
\value{
Returns a list object containing the elements:
\itemize{
  \item \code{lambda}: (vector) all the sequence of values of the penalty.
  \item \code{beta}: (matrix) regression coefficients for each predictor (in rows) associated to each value of the penalization parameter lambda (in columns).
  \item \code{nsup}: (vector) number of non-zero predictors associated to each value of lambda.
}
The returned object is of the class 'LASSO' for which methods \code{coef} and \code{fitted} exist. Function 'path.plot' can be also used
}
\description{
Computes the entire Elastic-Net solution for the regression coefficients for all
values of the penalization parameter, via the Coordinate Descent (CD) algorithm (Friedman, 2007). It uses as inputs a variance matrix among predictors and a covariance vector between response and predictors
}
\details{
Finds solutions for the regression coefficients in a linear model

\ifelse{html}{\out{<p style='text-align:center'>y<sub>i</sub> = <b>x</b>'<sub>i</sub> <b>&beta;</b> + e<sub>i</sub></p>}}{\deqn{y_i=\textbf{x}_i'\boldsymbol{\beta}+e_i}{y_i = x'_i beta + e_i}}

where
\ifelse{html}{\out{y<sub>i</sub>}}{\eqn{y_i}{y_i}} is the response for the \ifelse{html}{\out{i<sup>th</sup>}}{\eqn{i^{th}}{i^th}} observation,
\ifelse{html}{\out{<b>x</b><sub>i</sub> = (x<sub>i1</sub>,...,x<sub>ip</sub>)'}}{\eqn{\textbf{x}_i=(x_{i1},...,x_{ip})'}{x_i=(x_i1,...,x_ip)'}}
is a vector of \eqn{p}{p} predictors assumed to have unit variance,
\ifelse{html}{\out{<b>&beta;</b> = (&beta;<sub>1</sub>,...,&beta;<sub>p</sub>)'}}{\eqn{\boldsymbol{\beta}=(\beta_1,...,\beta_p)'}{beta=(beta[1],...,beta[p])'}}
is a vector of regression coefficients, and
\ifelse{html}{\out{e<sub>i</sub>}}{\eqn{e_i}{e_i}}
is a residual.

The regression coefficients
\ifelse{html}{\out{<b>&beta;</b>}}{\eqn{\boldsymbol{\beta}}{beta}}
are estimated as function of the variance matrix among
predictors (\ifelse{html}{\out{<b>&Sigma;</b>}}{\eqn{\boldsymbol{\Sigma}}{Sigma}}) and the covariance vector between response and predictors (\ifelse{html}{\out{<b>&Gamma;</b>}}{\eqn{\boldsymbol{\Gamma}}{Gamma}}) by minimizing the penalized mean squared error function

\ifelse{html}{\out{<p style='text-align:center'>-<b>&Gamma;</b>' <b>&beta;</b> + 1/2 <b>&beta;</b>' <b>&Sigma;</b> <b>&beta;</b> + &lambda; J(<b>&beta;</b>)</p>}}{\deqn{-\boldsymbol{\Gamma}' \boldsymbol{\beta} + 1/2 \boldsymbol{\beta}'\boldsymbol{\Sigma} \boldsymbol{\beta} + \lambda J(\boldsymbol{\beta})}{-Gamma' beta + 1/2 beta' Sigma beta + lambda J(beta)}}

where \ifelse{html}{\out{&lambda;}}{\eqn{\lambda}{lambda}}
is the penalization parameter and
\ifelse{html}{\out{J(<b>&beta;</b>)}}{\eqn{J(\boldsymbol{\beta})}{J(beta)}}
is a penalty function given by

\ifelse{html}{\out{<p style='text-align:center'>1/2(1-&alpha;)||<b>&beta;</b>||<sub>2</sub><sup>2</sup> + &alpha;||<b>&beta;</b>||<sub>1</sub></p>}}{\deqn{1/2(1-\alpha)||\boldsymbol{\beta}||_2^2 + \alpha||\boldsymbol{\beta}||_1}{1/2(1-alpha)||beta||_2^2 + alpha||beta||_1}}

where \ifelse{html}{\out{0 &le; &alpha; &le; 1}}{\eqn{0\leq\alpha\leq 1}{0 <= alpha <= 1}}, and
\ifelse{html}{\out{||<b>&beta;</b>||<sub>1</sub> = &sum;<sub>j=1</sub>|&beta;<sub>j</sub>|}}{\eqn{||\boldsymbol{\beta}||_1=\sum_{j=1}^p|\beta_j|}{||beta||_1 = sum(|beta_j|)}} and
\ifelse{html}{\out{||<b>&beta;</b>||<sub>2</sub><sup>2</sup> = &sum;<sub>j=1</sub>&beta;<sub>j</sub><sup>2</sup>}}{\eqn{||\boldsymbol{\beta}||_2^2=\sum_{j=1}^p\beta_j^2}{||beta||_2^2 = sum(beta_j^2)}} are the L1 and (squared) L2-norms, respectively.

The "partial residual" excluding the contribution of the predictor
\ifelse{html}{\out{x<sub>ij</sub>}}{\eqn{x_{ij}}{x_ij}} is

\ifelse{html}{\out{<p style='text-align:center'>e<sub>i</sub><sup>(j)</sup> = y<sub>i</sub> - <b>x</b>'<sub>i</sub> <b>&beta;</b> + x<sub>ij</sub>&beta;<sub>j</sub></p>}}{\deqn{e_i^{(j)}=y_i-\textbf{x}_i'\boldsymbol{\beta}+x_{ij}\beta_j}{r_i = y_i - x'_i beta + x_ij beta_j}}

then the ordinary least-squares (OLS) coefficient of
\ifelse{html}{\out{x<sub>ij</sub>}}{\eqn{x_{ij}}{x_ij}}
on this residual is (up-to a constant)

\ifelse{html}{\out{<p style='text-align:center'>&beta;<sub>j</sub><sup>(ols)</sup> = &Gamma;<sub>j</sub> - <b>&Sigma;</b>'<sub>j</sub> <b>&beta;</b> + &beta;<sub>j</sub></p>}}{\deqn{\beta_j^{(ols)}=\Gamma_j-\boldsymbol{\Sigma}_j'\boldsymbol{\beta}+\beta_j}{beta_j^ols = Gamma_j - Sigma'_j beta}}

where
\ifelse{html}{\out{&Gamma;<sub>j</sub>}}{\eqn{\Gamma_j}{Gamma_j}}
is the \ifelse{html}{\out{j<sup>th</sup>}}{\eqn{j^{th}}{j^th}} element of
\ifelse{html}{\out{<b>&Gamma;</b>}}{\eqn{\boldsymbol{\Gamma}}{Gamma}} and
\ifelse{html}{\out{<b>&Sigma;</b><sub>j</sub>}}{\eqn{\boldsymbol{\Sigma}_j}{Sigma_j}}
is the \ifelse{html}{\out{j<sup>th</sup>}}{\eqn{j^{th}}{j^th}} column of the matrix
\ifelse{html}{\out{<b>&Sigma;</b>}}{\eqn{\boldsymbol{\Sigma}}{Sigma}}.

Coefficients are updated for each \eqn{j=1,...,p}{j=1,...,p} from their current value
\ifelse{html}{\out{&beta;<sub>j</sub>}}{\eqn{\beta_j}{beta_j}}
to a new value
\ifelse{html}{\out{&beta;<sub>j</sub>(&alpha;,&lambda;)}}{\eqn{\beta_j(\alpha,\lambda)}{beta_j(alpha,lambda)}},
given \ifelse{html}{\out{&alpha;}}{\eqn{\alpha}{alpha}} and
\ifelse{html}{\out{&lambda;}}{\eqn{\lambda}{lambda}},
by "soft-thresholding" their OLS estimate until convergence as fully described in Friedman (2007).
}
\examples{
  require(SFSI)
  data(wheatHTP)
  
  y = as.vector(Y[,"E1"])  # Response variable
  X = scale(X_E1)          # Predictors

  # Training and testing sets
  tst = which(Y$trial \%in\% 1:10)
  trn = seq_along(y)[-tst]

  # Calculate covariances in training set
  XtX = var(X[trn,])
  Xty = cov(X[trn,],y[trn])
  
  # Run the penalized regression
  fm = solveEN(XtX,Xty,alpha=0.5,nlambda=100) 
  
  # Regression coefficients
  dim(coef(fm))
  dim(coef(fm, ilambda=50)) # Coefficients associated to the 50th lambda
  dim(coef(fm, nsup=25))    # Coefficients with around nsup=25 are non-zero

  # Predicted values
  yHat1 = predict(fm, X=X[trn,])  # training data
  yHat2 = predict(fm, X=X[tst,])  # testing data
  
  # Penalization vs correlation
  plot(-log(fm$lambda[-1]),cor(y[trn],yHat1[,-1]), main="training", type="l")
  plot(-log(fm$lambda[-1]),cor(y[tst],yHat2[,-1]), main="testing", type="l")
}
\references{
Friedman J, Hastie T, Höfling H, Tibshirani R (2007). Pathwise coordinate optimization. \emph{The Annals of Applied Statistics}, \bold{1}(2), 302–332.
}

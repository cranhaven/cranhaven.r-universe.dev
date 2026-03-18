\name{Derv1}
\alias{Derv1}
\title{Calculating the first derivative of the pendensity likelihood function w.r.t. parameter beta}
\description{
 Calculating the first derivative of the pendensity likelihood function w.r.t. parameter beta. 
}
\usage{
Derv1(penden.env)
}

\arguments{
	\item{penden.env}{Containing all information, environment of pendensity()}
}
\details{
We calculate the first derivative of the pendensity likelihood function w.r.t. the parameter beta. The calculation of the first derivative of the pendensity likelihood function w.r.t. parameter beta is done in four steps. The first derivative equals in the case of covariates
\deqn{s(\beta)= {\partial l(\beta)}/{\partial \beta}= \sum_{i=1}^n s_i(\beta)}{%
      \eqn{s(beta) = sum si(beta),}}
where
\deqn{s_i(\beta) = {{\bf \mathcal{Z}}^T(x_i)} {{\bf \mathcal{C}}^T (x_i,\beta)} \frac{\tilde{\boldsymbol\phi}_i}{\hat{f}(y_i|x_i)}}{%
     \eqn{s[i](beta) = Z^T(x[i]) C^T (x[i],beta) phi[i] / f(y[i]|x[i])}.}\cr
Without covariates, the matrix 'Z' doesn't appear. Starting density calculation, the groupings of the covariates are indexed in the main program. The groupings are saved in 'x.factor'. Creating an index that reports which response belongs to which covariate group, saving in 'Z.index'. Therefore, one can link to the rows in the object 'ck' to calculate the matrix 'C.bold', which depends only on the grouping of the covariate. Without any covariate, 'C.bold' is equal for every observation.\cr
The calculation of the first derivative of the pendensity likelihood function w.r.t. parameter beta is done in four steps. Firstly, we calculate the matrix 'C.bold', depending on the groups of 'x.factor'.\cr
Secondly, for calculating we need the fitted values of each observation, 'f.hat'. These values are calculated for the actual parameter set beta in the program 'f.hat'. Of course, we need the value of the base for each observation, \eqn{\phi[i]}.\cr
Moreover, for the case of conditional density estimation, we need a Z-Matrix, due to the rules for derivations of the function 'exp()'. This Z-matrix doesn't appear directly in the calculations. We construct the multiplication with this Z-matrix with using an outer product between the corresponding grouping in 'x.factor' and the product of the corresponding values 'C.bold' and 'base.den', divided by the fitted value 'f.hat'. Finally, we add some penalty on the derivative, which is calculated in the fourth step. The penalty equals \eqn{-\lambda D_m \beta }.\cr
For later use, we save the unpenalized first derivative as a matrix, in which the i-th column contains the first derivative of the pendensity likelihood function, evaluated for the i-th value of the response. The needed values are saved in the environment.}

\value{
  \item{Derv1.cal}{matrix, in which the i-th column contains the first derivative, evaluated for the i-th value of the response variable without penalty. Needed for calculating the second order derivative, called \eqn{s(\beta)}}
  \item{Derv1.pen}{first order derivation of the penalized likelihood function w.r.t. parameter beta, called \deqn{s_p(\beta)}{%
 \eqn{sp(beta)}} }
  \item{f.hat.val}{fitted values of the response for actual parameter beta, called \eqn{\hat{f}}}
}
\references{Density Estimation with a Penalized Mixture Approach, Schellhase C. and Kauermann G. (2012), Computational Statistics 27 (4), p. 757-777.}
\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}

\keyword{math}


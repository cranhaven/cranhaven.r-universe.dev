\name{distfreereg}
\alias{distfreereg}
\alias{distfreereg.default}
\alias{distfreereg.formula}
\alias{distfreereg.function}
\alias{distfreereg.lm}
\alias{distfreereg.nls}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Distribution-Free Parametric Regression Testing
}
\description{
	Conduct distribution-free parametric regression testing using the process introduced in \cite{Khmaladze (2021)}. A parametric model for the conditional mean (specified by \code{test_mean}) is checked against the data by fitting the model, transforming the resulting residuals, and then calculating a statistic on the empirical partial sum process of the transformed residuals. The statistic's null distribution can be simulated in a straight-forward way, thereby producing a p-value.
	
	Using \eqn{f} to denote the mean function being tested, the specific test has the following null and alternative hypotheses:
	\deqn{H_0\colon\ \exists\theta\in\Theta\subseteq\mathbb R^p \mathrel{\bigl|} \textrm{E}(Y| X)=f(X;\theta)
  \quad\hbox{against}\quad
  H_1\colon\ \forall\theta\in\Theta\subseteq\mathbb R^p \mathrel{\bigl|} \textrm{E}(Y| X)\neq f(X;\theta).}
	
	See the \href{../doc/v1_introduction.html}{An Introduction to the \code{distfreereg} Package} vignette for an introduction.
}
\usage{
	distfreereg(test_mean, ordering = "simplex", group = FALSE,
	stat = c("KS", "CvM"), B = 1e4, control = NULL, override = NULL, verbose = TRUE,
	...)

	\method{distfreereg}{default}(test_mean = NULL, ordering = "simplex", group = FALSE,
	stat = c("KS", "CvM"), B = 1e4, control = NULL, override = NULL, verbose = TRUE,
	..., Y, X = NULL, covariance, J, fitted_values)

	\method{distfreereg}{formula}(test_mean, ordering = "simplex", group = FALSE,
	stat = c("KS", "CvM"), B = 1e4, control = NULL, override = NULL, verbose = TRUE,
	..., data, covariance = NULL, method = "lm", theta_init = NULL)

	\method{distfreereg}{function}(test_mean, ordering = "simplex", group = FALSE,
	stat = c("KS", "CvM"), B = 1e4, control = NULL, override = NULL, verbose = TRUE,
	..., Y, X = NULL, covariance, theta_init)

	\method{distfreereg}{lm}(test_mean, ordering = "simplex", group = FALSE,
	stat = c("KS", "CvM"), B = 1e4, control = NULL, override = NULL, verbose = TRUE,
	...)

	\method{distfreereg}{nls}(test_mean, ordering = "simplex", group = FALSE,
	stat = c("KS", "CvM"), B = 1e4, control = NULL, override = NULL, verbose = TRUE,
	...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{test_mean}{
	A specification of the mean function to be tested. Methods exist for objects of classes \code{function}, \code{formula}, \code{lm}, and \code{nls}. See details.
}
  \item{covariance}{
	Named list; specifies the covariance structure of the model's error distribution. Valid element names are "\code{Sigma}", "\code{SqrtSigma}", "\code{P}", and "\code{Q}", corresponding to the covariance matrix, the square root of the covariance matrix, the precision matrix, and the square root of the precision matrix, respectively. Each element must be one of the following:
	\itemize{
	  \item a numeric matrix.
	  \item a numeric vector whose length is the sample size.
	  \item a numeric vector of length 1.
	}
	See details.
}
  \item{ordering}{
	A character string or a list; specifies how to order the residuals to form the empirical partial sum process. Valid character strings are:
	\itemize{
	  \item "\code{asis}": leaves the order unchanged (that is, in the order in which the observations appear in the supplied data).
	  \item "\code{natural}": orders residuals using column-wise ordering of the covariates.
	  \item "\code{optimal}": orders the residuals by ordering the observations using optimal transport on the covariates. The solution is estimated using the Hungarian method as implemented by \code{\link[clue]{solve_LSAP}}. This option can be very slow for large sets of covariates.
	  \item "\code{simplex}": (the default) orders residuals in order of increasing row sums of the covariates after each column has been scaled to the interval \eqn{[0,1]}.
	}
	If \code{ordering} is a list, then its elements specify columns of \code{X} or \code{data} to use to determine the order. The elements can be column names or numbers, but not a mix of the two.
}
  \item{group}{
  Logical; if \code{TRUE}, then columns specified by \code{ordering} are used to group observations (by summation) before forming the partial sum process. Can be \code{TRUE} only when \code{ordering} is "\code{natural}" or a list of column specifications.
}
  \item{J}{
  Numeric matrix; specifies the Jacobian of the function evaluated at the covariates and the estimated parameters.
}
  \item{fitted_values}{
  Numeric vector; specifies the model's fitted values.
}
  \item{stat}{
	Character vector; specifies the names of the functions used to calculate the desired statistics. By default, a Kolmogorov--Smirnov statistic and a Cramer--von Mises-like statistic are calculated:
	\deqn{\hbox{KS} = \max_{i}|a_i|\qquad\hbox{and}\qquad\hbox{CvM} = {1\over n}\sum_{i=1}^na_i^2} where \eqn{a_i} is term \eqn{i} in the empirical partial sum process and \eqn{n} is the sample size.
}
  \item{B}{
	Numeric vector of length one; specifies the Monte Carlo sample size used when simulating statistics. Silently converted to integer.
}
  \item{control}{
  Optional named list of elements that control the details of the algorithm's computations. The following elements are accepted for all methods:
  \itemize{
    \item \code{symmetric}:	Optional named list or \code{FALSE}; if a named list, its elements are passed as arguments to \code{\link{isSymmetric}} when testing elements of \code{covariance} for symmetry. If \code{FALSE}, then this test is skipped.
	\item \code{matsqrt_tol}: Numeric; specifies the threshold for considering an eigenvalue "too negative" when calculating the square root of a matrix. Must be non-positive. The default value is \code{-.Machine$double.eps^0.25}.
	\item \code{solve_tol}: Numeric; passed as \code{tol} argument to \code{\link{solve}}, used in particular to invert \code{Sigma}. The default value is \code{.Machine$double.eps}.
	\item \code{qr_tol}: Numeric; passed as \code{tol} argument to \code{\link{qr}}. The default value is \code{sqrt(.Machine$double.eps)}. This value might need to be decreased when the dimensions of the Jacobian are sufficiently large.
	\item \code{orth_tol}: Numeric; passed as \code{tolerance} argument to \code{\link{all.equal}} when testing whether or not \eqn{r^Tr} is the identity matrix. The default value is \code{sqrt(.Machine$double.eps)}.
	\item \code{trans_tol}: Numeric; passed as \code{tolerance} argument to \code{\link{all.equal}} to internal transformation function when determining whether the normalizing scalar is non-zero. The default value is \code{sqrt(.Machine$double.eps)}.
  }
  The following named elements, all but the first of which control the process of calculating the generalized least squares estimation of the parameter vector, are accepted for the \code{function} method:
  \itemize{
  	\item \code{jacobian_args}: Optional list; specifies arguments to pass to \code{\link[numDeriv]{jacobian}}.
    \item \code{optimization_fun}: Optional function; specifies the function used to estimate the parameters. If not specified, \code{\link{optim}} is used with \code{method = "BFGS"}.
    \item \code{fun_to_optimize_arg}: Optional character string, required when \code{optimization_fun} is specified; specifies the name of the argument of \code{optimization_fun} that is assigned the function to optimize. For example, \code{\link{optim}} uses "\code{fn}".
    \item \code{theta_init_arg}: Optional character string, required when \code{optimization_fun} is specified; specifies the name of the argument of \code{optimization_fun} that is assigned the initial parameter values for optimization. For example, \code{\link{optim}} uses "\code{par}".
    \item \code{theta_hat_name}: Optional character string, required when \code{optimization_fun} is specified; specifies the name of the element of the output of \code{optimization_fun} that contains the estimated parameters. For example, \code{\link{optim}} uses "\code{par}" (the same character string that specifies the argument containing the initial values). See Warnings.
    \item \code{optimization_args}: Optional list; specifies additional arguments to pass to \code{optimization_fun}.
  }
  Finally, the following element is available for the \code{formula} method:
  \itemize{
    \item \code{method_args}: Optional list of argument values to be passed to \code{\link{lm}} or \code{\link{nls}}, according to the value of the \code{method} argument.
  }
}
  \item{override}{
  Optional named list of arguments that override internally calculated values. Used primarily by \code{\link{update.distfreereg}}, but can be accessed directly. The following named elements are accepted:
  	\itemize{
	  \item \code{J}: Numeric matrix. Overrides the calculation of \code{J} by non-default methods.
	  \item \code{fitted_values}: Numeric vector of fitted values.
	  \item \code{res_order}: Integer vector specifying the permutation (analogous to the output of \code{\link{order}}) that orders the residuals for the computation of the partial sums. Overrides \code{ordering}.
	  \item \code{theta_hat}: Numeric vector. Used as the estimated parameter vector when \code{test_mean} has class \code{function}, overriding internal computation that would have used \code{optimization_fun}.
	  \item \code{r}: Numeric matrix. Overrides the construction of the transformation anchors.
	  \item \code{mcsim_stats}: List. Overrides the creation of the list of simulated statistics.
}
}
  \item{verbose}{
	Logical; if \code{TRUE}, progress messages are printed. Default value is \code{TRUE}.
}
  \item{...}{
	Additional arguments to pass to various methods; should be empty except in a call to the generic function.
}
  \item{Y}{
	Numeric vector of observations. A matrix value is silently converted to a vector.
}
  \item{X}{
	Optional numeric matrix of covariates. A vector value is converted to a single-column matrix with a warning.
}
  \item{method}{
	Character vector; specifies the function to use for fitting the model when \code{test_mean} is a formula. Possible values are "\code{lm}" and "\code{nls}", specifying \code{\link{lm}} and \code{\link{nls}}, respectively.
}
  \item{theta_init}{
	Numeric vector; specifies the starting parameter values passed to the optimizing function to be used to estimate the parameter vector. Must be \code{NULL} when \code{method} is "\code{lm}". Optional for \code{formula} method when \code{method} is "\code{nls}", but must be a \strong{named} vector if present in this case.
}
  \item{data}{
	Optional data frame of covariate values; required for \code{formula} method, must be absent otherwise.
}
}
\details{
	This function implements distribution-free parametric regression testing. The model is specified by a mean structure and a covariance structure.
	
	The mean structure is specified by the argument \code{test_mean}. This can be a function, formula, \code{lm} object, \code{nls} object, or \code{NULL}.
	
	If \code{test_mean} is a function, then it must have one or two arguments: either \code{theta} only, or \code{theta} and either \code{X} (uppercase) or \code{x} (lowercase). An uppercase \code{X} is interpreted in the function definition as a matrix, while a lowercase \code{x} is interpreted as a vector. (See examples and \href{../doc/v5_advanced-options.html}{this vignette}.) The primary reason to use a lowercase \code{x} is to allow for a function definition using an \code{R} function that is not vectorized. In general, an uppercase \code{X} should be preferred for speed.
	
	If \code{test_mean} is an \code{lm} or \code{nls} object, then the covariance structure is obtained from the supplied model.
	
	If \code{test_mean} is a formula, then it must be a formula that can be passed to \code{\link{lm}} or \code{\link{nls}}, and the \code{data} argument must be specified. The appropriate model will be created, and then sent back to \code{distfreereg()} for method dispatch.
	
	The \code{function} method estimates parameter values, and then uses those to evaluate the Jacobian of the mean function and to calculate fitted values. It then calls the default method, which does not use \code{test_mean}. The default method also allows the user to implement the algorithm even when the mean structure is not specified in \code{R}. (This is useful if a particularly complicated function is defined in another language and cannot easily be copied into \code{R}.) It requires specifying the vector of fitted values and the Jacobian matrix of the mean function evaluated at the estimated parameter values.
	
	The covariance structure for \eqn{Y|X} must be specified using the \code{covariance} argument for the \code{function} and default methods. It is optional for the \code{formula} method; when present in that case, it must specify a diagonal matrix which is converted internally into a vector of weights. For the \code{lm} and \code{nls} methods, the covariance is determined using the supplied object.
	
	Any element of \code{covariance} can be a numeric matrix, or a numeric vector. If it is a vector, its length must be either 1 or the sample size. This option is mathematically equivalent to setting a covariance list element to a diagonal matrix with the specified value(s) along the diagonal. Using vectors, when possible, is more efficient than using the corresponding matrix.
	
	Internally, \code{distfreereg()} only needs \code{Q}, so some efficiency can be gained by supplying that directly when available. When \code{Q} is not specified, it is calculated using whichever element is specified. When more than one of the other elements are specified, \code{Q} is calculated using the least expensive path, with no warning given if the specified elements are incompatible. (For example, if both \code{Sigma} and \code{SqrtSigma} elements are supplied to \code{covariance}, then \code{Q} is calculated using \code{SqrtSigma} with no attempt to verify that \code{SqrtSigma} is the matrix square root of\code{Sigma}.)
		
	The \code{override} argument is used primarily by \code{\link{update.distfreereg}} to avoid unnecessary and potentially computationally expensive recomputation. This \code{\link{update}} method imports appropriate values automatically from a previously created object of class \code{distfreereg}, and therefore validation is not always done. Use manually with caution.
}

\section{Warnings}{
Consistency between \code{test_mean} and \code{theta_init} is verified only
indirectly. Uninformative errors can occur when, for example, \code{theta_init}
does not have the correct length. The two most common error messages that arise
in this case are "\code{f_out cannot have NA values}", indicating that
\code{theta_init} is too short, and "\code{Unable to invert square root of J^tJ}",
indicating that \code{theta_init} is too long. (Both of these errors might occur
for other reasons, as well.) To be safe, always define \code{test_mean} to use
every element of \code{theta}.

No verification of consistency is done when multiple elements of \code{coviariance} are specified. For example, if \code{P} and \code{Sigma} are both specified, then the code will use only one of these, and will not verify that \code{P} is the inverse of \code{Sigma}.

When using the \code{control} argument element \code{optimization_fun} to
specify an optimization function other than \code{\link{optim}}, the
verification that \code{theta_hat_name} actually matches the name of an element
of the optimization function's output is done only after the optimization has
been done. If this optimization will likely take a long time, it is important to
verify the value of \code{theta_hat_name} before running \code{distfreereg()}.
}


\value{
An object of class \code{distfreereg} with the following components:

	\item{call}{The matched call.}
	\item{data}{A list containing \code{data}, if present, and \code{Y} and \code{X}.}
	\item{test_mean}{The value supplied to the argument \code{test_mean}.}
	\item{model}{The model built when using the \code{formula} method; only present when using that method.}
	\item{covariance}{The list of covariance matrices, containing at least \code{Q}.}
	\item{theta_hat}{The estimated parameter vector.}
	\item{optimization_output}{The output of \code{optimization_fun} or \code{\link{nls}} from calculating \code{theta_hat}.}
	\item{fitted_values}{The vector of fitted values, \eqn{f\bigl(X,\hat\theta\bigr)}.}
	\item{J}{The Jacobian matrix.}
	\item{mu}{The mu matrix.}
	\item{r}{The matrix of transformation anchor vectors.}
	\item{r_tilde}{The matrix of modified transformation anchor vectors.}
	\item{residuals}{A named list of three vectors containing raw, sphered, and transformed residuals.}
	\item{res_order}{A numeric vector indicating the ordering of the residuals used to form the empirical partial sum process, in a format analogous to the output	of \code{\link{order}}.}
	\item{epsp}{The empirical partial sum process formed by calculating the scaled
	partial sums of the transformed residuals ordered according to \code{res_order}.}
	\item{observed_stat}{A named list of the observed statistic(s) corresponding to the transformed residuals.}
	\item{mcsim_stats}{A named list, each element of which contains the values of a simulated statistic.}
	\item{p}{A named list with two elements: \code{value}, which contains the p-values for each observed statistic, and \code{mcse}, which contains the Monte Carlo standard errors for the p-values.}
}
\references{
	Khmaladze, Estate V. \emph{Distribution-free testing in linear and parametric regression}, 2021-03, Annals of the Institute of Statistical Mathematics, Vol. 73, No. 6, p. 1063--1087. \doi{10.1007/s10463-021-00786-3}
}
\author{
	Jesse Miller
}
\seealso{
	\code{\link{coef.distfreereg}}, \code{\link{confint.distfreereg}}, \code{\link{fitted.distfreereg}}, \code{\link{formula.distfreereg}}, \code{\link{plot.distfreereg}}, \code{\link{predict.distfreereg}}, \code{\link{print.distfreereg}}, \code{\link{residuals.distfreereg}}, \code{\link{update.distfreereg}}, \code{\link{vcov.distfreereg}}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
set.seed(20240218)
n <- 1e2
func <- function(X, theta) X[,1]^theta[1] + theta[2]*X[,2]
Sig <- runif(n, min = 1, max = 3)
theta <- c(2,5)
X <- matrix(runif(2*n, min = 1, max = 5), nrow = n)
Y <- X[,1]^theta[1] + theta[2]*X[,2] + rnorm(n, sd = sqrt(Sig))
(dfr <- distfreereg(Y = Y, X = X, test_mean = func,
                    covariance = list(Sigma = Sig),
                    theta_init = c(1,1)))

func_lower <- function(x, theta) x[1]^theta[1] + theta[2]*x[2]
(dfr_lower <- distfreereg(Y = Y, X = X, test_mean = func_lower,
                          covariance = list(Sigma = Sig),
                          theta_init = c(1,1)))

identical(dfr$observed_stats, dfr_lower$observed_stats)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

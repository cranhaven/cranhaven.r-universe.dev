\name{compare}
\alias{compare}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Compare the simulated statistic distribution with the observed statistic distribution used in distribution-free parametric regression testing
}
\description{
	Simulate response data repeatedly with \code{true_mean} as the mean and \code{true_covariance} as the covariance structure, each time running \code{\link{distfreereg}} on the simulated data. The observed statistics and p-values are saved, as are the simulated statistics from the first replication.
	
	See the \href{../doc/v2_compare.html}{Comparing Distributions with the \code{distfreereg} Package} vignette for an introduction.
}
\usage{
	compare(true_mean, true_method = NULL, true_method_args = NULL, true_covariance,
	true_X = NULL, true_data = NULL, theta, n = NULL, reps = 1e3, prog = reps/10,
	err_dist_fun = rmvnorm, err_dist_args = NULL, keep = NULL, manual = NULL,
	update_args = NULL, global_override = NULL, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{true_mean}{
	Object specifying the mean structure of the true model. It is used to generate the true values of \code{Y} that are passed internally to \code{\link{distfreereg}}.
}
  \item{true_method}{
	Character vector of length one; specifies the method to use to create a model. Valid options are "\code{lm}" and "\code{nls}".
}
  \item{true_method_args}{
	Optional list; values are passed to the function specified by \code{true_method}.
}
  \item{true_covariance}{
	Named list; specifies the covariance structures of the true error distribution in the format described in the documentation for the \code{covariance} argument of \code{\link{distfreereg}}.
}
  \item{true_X, true_data}{
	Optional numeric matrix or data frame, respectively; specifies the covariate values for the true model. \code{true_X} is used when the model is specified by a function that has an \code{X} or \code{x} argument, and the \code{data} argument is used with formulas or other objects with a formula method.
}
  \item{theta}{
	Numeric vector; used as the (true) parameter values for the model with mean function \code{true_mean}.
}
  \item{n}{
	Optional integer; indicates how long each simulated data vector should be. Required only when no covariate values are specified for either the true or test mean. Silently converted to integer if numeric.
}
  \item{reps}{
	Integer; specifies number of replications. Silently converted to integer if numeric.
}
  \item{prog}{
	Integer or \code{Inf}; if finite, a progress message is given when the current repetition is a multiple of \code{prog}. Default value is \code{reps/10}, unless \code{reps} is less than 10, in which case the default is changed to \code{1}. If \code{Inf}, no prgress messages are given. Silently converted to integer if finite numeric.
}
  \item{err_dist_fun}{
	Function; specifies the function to be used to simulate errors. See details.
}
  \item{err_dist_args}{
	Optional list; specifies additional named arguments to pass to \code{err_dist_fun}.
}
  \item{keep}{
	A vector of integers, or the character string "\code{all}". If not \code{NULL}, then the output of each replication's call to \code{\link{distfreereg}} is included in the output if its repetition number is included in \code{keep}. Using \code{keep = "all"} is equivalent to \code{keep = 1:reps}.
}
  \item{manual}{
	Optional function; applied to the \code{distfreereg} object created in each iteration, whose output is saved in the list \code{manual} in the output.
}
  \item{update_args}{
	Optional named list; specifies arguments to pass to \code{\link{update.distfreereg}}.
}
  \item{global_override}{
	Optional named list; specifies arguments to pass to the \code{override} argument of \code{\link{distfreereg}} on each call to that function.
}
  \item{...}{
	Additional arguments passed to \code{\link{distfreereg}}. See details.
}
}
\details{
	This function allows the user to explore the asymptotic behavior of the distributions involved in the test conducted by \code{\link{distfreereg}}. If the sample size is sufficiently large, and assuming that the true covariance matrix of the errors is known, then the observed and simulated statistics have nearly the same distribution. How large the sample size must be depends on the details of the situation. This function can be used to determine how large the sample size must be to obtain approximately equal distributions, and to estimate the power of the test against a specific alternative.
	
	The user specifies a particular true model, comprising a mean function \code{true_mean} and an error generating function \code{err_dist_fun}, to generate the data. The user also specifies a test model, comprising a mean function \code{test_mean} and a covariance structure specified by \code{covariance}, to pass to \code{\link{distfreereg}} to test. For each repetition, \code{compare} simulates data using \code{true_mean} as the mean function and \code{err_dist_fun} to generate the errors. The covariance matrix of the errors is specified using \code{true_covariance}. (See below for more details.) This simulated data is passed as \code{Y} (or as part of \code{data}) to \code{\link{distfreereg}}.
	
	The \code{true_covariance} argument specifies the covariance structure that is available to \code{err_dist_fun} for generating errors. The needs of \code{err_dist_fun} can vary (for example, the default function uses \code{SqrtSigma} to generate multivariate normal errors), so any one of the matrices \code{Sigma}, \code{SqrtSigma}, \code{P}, and \code{Q} (defined in the documentation of \code{\link{distfreereg}}) can be specified. Any matrix needed by \code{err_dist_fun} is calculated automatically if not supplied.
	
	The value of \code{err_dist_fun} must be a function whose output is a numeric matrix with \code{n} rows and \code{reps} columns. Each column is used as the vector of errors in one repetition. The error function's arguments can include the special values \code{n}, \code{reps}, \code{Sigma}, \code{SqrtSigma}, \code{P}, and \code{Q}. These arguments are automatically assigned their corresponding values from the values passed to \code{compare}. For example, the default value \code{rmvnorm} uses \code{SqrtSigma} to generate multivariate normal values with mean 0 and covariance \code{Sigma}.
	
	The argument \code{keep} is useful for diagnosing problems, but caution should be used lest a very large object be created. It is often sufficient to save the \code{distfreereg} objects from only the first few replications.
	
	For more specialized needs, the \code{manual} argument allows the calculation and saving of objects during each repetition. For example, using \code{manual = function(x) residuals(x)} will save the (raw) residuals from each repetition.
	
	The first repetition creates a \code{distfreereg} object. During each subsequent repetition, this object is passed to \code{\link{update.distfreereg}} to create a new object. The \code{update_args} argument can be used to modify this call.
	
	If necessary, \code{global_override} can be used to pass an \code{override} argument to \code{\link{distfreereg}} in each repetition. For example, using \code{gobal_override = list(theta_hat = theta)} forces the estimated parameter vector used in the test in each call to be the true parameter vector \code{theta}.
}
\value{
	An object of class \code{compare} with the following components:
	\item{call}{The matched call.}
	\item{Y_mean}{The vector of mean values for the simulated responses.}
	\item{errors}{The matrix whose columns contain the errors used for the corresponding repetitions.}
	\item{theta}{Supplied vector of parameter values.}
	\item{true_mean}{Supplied object specifying the true mean function.}
	\item{true_covariance}{List containing element(s) that specify the true covariance structure.}
	\item{true_X}{Supplied matrix of true covariate values.}
	\item{true_data}{Supplied data frame of true covariate values.}
	\item{test_mean}{Supplied object specifying the mean function being tested.}
	\item{covariance}{List containing element(s) that specify the test covariance structure.}
	\item{X}{Supplied matrix of test covariate values.}
	\item{data}{Supplied data frame of test covariate values.}
	\item{observed_stats}{The observed statistics collected in each repetition.}
	\item{mcsim_stats}{The simulated statistics from the first repetition. (They are the same for each repetition, because \code{compare} uses \code{\link{update.distfreereg}}.)}
	\item{p}{The p-values for the observed statistics.}
	\item{dfrs}{A list containing the outputs of \code{\link{distfreereg}} for repetitions specified in \code{keep}. Included when \code{keep} is not \code{NULL}.}
	\item{manual}{A list containing the results of the function specified by the argument \code{manual}. Included when \code{manual} argument is not \code{NULL}.}
}
\author{
	Jesse Miller
}


\note{
	Some of the processing of the elements of \code{true_covariance} is analogous to the processing of \code{covariance} by \code{\link{distfreereg}}. Any values of \code{solve_tol} and \code{symmetric} specified in \code{\link{distfreereg}}'s \code{control} argument are used by \code{compare} to similar effect in processing \code{true_covariance}.

  The presence of \code{call} in the value allows a \code{compare} object to be passed to \code{\link{update}}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{\link{distfreereg}}, \code{\link{rejection}}, \code{\link{plot.compare}}, \code{\link{ks.test.compare}}
}
\examples{
set.seed(20240201)
n <- 100
func <- function(X, theta) theta[1] + theta[2]*X
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5)
X <- matrix(rexp(n, rate = 1))
# In practice, 'reps' should be much larger
cdfr <- compare(true_mean = func, true_X = X, true_covariance = list(Sigma = Sig),
                test_mean = func, X = X, covariance = list(Sigma = Sig),
                reps = 10, prog = Inf, theta = theta, theta_init = rep(1, length(theta)))

cdfr$p
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

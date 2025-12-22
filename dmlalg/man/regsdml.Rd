% Please edit documentation in R/regsdml.R
\name{regsdml}
\alias{regsdml}
\alias{regdml}
\title{Estimating linear coefficients with double machine learning (DML)}
\usage{
regsdml(
  a, w, x, y, data = NULL,
  DML = c("DML2", "DML1"),
  K = 2L,
  gamma = exp(seq(-4, 10, length.out = 100)),
  aN = NULL,
  do_regsDML = TRUE,
  do_safety = FALSE,
  do_DML = do_regDML || do_regsDML || do_safety,
  do_regDML = FALSE,
  do_regDML_all_gamma = FALSE,
  safety_factor = 0.7,
  cond_method = rep("spline", 3),
  params = NULL,
  level = 0.95,
  S = 100L,
  parallel = c("no", "multicore", "snow"),
  ncpus = 1L,
  cl = NULL
)
}
\arguments{
\item{a}{A vector, matrix, or data frame. It acts as an instrument after
    regressing out \code{w} of it.
    Alternatively, if the data is
    provided in the data frame \code{data}, \code{a} is a character vector
    whose entries specify the columns of \code{data} acting as
    "instrument" \eqn{A}.}

\item{w}{A vector, matrix, or data frame. Its columns contain observations
    of the nonlinear predictors. Alternatively, if the data is
    provided in the data frame \code{data}, \code{w} is a character vector
    whose entries specify the columns of \code{data} acting as \eqn{W}.}

\item{x}{A vector, matrix, or data frame. This is the linear predictor.
    Alternatively, if the data is provided in the data frame
    \code{data}, \code{x} is a character vector whose entries specify
    the columns of \code{data} acting as \eqn{X}.}

\item{y}{A vector, matrix, or data frame. This is the response.
    Alternatively, if the data is provided in the data frame
    \code{data}, \code{y} is a character vector whose entries specify
    the columns of \code{data} acting as \eqn{Y}.}

\item{data}{An optional data frame. If it is specified, its column names
    need to coincide with the character vectors specified in \code{a},
    \code{w}, \code{x}, and \code{y}.}

\item{DML}{Either \code{"DML2"} or \code{"DML1"} depending on which DML
    method should be used. The default is \code{"DML2"}.}

\item{K}{The number of sample splits used for cross-fitting.}

\item{gamma}{A vector specifying the grid of regularization parameters
    over which to optimize.}

\item{aN}{The \eqn{N}th element of a sequence of non-negative
    real numbers diverging to \eqn{+ \infty} as the sample size
    \eqn{N} tends to \eqn{+ \infty}. By default, it equals
    \code{max(log(sqrt(N)), 1)}, where \code{N} denotes the sample size.}

\item{do_regsDML}{A boolean that specifies whether the regsDML estimator
    is computed. It is set to \code{TRUE} by default.}

\item{do_safety}{A boolean that specifies whether a safety device is employed.
    The safety device chooses the regularization parameter \eqn{\gamma}
    such that the variance of the regularized estimator
    is at least (100 * \code{safety_factor})\% of the variance of standard DML.}

\item{do_DML}{A boolean that specifies whether the standard DML estimator is
    computed. It is set to \code{TRUE} by default if at least one of
    \code{do_regsDML}, \code{do_safety}, or \code{do_regDML} is set to
    \code{TRUE}.}

\item{do_regDML}{A boolean that specifies whether the regularized DML
    estimator regDML with the regularization parameter equal to \code{a_N}
    times the \eqn{\gamma} leading to the lowest mean
    squared error is computed. It is set to \code{FALSE} by default.}

\item{do_regDML_all_gamma}{A boolean that specifies whether the regularized
    estimators for all values \eqn{\gamma} of the grid \code{gamma} are
    returned. It is set to \code{FALSE} by default.}

\item{safety_factor}{The factor of the safety method. It is set to \code{0.7}
    by default.}

\item{cond_method}{A character vector of length 3 specifying the estimation
    methods used to fit the conditional
    expectations \eqn{E[A|W]}, \eqn{E[X|W]}, and \eqn{E[Y|W]}.
    Its components are from
    from \code{"spline"}, \code{"forest"},
    \code{"ols"}, \code{"lasso"}, \code{"ridge"}, and \code{"elasticnet"},
    or it is a list of length 3 with components from \code{"spline"},
    \code{"forest"},
    \code{"ols"}, \code{"lasso"}, \code{"ridge"}, and \code{"elasticnet"},
    and where some components of the list are functions to estimate
    the conditional expectations.
    These functions have the input arguments
    \code{(yy_fit, ww_fit, ww_predict, params = NULL)} and output the
    conditional expectation of \eqn{E[Y|W]} estimated with \code{yy_fit}
    and \code{ww_fit} and predicted with \code{ww_predict}.
    The argument \code{params} is described below. The functions
    return a matrix where the columns correspond to the component-wise
    estimated conditional expectations.
    Here, \code{yy} symbolically stands for either \code{a},
    \code{x}, or \code{y}.
    Please see below for the default arguments
    of the \code{"spline"}, \code{"forest"}, \code{"ols"}, \code{"lasso"},
    \code{"ridge"}, and \code{"elasticnet"} methods.}

\item{params}{An optional list of length 3. All 3 elements of this list
    are lists themselves. These lists specify additional input arguments for
    estimating the conditional expectations \eqn{E[A|W]}, \eqn{E[X|W]},
    and \eqn{E[Y|W]}, respectively.}

\item{level}{Level for computing
    confidence intervals for testing the two-sided component-wise
    null hypotheses that test if a component equals zero
    with the (approximate) asymptotic Gaussian distribution. The default is
    \code{0.95}.}

\item{S}{Number of replications to correct for the random
    splitting of the sample. It is set to \code{100L} by default.}

\item{parallel}{One out of \code{"no"}, \code{"multicore"}, or \code{"snow"}
    specifying the parallelization method used to compute the \code{S}
    replications. The default is \code{"no"}.}

\item{ncpus}{An integer specifying the number of cores used if
    \code{parallel} is not set to \code{"no"}.}

\item{cl}{An optional parallel or snow cluster if \code{parallel = "snow"}.
    The argument \code{ncpus} does not have to be specified if the
    argument \code{cl}
    is specified.}
}

\value{
A list containing some of the lists
    \code{regsDML_statistics},
    \code{regDML_safety_statistics},
    \code{DML_statistics}, \code{regDML_statistics}, and
    \code{regDML_all_gamma_statistics} is returned.
    The individual sublists contain the following arguments supplemented
    by an additional suffix specifying the method they correspond to.

\item{\code{beta}}{Estimator of the linear coefficient \eqn{\beta_0}.}

\item{\code{sd}}{Standard error estimates of the respective entries
    of \code{beta}.}

\item{\code{var}}{Variance-covariance matrix of \code{beta}.}

\item{\code{pval}}{p-values for the respective entries of \code{beta}.}

\item{\code{CI}}{Two-sided confidence intervals
    for \eqn{\beta_0} where the \eqn{j}th row of \code{CI}
    corresponds to the two-sided testing of \eqn{H_0: (\beta_0)_j=0}
    at level \code{level}. They are computed with the (approximate) asymptotic
    Gaussian distribution of the coefficient estimates.}

The list \code{regsDML_statistics} contains the following additional entries:

\item{\code{message_regsDML}}{Specifies if regsDML selects
    the regularized estimator or DML.}

\item{\code{gamma_aN}}{Chosen optimal regularization parameter if
    regsDML equals the regularized estimator.
    This entry is not present if DML is selected.}

If the safety device is applicable, the list \code{regDML_safety_statistics}
    contains the following additional entries:

\item{\code{message_safety}}{Specifies whether the safety device was
    applicable.}

\item{\code{gamma_safety}}{Chosen regularization parameter of the
    safety device.}

If the safety device is not applicable, the list
    \code{regDML_safety_statistics} contains \code{message_safety}
    as its only entry.

The list \code{regDML_statistics} contains the
    following additional entry:

\item{\code{gamma_opt}}{Chosen optimal regularization parameter.}

The list \code{regDML_all_gamma_statistics} is a list of the same
    length as the grid \code{gamma}, where each individual list is of the
    structure just described.
}

\description{
Our goal is to perform inference for the linear parameter in partially
    linear models with confounding variables.
    The standard double machine learning (DML) estimator of the linear
    parameter has a two-stage least
    squares interpretation, which can lead to a large variance and overwide
    confidence intervals.
    We apply regularization to reduce the variance of the estimator,
    which produces narrower confidence intervals that remain approximately valid.

The function \code{regsdml} estimates the linear parameter \eqn{\beta_0}
    in the partially linear model
    \deqn{Y = X^T\beta_0 + g(W) + h(H) + \epsilon_Y}
    of the continuous response \eqn{Y}
    with linear covariates
    \eqn{X}, nonlinear covariates \eqn{W},  unobserved confounding
    variables \eqn{H}, and the error term \eqn{\epsilon_Y}. An additional
    variable \eqn{A} is required that is not part of the right-hand side
    defining \eqn{Y}. The variable \eqn{A} acts as an instrument after
    \eqn{W} is regressed out of it.

The linear parameter \eqn{\beta_0} can be estimated with
    a two-stage least squares (TSLS) approach ("standard" DML) or with
    regularized approaches (regDML, regsDML).
    All approaches use double
    machine learning.
    The TSLS approach regresses the residual \eqn{Y - E[Y|W]} on
    \eqn{X - E[X|W]} using the instrument \eqn{A - E[A|W]}.
    The regularized approaches
    minimize an objective function that equals \eqn{\gamma} times
    the objective function
    of TSLS plus an objective function that partials out \eqn{A - E[A|W]}
    from the residual quantity \eqn{Y - E[Y|W] - (X - E[X|W])^T\beta}.
    The different regularization approaches choose different regularization
    parameters \eqn{\gamma}.
    The conditional expectations act as nuisance parameters and are estimated
    with machine learning algorithms.
    All approaches use sample splitting and cross-fitting to
    estimate \eqn{\beta_0}.
}

\details{
The estimator of \eqn{\beta_0} is computed using sample splitting and
    cross-fitting.
    Irrespective of which methods are performed,
    the data is split into \code{K} sets that are equally large
    if possible. For each such set, the nuisance parameters
    (that is, the conditional expectations \eqn{E[A|W]}, \eqn{E[X|W]},
    and \eqn{E[Y|W]}) are estimated on its complement and evaluated on the
    set itself. If \code{DML = "DML1"}, then \code{K} individual
    estimators are computed for each
    of the \code{K} data sets and are then averaged. If \code{DML = "DML2"},
    the nuisance parameter matrices are first assembled before the estimator
    of \eqn{\beta_0} is computed. This enhances stability of the coefficient
    estimator compared to \code{"DML1"}. If \code{K = 1}, no sample splitting
    is performed. In this case, the nuisance parameters are estimated and
    predicted on the full sample.

The whole estimation procedure can be repeated \code{S} times to
    account for the randomness introduced by the random sample splits.
    The \code{S} repetitions can be run in parallel by specifying the
    arguments \code{parallel} and \code{ncpus}.
    The \code{S} estimators of \eqn{\beta_0} are aggregated by taking the
    median of them. The \code{S} variance-covariance matrices are aggregated
    by first adding a correction term to them that accounts for the random
    splitting and by afterwards taking the median of the corrected
    variance-covariance matrices. If \eqn{d > 1}, it can happen that this
    final matrix is not positive definite anymore, in which case the mean
    is considered instead.

If the design in at least \code{0.5 * S} of the \code{S} repetitions is
    singular, an error message is displayed.
    If the designs in some but less than \code{0.5 * S} of the \code{S}
    repetitions are singular, another \code{S} repetitions are performed.
    If, in total, at least \code{S} repetitions result in a nonsingular design,
    the results are returned together with a warning message.

The regularized estimators and their associated mean squared errors
    (MSEs) are computed for the regularization parameters \eqn{\gamma} of
    the grid \code{gamma}. These estimators are returned if the argument
    \code{do_regDML_all_gamma} is set to \code{TRUE}.
    The \eqn{\gamma}-value whose corresponding regularized estimator
    from the \code{do_regDML_all_gamma} method achieves
    the smallest MSE
    is multiplied by \code{aN}, leading to \eqn{\gamma'}.
    The \code{do_regDML_all_gamma} estimator with regularization parameter
    \eqn{\gamma'} is called regDML.
    The regsDML estimator equals regDML or DML depending on whose variance
    is smaller.
    If \eqn{\beta_0} is of larger dimension than 1, the MSE computations and
    the variance comparison step are performed with the sum of the diagonal
    entries of the respective variance-covariance matrices.

If \code{do_safety = TRUE}, a \eqn{\gamma} value is chosen such that the
    regularized estimator among \code{do_regDML_all_gamma}
    with this value of \eqn{\gamma} has a variance
    that is just not smaller than \code{safety_factor} times the variance of
    DML.
    If \eqn{\beta_0} is of larger dimension than 1, the sum of the diagonal
    entries of the respective variance-covariance matrices is taken as
    a measure of variance.
    If the regularization scheme leads to considerable variance
    reductions, it is possible that this safety device cannot be applied.
    In this case, a respective message is returned.

The default options of the \code{"spline"}, \code{"forest"},
    \code{"ols"}, \code{"lasso"}, \code{"ridge"}, and \code{"elasticnet"}
    methods are as follows. With the \code{"spline"} method,
    the function \code{bs} from the package \code{splines} is employed
    with \code{degree = 3} and \code{df = ceiling(N ^ (1 / 5)) + 2} if
    \code{N} satisfies \code{(df + 1) * v + 1 > N}, where \code{v} denotes
    the number of columns of \code{w} and \code{N} denotes the sample size.
    Otherwise, \code{df} is consecutively
    reduced by \code{1} until this condition is satisfied.
    The splines are fitted and predicted on different data sets.
    If they are extrapolated, a warning message is displayed.
    With the \code{"forest"} method, the function \code{randomForest} from
    the package \code{randomForest} is employed with \code{nodesize = 5},
    \code{ntree = 500}, \code{na.action = na.omit}, and \code{replace = TRUE}.
    With the \code{"ols"} method, the default arguments are used and no
    additional arguments are specified.
    With the \code{"lasso"} and \code{"ridge"} methods,
    the function \code{cv.glmnet} from the package \code{glmnet} performs
    10-fold cross validation by default (argument \code{nfolds})
    to find the one-standard-error-rule \eqn{\lambda}-parameter.
    With the \code{"elasticnet"} method, the function \code{cv.glmnet} from
    the package \code{glmnet} performs 10-fold cross validation
    (argument \code{nfolds}) with \code{alpha = 0.5} by default
    to find the one-standard-error-rule \eqn{\lambda}-parameter.
    All default values of the mentioned parameters can be adapted by
    specifying the argument \code{params}.

There are three possibilities to set the argument \code{parallel}, namely
    \code{"no"} for serial evaluation (default),
    \code{"multicore"} for parallel evaluation using forking,
    and \code{"snow"} for parallel evaluation using a parallel
    socket cluster. It is recommended to select \link[base]{RNGkind}
    (\code{"L'Ecuyer-CMRG"}) and to set a seed to ensure that the parallel
    computing of the package \code{dmlalg} is reproducible.
    This ensures that each processor receives a different substream of the
    pseudo random number generator stream.
    Thus, the results reproducible if the arguments remain unchanged.
    There is an optional argument \code{cl} to specify a custom cluster
    if \code{parallel = "snow"}.

The response \code{y} needs to be continuous.
    The covariate \code{w} may contain factor variables in its columns.
    If the variables \code{a} and \code{x} contain factor variables,
    the factors should not be included as factor columns of \code{a} or
    \code{x}.
    Instead, dummy encoding should be used for all individual levels of the
    factor.
    That is, a factor with 4 levels should be encoded with 4 columns where each
    column consists of 1 and 0 entries indicating the presence of the
    respective level of the factor.

There are \code{summary}, \code{confint}, \code{coef}, \code{vcov},
    and \code{print} methods available
    for objects fitted with \code{regsdml}. They are called
    \code{\link{summary.regsdml}},
    \code{\link{confint.regsdml}},
    \code{\link{coef.regsdml}},
    \code{\link{vcov.regsdml}}, and
    \code{\link{print.regsdml}}, respectively.
}

\examples{
## Generate some data:
RNGkind("L'Ecuyer-CMRG")
set.seed(19)
# true linear parameter
beta0 <- 1
n <- 40
# observed confounder
w <- pi * runif(n, -1, 1)
# instrument
a <- 3 * tanh(2 * w) + rnorm(n, 0, 1)
# unobserved confounder
h <- 2 * sin(w) + rnorm(n, 0, 1)
# linear covariate
x <- -1 * abs(a) - h - 2 * tanh(w) + rnorm(n, 0, 1)
# response
y <- beta0 * x - 3 * cos(pi * 0.25 * h) + 0.5 * w ^ 2 + rnorm(n, 0, 1)

## Estimate the linear coefficient from x to y
## (The parameters are chosen small enough to make estimation fast):
## Caveat: A spline estimator is extrapolated, which raises a warning message.
## Extrapolation lies in the nature of our method. To omit the warning message
## resulting from the spline estimator, another estimator may be used.
fit <- regsdml(a, w, x, y,
               gamma = exp(seq(-4, 1, length.out = 4)),
               S = 3,
               do_regDML_all_gamma = TRUE,
               cond_method = c("forest",  # for E[A|W]
                               "spline",  # for E[X|W]
                               "spline"), # for E[Y|W]
               params = list(list(ntree = 1), NULL, NULL))
## parm = c(2, 3) prints an additional summary for the 2nd and 3rd gamma-values
summary(fit, parm = c(2, 3),
        correlation = TRUE,
        print_gamma = TRUE)
confint(fit, parm = c(2, 3),
        print_gamma = TRUE)
coef(fit) # coefficients
vcov(fit) # variance-covariance matrices

## Alternatively, provide the data in a single data frame
## (see also caveat above):
data <- data.frame(a = a, w = w, x = x, y = y)
fit <- regsdml(a = "a", w = "w", x = "x", y = "y", data = data,
               gamma = exp(seq(-4, 1, length.out = 4)),
               S = 3)

## With more realistic parameter choices:
if (FALSE) {
  fit <- regsdml(a, w, x, y,
                 cond_method = c("forest",  # for E[A|W]
                                 "spline",  # for E[X|W]
                                 "spline")) # for E[Y|W]
  summary(fit)
  confint(fit)

  ## Alternatively, provide the data in a single data frame:
  ## (see also caveat above):
  data <- data.frame(a = a, w = w, x = x, y = y)
  fit <- regsdml(a = "a", w = "w", x = "x", y = "y", data = data)
}
}

\references{
C. Emmenegger and P. \enc{Bühlmann}{Buehlmann}.
Regularizing Double Machine Learning in Partially Linear Endogenous
Models, 2021. Preprint arXiv:2101.12525.
}

\seealso{
\code{\link{summary.regsdml}},
    \code{\link{confint.regsdml}},
    \code{\link{coef.regsdml}},
    \code{\link{vcov.regsdml}}
    \code{\link{print.regsdml}}
}

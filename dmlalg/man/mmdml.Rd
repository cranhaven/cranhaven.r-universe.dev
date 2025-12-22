\name{mmdml}
\alias{mmdml}
\alias{dmlmixed}
\alias{mixed_dml}
\alias{dml_mixed}
\title{Estimating linear coefficients in partially linear mixed-effects models with repeated measurements using double machine learning.}
\usage{
mmdml(
  w, x, y, z, data = NULL,
  z_formula = NULL, group = "group",
  K = 2L, S = 100L,
  cond_method = rep("forest", 2),
  params = NULL,
  parallel = c("no", "multicore", "snow"),
  ncpus = 1L, cl = NULL,
  nr_random_eff = if (S > 5) 1L else S,
  nr_res = nr_random_eff
)
}
\arguments{
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

\item{z}{A vector, matrix, or data frame. It acts as the fixed matrix
    assigning the random effects.
    Alternatively, if the data is
    provided in the data frame \code{data}, \code{z} is a character vector
    whose entries specify the columns of \code{data} acting as \eqn{Z}.}

\item{z_formula}{A string specifying the structure of the random effect
    using the notation as in \code{\link[lme4]{lmer}}
    from package \CRANpkg{lme4}, e.g.,
    \code{(1|id) + (1|cask:id)}.}

\item{group}{A string containing the name of the grouping variable in
    \code{zz}.}

\item{data}{An optional data frame. If it is specified, its column names
    need to coincide with the character vectors specified in \code{a},
    \code{w}, \code{x}, and \code{y}.}

\item{K}{The number of sample splits used for cross-fitting.}

\item{S}{Number of replications to correct for the random
    splitting of the sample. It is set to \code{100L} by default.}

\item{cond_method}{A character vector of length 2 specifying the estimation
    methods used to fit the conditional
    expectations \eqn{E[X|W]} and \eqn{E[Y|W]}.
    Its components are from
    from \code{"spline"}, \code{"forest"},
    \code{"ols"}, \code{"lasso"}, \code{"ridge"}, and \code{"elasticnet"},
    or it is a list of length 2 with components from \code{"spline"},
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
    Here, \code{yy} symbolically stands for either
    \code{x} or \code{y}.
    Please see below for the default arguments
    of the \code{"spline"}, \code{"forest"}, \code{"ols"}, \code{"lasso"},
    \code{"ridge"}, and \code{"elasticnet"} methods.}

\item{params}{An optional list of length 2. The 2 elements of this list
    are lists themselves. These lists specify additional input arguments for
    estimating the conditional expectations \eqn{E[X|W]}
    and \eqn{E[Y|W]}, respectively.}

\item{parallel}{One out of \code{"no"}, \code{"multicore"}, or \code{"snow"}
    specifying the parallelization method used to compute the \code{S}
    replications. The default is \code{"no"}.}

\item{ncpus}{An integer specifying the number of cores used if
    \code{parallel} is not set to \code{"no"}.}

\item{cl}{An optional parallel or snow cluster if \code{parallel = "snow"}.
    The argument \code{ncpus} does not have to be specified if the
    argument \code{cl}
    is specified.}

\item{nr_random_eff}{An integer specifying the number of unaggregated sets
    of random effect estimates among the \code{S} repetitions that should be
    returned.}

\item{nr_res}{An integer specifying the number of unaggregated sets
    of residual estimates among the \code{S} repetitions that should be
    returned.}
}

\value{
A list similar to the output of \code{\link[lme4]{lmer}}
    from package \CRANpkg{lme4} containing
    the following entries.

\item{\code{beta}}{Estimator of the linear coefficient \eqn{\beta_0}.}

\item{\code{vcov}}{Variance-covariance matrix of \code{beta}.
    Also see \code{\link[lme4]{lmer}}. The \code{S} individual variance-covariance
    matrices are aggregated by first adding a correction term to them
    correcting for the randomness of the sample splits and by subsequently
    taking the median of the corrected variance-covariance matrices.}

\item{\code{sigma}}{Please see \code{\link[lme4]{lmer}} for its meaning.
    It is computed by averaging over the \code{K} sample splits and by
    aggregating the \code{S} repetitions using the median.}

\item{\code{theta}}{Please see \code{\link[lme4]{lmer}} for its meaning.
    It is computed by averaging over the \code{K} sample splits and by
    aggregating the \code{S} repetitions using the median.}

\item{\code{varcor}}{Variance correlation components computed with
    \code{theta}. Please also see \code{\link[lme4]{lmer}}.}

\item{\code{random_eff}}{Conditional estimates of the random effects
    similarly to \code{\link[lme4]{lmer}}.
    The individual sets of \code{S} random effects estimates are aggregated
    using the mean.}

\item{\code{random_eff_all}}{The first \code{nr_random_eff} sets of the
    \code{S} sets of random effects estimates.}

\item{\code{residuals}}{The first \code{nr_res} sets of the
    \code{S} sets of residuals.
    Each set of residuals is computed with parameters and data that is
    aggregated over the \code{K} sample splits.}

The other elements \code{ngrps}, \code{nobs}, \code{fitMsgs}, \code{cnms},
    \code{nc}, \code{nms}, \code{useSc}, \code{optinfo}, and \code{methTitle}
    are as in \code{\link[lme4]{lmer}}.
    The gradient and Hessian information of \code{optinfo} is computed
    by aggregating the respective information over the \code{S} repetitions
    with the median.
}

\description{
Our goal is to perform inference for the linear parameter in partially
    linear mixed-effects models (PLMMs) with
    repeated measurements using double machine learning (DML).

The function \code{mmdml} estimates the linear parameter \eqn{\beta_0}
    in the PLMM
    \deqn{Y_i = X_i\beta_0 + g(W_i) + Z_ib_i + \epsilon_{Y_i},
    (i = 1, \ldots, N)}
    for the continuous response \eqn{Y_i}
    with linear covariates
    \eqn{X_i}, nonlinear covariates \eqn{W_i},  unobserved random effects
    \eqn{b_i}, and the error term \eqn{\epsilon_{Y_i}}.
    For each \eqn{i}, there are \eqn{n_i} repeated observations available.
    That is, \eqn{Y_i} is an \eqn{n_i}-dimensional vector.
    The matrix \eqn{Z_i} is fixed. The random effects \eqn{b_i} and the
    error terms \eqn{\epsilon_{Y_i}} are Gaussian distributed, independent,
    and independent of \eqn{b_j} and \eqn{\epsilon_{Y_j}}, respectively,
    for \eqn{i\neq j}. The linear and nonlineare covariates \eqn{X_i} and
    \eqn{W_i} are random and independent of all random effects and error terms.

The linear parameter \eqn{\beta_0} can be estimated with
    a linear mixed-effects modeling approach with maximum likelihood
    after regressing out \eqn{W_i}
    nonparametrically from \eqn{Y_i} and \eqn{X_i} using machine learning
    algorithms.
    A linear mixed-effects model is estimated on the partialled-out data
    \deqn{Y_i - E[Y_i|W_i] = (X_i - E[X_i|W_i])\beta_0 + Z_ib_i
    + \epsilon_{Y_i}.}
    The conditional expectations are estimated with machine learning algorithms
    and sample splitting, and cross-fitting is used to regain full efficiency
    of the estimator of \eqn{beta_0}. This estimator
    is asymptotically Gaussian distributed and efficient.
}

\details{
The estimator of \eqn{\beta_0} is computed using sample splitting and
    cross-fitting.
    The subject-specific data (over \eqn{i = 1, \ldots, N}) is split
    into \code{K} sets that are equally large
    if possible. For each such set, the nuisance parameters
    (that is, the conditional expectations \eqn{E[Y_i|W_i]} and
    \eqn{E[X_i|W_i]}) are estimated on its complement and evaluated on the
    set itself.
    Estimators of \eqn{\beta_0} and the variance parameters are computed
    for each
    of the \code{K} data sets and are then averaged.
    If \code{K = 1}, no sample splitting
    is performed. In this case, the nuisance parameters are estimated and
    predicted on the full sample.

The whole estimation procedure is repeated \code{S} times to
    account for the randomness introduced by the random sample splits.
    The \code{S} repetitions can be run in parallel by specifying the
    arguments \code{parallel} and \code{ncpus}.
    The \code{S} estimators of \eqn{\beta_0} and the variance components
    are aggregated by taking the
    median of them. The \code{S} variance-covariance matrices of the estimator
    of \eqn{\beta_0} are aggregated
    by first adding a correction term to them that accounts for the random
    splitting and by afterwards taking the median of the corrected
    variance-covariance matrices. If \eqn{d > 1}, it can happen that this
    final matrix is not positive definite anymore, in which case the mean
    is considered instead.
    Estimates of the conditional random effects and the residuals are computed
    in each of the \code{S} repetitions. A total number of \code{nr_random_eff}
    and \code{nr_res} of them, respectively, is returned.
    Additionally, the random effects estimates from all \code{S} repetitions
    are aggregated using the mean and returned.

If the design in at least \code{0.5 * S} of the \code{S} repetitions is
    singular, an error message is displayed.
    If the designs in some but less than \code{0.5 * S} of the \code{S}
    repetitions are singular, another \code{S} repetitions are performed.
    If, in total, at least \code{S} repetitions result in a nonsingular design,
    the results are returned together with a warning message.

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
    socket cluster. It is recommended to select \code{\link[base]{RNGkind}}
    (\code{"L'Ecuyer-CMRG"}) and to set a seed to ensure that the parallel
    computing of the package \code{dmlalg} is reproducible.
    This ensures that each processor receives a different substream of the
    pseudo random number generator stream.
    Thus, the results reproducible if the arguments remain unchanged.
    There is an optional argument \code{cl} to specify a custom cluster
    if \code{parallel = "snow"}.

The response \code{y} needs to be continuous.
    The covariate \code{w} may contain factor variables in its columns.
    If the variable \code{x} contains factor variables,
    the factors should not be included as factor columns of \code{x}.
    Instead, dummy encoding should be used for all individual levels of the
    factor.
    That is, a factor with 4 levels should be encoded with 4 columns where each
    column consists of 1 and 0 entries indicating the presence of the
    respective level of the factor.

There are \code{confint}, \code{fixef}, \code{print}, \code{ranef},
    \code{residuals}, \code{sigma}, \code{summary}, \code{vcov},
    and \code{VarCorr} methods available
    for objects fitted with \code{mmdml}. They are called
    \code{\link{confint.mmdml}},
    \code{\link{fixef.mmdml}},
    \code{\link{print.mmdml}},
    \code{\link{ranef.mmdml}},
    \code{\link{residuals.mmdml}},
    \code{\link{sigma.mmdml}},
    \code{\link{summary.mmdml}},
    \code{\link{vcov.mmdml}}, and
    \code{\link{VarCorr.mmdml}}, respectively.
}

\examples{
## generate data
RNGkind("L'Ecuyer-CMRG")
set.seed(19)
data1 <- example_data_mmdml(beta0 = 0.2)
data2 <- example_data_mmdml(beta0 = c(0.2, 0.2))

## fit models
## Caveat: Warning messages are displayed because the small number of
## observations results in a singular random effects model
fit1 <-
  mmdml(w = c("w1", "w2", "w3"), x = "x1", y = "resp", z = c("id", "cask"),
        data = data1, z_formula = "(1|id) + (1|cask:id)", group = "id", S = 3)

fit2 <-
  mmdml(w = c("w1", "w2", "w3"), x = c("x1", "x2"), y = "resp", z = c("id", "cask"),
        data = data2, z_formula = "(1|id) + (1|cask:id)", group = "id", S = 3)

## apply methods
confint(fit2)
fixef(fit2)
print(fit2)
ranef(fit2)
residuals(fit2)
sigma(fit2)
summary(fit2)
vcov(fit2)
VarCorr(fit2)
}

\references{
C. Emmenegger and P. \enc{Bühlmann}{Buehlmann}.
Double Machine Learning for Partially Linear Mixed-Effects Models with Repeated Measurements. Preprint arXiv:2108.13657.
}

\seealso{
\code{\link{confint}},
    \code{\link{fixef}},
    \code{\link{print}},
    \code{\link{ranef}},
    \code{\link{residuals}},
    \code{\link{sigma}},
    \code{\link{summary}},
    \code{\link{vcov}},
    \code{\link{VarCorr}}
}

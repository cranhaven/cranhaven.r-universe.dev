
\name{stable_glm}
\alias{stable_glm}
\title{Generalized linear-model fitting with Stable residuals}
\usage{
stable_glm(y_name, formulas, data, output_se, calc_confbounds, conf, trace, optim_control)
}
\arguments{
    \item{y_name}{character string denoting column name in input data, containing dependent variable}
    \item{formulas}{list for formulas for the regression. See Details}
    \item{data}{data.frame}
    \item{output_se}{logical - whether to calculate standard errors (will entail calculation and inversion of final Hessian)}
    \item{calc_confbounds}{logical - whether to calculate exact confidence bounds. See Details}
    \item{conf}{confidence level for confidence bounds. Default 0.95}
    \item{trace}{trace level}
    \item{optim_control}{list passed to \code{nlminb}'s control argument}
}
\description{
    Fitting of a generalized linear model with stable residuals. Allows for regression formulas for all 4 parameters of the stable distribution. NAs not allowed.
}
\details{
    Lowest possible estimate for alpha is 1.1.

    The \code{formulas} argument is a list of formulas, where each name in the list is either "loc", "scale", "beta", or "alpha". For each of these, a formula is supplied without any dependent variable. E.g. in a regression for the alpha parameter the \code{formulas} list will look like \code{ formulas = list( alpha = formula(~ x1 + x2) ) }

    By default, \code{output_se = TRUE}, which will calculate standard errors and approximate confidence bounds. These will be approximate in the sense that the likelihood will typically not be symmetric, and thus these confidence bounds will only provide an indicative measure of confidence. To calculate exact confidence bounds use \code{calc_confbounds = TRUE}. However, exact confidence bounds calculation is very time consuming as this requires repeated computation of profile likelihoods for each regression variable. If N is the number of regressors in the model, then the expected time to compute all confidence bounds is about 200N times the time it takes to just fit the model. Currently, confidence bounds are calculated only for the regressors and not the parameters of the distribution of the residuals (i.e. alpha, beta, and scale).

    Note that standard errors and t-scores for alpha, beta, and scale parameters are calculated in transformed space and cannot be interpreted as-is. All confidence bounds (both approximate and exact), however, are transformed back to the original space and can be interpreted as-is.

    In the case that alpha is estimated to be numerically equal to 2, beta is automatically set to 0. In this boundary case calculation of standard errors will fail and return the default maximum range for alpha, i.e. 1.1 to 2. This should be interpreted as the residuals having a Normal distribution, in which case OLS regression would be preferable.
}
\examples{
# generate some data: y = 4x - 1 + epsilon
# where epsilon is heavy-tailed student-t with 5 df
set.seed(123)
df <- data.frame(x = rnorm(500))
df$y <- 4 * df$x - 1 + rt(500, df = 5)

# regress both location and skew:
formulas <- list(
    "loc" = formula( ~ x),
    "beta" = formula( ~ x)
)

sfit <- stable_glm("y", formulas, data = df)
print(sfit)
}

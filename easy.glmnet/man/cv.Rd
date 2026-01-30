\name{cv}
\alias{cv}
\title{Conduct cross-validation}
\description{Function to easily cross-validate (including fold assignation, merging fold outputs, etc).}
\usage{cv(x, y, family = c("binomial", "cox", "gaussian"), fit_fun, predict_fun, site = NULL,
covar = NULL, nfolds = 10, pred.format = NA, verbose = TRUE, ...)}
\arguments{
  \item{x}{input matrix for glmnet of dimension nobs x nvars; each row is an observation vector. It can be easily obtained with \code{\link{data.frame2glmnet.matrix}}.}
  \item{y}{response to be predicted. A binary vector for "binomial", a "Surv" object for "cox", or a numeric vector for "gaussian".}
  \item{family}{distribution of y: "binomial", "cox", or "gaussian".}
  \item{fit_fun}{function to create the prediction model using the training subsets. It can have between two and four arguments(the first two are compulsory): \code{x_training} (training X data.frame), \code{y_training} (training Y outcomes), \code{site_training} (training site names), and \code{covar_training} (training covariates). It must return the overall prediction model, which may be a list of the different submodels used in different steps and/or derived from different imputations.}
  \item{predict_fun}{function to apply the prediction model to the test sets. It can have between two and four arguments (the first two are compulsory): \code{model} (the overall prediction model), \code{x_test} (test X data.frame), \code{site_test} (test site names), and \code{covar_test} (test covariates). It must return the predictions.}
  \item{site}{vector with the sites' names, or NULL for studies conducted in a single site.}
  \item{covar}{other covariates that can be passed to fit_fun and predict_fun.}
  \item{\dots}{other arguments that can be passed to fit_fun and predict_fun.}
  \item{nfolds}{number of folds, only used if \code{folds} is NULL.}
  \item{pred.format}{format of the predictions returned by each fold. E.g., if the prediction is an array, use NA.}
  \item{verbose}{(optional) logical, whether to print some messages during execution.}
}
\details{This function iteratively divides the dataset into a training dataset, with which fits the model using the function \code{fit_fun}, and a test dataset, to which applies the model using the function \code{predict_fun}. It saves the models fit with the training datasets and the predictions obtained in the test datasets. The fols are assigned automatically using \code{\link{assign.folds}}, accounting for the \code{site} is this is not null.}
\value{A list with the predictions and the models used.}
\author{Joaquim Radua}
\seealso{
  \code{\link{glmnet_predict}} for obtaining predictions.
}
\examples{
# Create random x (predictors) and y (binary)
x = matrix(rnorm(25000), ncol = 50)
y = 1 * (plogis(apply(x[,1:5], 1, sum) + rnorm(500, 0, 0.1)) > 0.5)

# Predict y via cross-validation
fit_fun = function (x_training, y_training) {
  list(
    lasso = glmnet_fit(x_training, y_training, family = "binomial")
  )
}
predict_fun = function (m, x_test) {
  glmnet_predict(m$lasso, x_test)
}
# Only 2 folds to ensure the example runs quickly
res = cv(x, y, family = "binomial", fit_fun = fit_fun, predict_fun = predict_fun, nfolds = 2)

# Show accuracy
se = mean(res$predictions$y.pred[res$predictions$y == 1] > 0.5)
sp = mean(res$predictions$y.pred[res$predictions$y == 0] < 0.5)
bac = (se + sp) / 2
cat("Sensitivity:", round(se, 2), "\n")
cat("Specificity:", round(sp, 2), "\n")
cat("Balanced accuracy:", round(bac, 2), "\n")
}

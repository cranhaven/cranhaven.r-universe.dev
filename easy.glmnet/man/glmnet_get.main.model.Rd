\name{glmnet_get.main.model}
\alias{glmnet_get.main.model}
\title{Get the main glmnet model across imputations and folds}
\description{Function to choose the \code{glmnet} model most similar to the other models on the list according to the Dice coefficient.}
\usage{
glmnet_get.main.model(x, childname = NULL, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a list of objects of class \code{"glmnet_fit"} or a list of objects that have a child of class \code{"glmnet_fit"}.}
  \item{childname}{name of the child of class \code{"glmnet_fit"} (if \code{x}) is a list of objects that have a child of class \code{"glmnet_fit"}).}
  \item{verbose}{(optional) logical, whether to print some messages during execution.}
}
\details{If there are several instances of the most similar model, it averages them.}
\value{An object of class \code{"glmnet_fit"}, representing the model most similar to the other models of the list according to the Dice coefficient.}
\references{
  Sobregrau, P., Bailles, E., Radua, J., Carreno, M., Donaire, A., Setoain, X., Bargallo, N., Rumia, J., Sanchez-Vives, M.V., Pintor, L. (2024)
  Design and validation of a diagnostic suspicion checklist to differentiate epileptic from psychogenic nonepileptic seizures (PNES-DSC).
  \emph{Journal of Psychosomatic Research}, \bold{180}, 111656, doi:10.1016/j.jpsychores.2024.111656.
}
\author{Joaquim Radua}
\seealso{
  \code{\link{glmnet_predict}} for obtaining predictions.
  \code{\link{cv}} for conducting a cross-validation.
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

# Show the main model
lasso = glmnet_get.main.model(res$models, "lasso")
cat(
  "Model: ~plogis(", round(lasso$a0, 2), "+",
  paste0(round(lasso$beta, 2), "*", names(lasso$beta), collapse = " + "),
  ")\n"
)
}

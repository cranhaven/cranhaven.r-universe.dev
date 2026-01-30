\name{glmnet_get.items.relevance}
\alias{glmnet_get.items.relevance}
\title{Get the relevance of the model items}
\description{Function to calculate the relevance of the items of a model or of a list of models.}
\usage{
glmnet_get.items.relevance(x, childname = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an object of class \code{"glmnet_fit"}, a list of objects of class \code{"glmnet_fit"}, or a list of objects that have a child of class \code{"glmnet_fit"}.}
  \item{childname}{name of the child of class \code{"glmnet_fit"} (if \code{x}) is a list of objects that have a child of class \code{"glmnet_fit"}).}
}
\details{The relevance is calculated as \code{abs( standardized_coefficient ) / sum(abs( standardized_coefficients ))}, as in the function \code{\link[lares]{lasso_vars}}.}
\value{A numeric vector representing the relevance of the items of the model.}
\author{Joaquim Radua, based on the previous work of others (see Details)}
\references{
  Palau, P., Solanes, A., Madre, M., Saez-Francas, N., Sarro, S., Moro, N., Verdolini, N., Sanchez, M., Alonso-Lana, S., Amann, B.L., Romaguera, A., Martin-Subero, M., Fortea, L., Fuentes-Claramonte, P., Garcia-Leon, M.A., Munuera, J., Canales-Rodriguez, E.J., Fernandez-Corcuera, P., Brambilla, P., Vieta, E., Pomarol-Clotet, E., Radua, J. (2023)
  Improved estimation of the risk of manic relapse by combining clinical and brain scan data.
  \emph{Spanish Journal of Psychiatry and Mental Health}, \bold{16}, 235--243, doi:10.1016/j.rpsm.2023.01.001.
}
\seealso{
  \code{\link{glmnet_predict}} for obtaining predictions,
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

# Show the relevance of the predictors
relevance = glmnet_get.items.relevance(res$models, "lasso")
relevance = relevance[which(relevance >= 0.01)] # Select items with >=1% relevance
round(relevance, 2)
}

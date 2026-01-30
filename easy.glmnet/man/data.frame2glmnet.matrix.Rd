\name{data.frame2glmnet.matrix}
\alias{data.frame2glmnet.matrix}
\alias{data.frame2glmnet.matrix_fit}
\title{Convert a data.frame into a matrix ready for glmnet}
\description{Function to convert categorical variables into dummy variables ready for \code{\link{glmnet_fit}} and  \code{\link{glmnet_predict}}. Additionally, it also removes constant columns.}
\usage{
data.frame2glmnet.matrix_fit(x)
data.frame2glmnet.matrix(m, x)
}
\arguments{
  \item{m}{model to conduct the conversion, obtained with \code{\link{data.frame2glmnet.matrix_fit}}.}
  \item{x}{data.frame to be converted.}
}
\details{Note that the returned matrix might differ from the design matrix of a linear model because for categoric variables with more than two levels, it creates as many dummy variables as levels (which is ok for lasso).}
\value{A matrix ready for \code{\link{glmnet_fit}} and \code{\link{glmnet_predict}}.}
\author{Joaquim Radua and Aleix Solanes}
\seealso{
  \code{\link{glmnet_predict}} for obtaining predictions,
  \code{\link{cv}} for conducting a cross-validation.
}
\examples{
# Create random x (predictors) and y (binary)
x = cbind(
  as.data.frame(matrix(rnorm(10000), ncol = 20)),
  matrix(sample(letters, 2500, TRUE), ncol = 5)
)
y = 1 * (plogis(apply(x[,1:5], 1, sum) + rnorm(500, 0, 0.1)) > 0.5)

# Predict y via cross-validation, including conversion to matrix
fit_fun = function (x_training, y_training) {
  m = list(
    matrix = data.frame2glmnet.matrix_fit(x_training)
  )
  x_mat = data.frame2glmnet.matrix(m$matrix, x_training)
  m$lasso = glmnet_fit(x_mat, y_training, family = "binomial")
  m
}
predict_fun = function (m, x_test) {
  x_mat = data.frame2glmnet.matrix(m$matrix, x_test)
  glmnet_predict(m$lasso, x_mat)
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

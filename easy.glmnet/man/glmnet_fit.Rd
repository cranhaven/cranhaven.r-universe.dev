\name{glmnet_fit}
\alias{glmnet_fit}
\alias{glmnet_predict}
\title{Obtain and use a glmnet prediction model}
\description{Function to easily fit and apply glmnet models (including best lambda estimation, etc).}
\usage{
glmnet_fit(x, y, family = c("binomial", "cox", "gaussian"),
           nfolds = 10, standardize = TRUE, min.beta = 1e-12)
glmnet_predict(m, x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{input matrix of dimension nobs x nvars; each row is an observation vector. It can be easily obtained with \code{\link{data.frame2glmnet.matrix}}.}
  \item{y}{response to be predicted. A binary vector for "binomial", a "Surv" object for "cox", or a numeric vector for "gaussian".}
  \item{family}{distribution of y: "binomial", "cox", or "gaussian".}
  \item{m}{lasso model to conduct the prediction, obtained with \code{\link{glmnet_fit}}.}
  \item{nfolds}{number of folds.}
  \item{standardize}{logical flag for x variable standardization. The coefficients are always returned on the original scale.}
  \item{min.beta}{minimum value of betas.}
}
\details{The function \code{glmnet_fit} mainly calls the function \code{\link[glmnet]{glmnet}} to fit a generalized linear model with lasso regularization, though with some extra code to make the call easier: it allow \code{x} to have a single column, it conducts an internal cross-validation using the function \code{\link[glmnet]{cv.glmnet}} to select the regularization parameter \code{lambda} automatically, and it removes the negligible coefficients.}
\value{An object of class \code{"glmnet_fit"}, which is briefly a list with the intercept (\code{"a0"}) and regressors (\code{"beta"}) of the model; it also includes the indices of the regressors (\code{"i"}) and the \code{"family"} of the response.}
\author{Joaquim Radua and Aleix Solanes}
\references{
  Solanes, A., Mezquida, G., Janssen, J., Amoretti, S., Lobo, A., Gonzalez-Pinto, A., Arango, C., Vieta, E., Castro-Fornieles, J., Berge, D., Albacete, A., Gine, E., Parellada, M., Bernardo, M.; PEPs group (collaborators); Pomarol-Clotet, E., Radua, J. (2022)
  Combining MRI and clinical data to detect high relapse risk after the first episode of psychosis.
  \emph{Schizophrenia}, \bold{8}, 100, doi:10.1038/s41537-022-00309-w.

  Palau, P., Solanes, A., Madre, M., Saez-Francas, N., Sarro, S., Moro, N., Verdolini, N., Sanchez, M., Alonso-Lana, S., Amann, B.L., Romaguera, A., Martin-Subero, M., Fortea, L., Fuentes-Claramonte, P., Garcia-Leon, M.A., Munuera, J., Canales-Rodriguez, E.J., Fernandez-Corcuera, P., Brambilla, P., Vieta, E., Pomarol-Clotet, E., Radua, J. (2023)
  Improved estimation of the risk of manic relapse by combining clinical and brain scan data.
  \emph{Spanish Journal of Psychiatry and Mental Health}, \bold{16}, 235--243, doi:10.1016/j.rpsm.2023.01.001.
}
\seealso{
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
res = cv(x, y, family = "binomial", fit_fun = fit_fun, predict_fun = predict_fun, nfold = 2)

# Show accuracy
se = mean(res$predictions$y.pred[res$predictions$y == 1] > 0.5)
sp = mean(res$predictions$y.pred[res$predictions$y == 0] < 0.5)
bac = (se + sp) / 2
cat("Sensitivity:", round(se, 2), "\n")
cat("Specificity:", round(sp, 2), "\n")
cat("Balanced accuracy:", round(bac, 2), "\n")
}

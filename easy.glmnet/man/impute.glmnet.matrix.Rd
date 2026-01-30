\name{impute.glmnet.matrix_fit}
\alias{impute.glmnet.matrix}
\alias{impute.glmnet.matrix_fit}
\title{Impute missing variables in a glmnet matrix multiple times}
\description{Function to impute, multiple times, the missing variables in a \code{glmnet.matrix}. \code{impute.glmnet.matrix_fit} finds the "lasso" models to conduct the imputations, and \code{impute.glmnet.matrix} does the imputations (in the same or a different dataset).}
\usage{
impute.glmnet.matrix_fit(x, ncores = 1, verbose = TRUE)
impute.glmnet.matrix(m, x, nimp = 20, verbose = TRUE)
}
\arguments{
  \item{m}{model to conduct the imputations, obtained with \code{\link{impute.glmnet.matrix_fit}}.}
  \item{x}{input matrix for glmnet of dimension nobs x nvars; each row is an observation vector. It can be easily obtained with \code{\link{data.frame2glmnet.matrix}}.}
  \item{ncores}{number of number of worker nodes (for parallelization).}
  \item{nimp}{number of imputations}
  \item{verbose}{(optional) logical, whether to print some messages during execution.}
}
\details{The user can then obtain a prediction from each dataset and combine the predictions using Rubin's rules (which usually means just averaging them). Note also that this function may take a lot of time.}
\value{A list of complete matrixes ready for \code{\link{glmnet_fit}} and \code{\link{glmnet_predict}}.}
\references{
  Solanes, A., Mezquida, G., Janssen, J., Amoretti, S., Lobo, A., Gonzalez-Pinto, A., Arango, C., Vieta, E., Castro-Fornieles, J., Berge, D., Albacete, A., Gine, E., Parellada, M., Bernardo, M.; PEPs group (collaborators); Pomarol-Clotet, E., Radua, J. (2022)
  Combining MRI and clinical data to detect high relapse risk after the first episode of psychosis.
  \emph{Schizophrenia}, \bold{8}, 100, doi:10.1038/s41537-022-00309-w.

  Palau, P., Solanes, A., Madre, M., Saez-Francas, N., Sarro, S., Moro, N., Verdolini, N., Sanchez, M., Alonso-Lana, S., Amann, B.L., Romaguera, A., Martin-Subero, M., Fortea, L., Fuentes-Claramonte, P., Garcia-Leon, M.A., Munuera, J., Canales-Rodriguez, E.J., Fernandez-Corcuera, P., Brambilla, P., Vieta, E., Pomarol-Clotet, E., Radua, J. (2023)
  Improved estimation of the risk of manic relapse by combining clinical and brain scan data.
  \emph{Spanish Journal of Psychiatry and Mental Health}, \bold{16}, 235--243, doi:10.1016/j.rpsm.2023.01.001.
}
\author{Joaquim Radua and Aleix Solanes}
\seealso{
  \code{\link{glmnet_predict}} for obtaining predictions.
  \code{\link{cv}} for conducting a cross-validation.
}
\examples{
# Quick example

# Create random x with missing values
x = matrix(rnorm(300), ncol = 3)
x = x + rnorm(1) * x[,sample(1:3)] + rnorm(1) * x[,sample(1:3)]
x[sample(1:300, 30)] = NA

# Impute missing values
m_impute = impute.glmnet.matrix_fit(x, ncores = 2)
x_imputed = impute.glmnet.matrix(m_impute, x)


# Complete example (it might take some time even if the example is simple...)
\donttest{
  # Create random x (predictors) and y (binary)
  x = matrix(rnorm(4000), ncol = 20)
  x = x + rnorm(1) * x[,sample(1:20)] + rnorm(1) * x[,sample(1:20)]
  y = 1 * (plogis(x[,1] - x[,2] + rnorm(200, 0, 0.1)) > 0.5)
  
  # Make some x missing values
  x[sample(1:4000, 400)] = NA
  
  # Predict y via cross-validation, including imputations
  fit_fun = function (x_training, y_training) {
    m = list(
      impute = impute.glmnet.matrix_fit(x_training, ncores = pmax(1, parallel::detectCores() - 2)),
      lasso = list()
    )
    x_imputed = impute.glmnet.matrix(m$impute, x_training)
    for (imp in 1:length(x_imputed)) {
      m$lasso[[imp]] = glmnet_fit(x_imputed[[imp]], y_training, family = "binomial")
    }
    m
  }
  predict_fun = function (m, x_test) {
    x_imputed = impute.glmnet.matrix(m$impute, x_test)
    y_pred = NULL
    for (imp in 1:length(x_imputed)) {
      y_pred = cbind(y_pred, glmnet_predict(m$lasso[[imp]], x_imputed[[imp]]))
    }
    apply(y_pred, 1, mean)
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
}

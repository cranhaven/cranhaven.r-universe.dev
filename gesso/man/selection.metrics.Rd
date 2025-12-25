\name{selection.metrics}
\alias{selection.metrics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Selection metrics
}
\description{
Calculates principal selection metrics for the binary zero/non-zero classification problem (sensitivity, specificity, precision, auc).
}
\usage{
selection.metrics(true_b_g, true_b_gxe, estimated_b_g, estimated_b_gxe)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{true_b_g}{vector of true main effect coefficients}
  \item{true_b_gxe}{vector of true interaction coefficients}
  \item{estimated_b_g}{vector of estimated main effect coefficients}
  \item{estimated_b_gxe}{vector of estimated interaction coefficients}
}
\value{
A list of principal selection metrics
  \item{b_g_non_zero}{number of non-zero main effects}
  \item{b_gxe_non_zero}{number of non-zero interactions}
  \item{mse_b_g}{mean squared error for estimation of main effects effect sizes}
  \item{mse_b_gxe}{mean squared error for estimation of interactions effect sizes}
  \item{sensitivity_g}{recall of the non-zero main effects}
  \item{specificity_g}{recall of the zero main effects}
  \item{precision_g}{precision with respect to non-zero main effects}
  \item{sensitivity_gxe}{recall of the non-zero interactions}
  \item{specificity_gxe}{recall of the zero interactions}
  \item{precision_gxe}{precision with respect to non-zero interactions}
  \item{auc_g}{area under the curve for zero/non-zero binary classification problem for main effects}
  \item{auc_gxe}{area under the curve for zero/non-zero binary classification problem for interactions}
}
\examples{
data = data.gen()
model = gesso.cv(data$G_train, data$E_train, data$Y_train)
gxe_coefficients = gesso.coef(model$fit, model$lambda_min)$beta_gxe                
g_coefficients = gesso.coef(model$fit, model$lambda_min)$beta_g  
selection.metrics(data$Beta_G, data$Beta_GxE, g_coefficients, gxe_coefficients)

}

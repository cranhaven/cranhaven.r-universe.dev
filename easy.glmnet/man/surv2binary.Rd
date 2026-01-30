\name{surv2binary}
\alias{surv2binary}
\title{Convert a "Surv" object into binary variables at different time points}
\description{Function to convert a \code{"Surv"} object (e.g., the predictions obtained from \code{\link{glmnet_predict}} using a \code{"cox"} model) into a list of binary variables (e.g., as obtained from \code{\link{glmnet_predict}} using a \code{"binomial"} model) at different time points.}
\usage{
surv2binary(x)
}
\arguments{
  \item{x}{a \code{"Surv"} object.}
}
\details{This function is useful, for instance, to estimate the AUC at different timepoints from \code{"cox"} predictions.}
\value{A list of times and binary variables.}
\author{Joaquim Radua}
\seealso{
  \code{\link{glmnet_predict}} for obtaining \code{"cox"} predictions.
  \code{\link{cv}} for conducting a cross-validation.
}
\examples{
library(survival)
library(pROC)

# Create random x (predictors) and y (survival)
x = matrix(rnorm(5000), ncol = 10)
time = rexp(500)
y = Surv(time, plogis(x[,1] / pmax(1, time^2) + rnorm(500)) > 0.5)

# Predict y via cross-validation
fit_fun = function (x, y) {
  glmnet_fit(x, y, family = "cox")
}
predict_fun = function (m, x) {
  glmnet_predict(m, x)
}
res = cv(x, y, family = "cox", fit_fun = fit_fun, predict_fun = predict_fun)

# Convert y to binary
y.binary = surv2binary(y)

# Calculate and plot AUC for binary y at each timepoint
time_auc = NULL
for (i in 1:length(y.binary)) {
  status_i = y.binary[[i]]$status
  if (length(unique(na.omit(status_i))) == 2) {
    time_auc = rbind(time_auc, data.frame(
      time = y.binary[[i]]$time,
      auc = roc(status_i ~ res$predictions$y.pred, levels = 0:1, direction = "<")$auc
    ))
  }
}
plot(time_auc$time, time_auc$auc, type = "l", xlab = "Time", ylab = "AUC", ylim = 0:1)
abline(h = 0.5)
}

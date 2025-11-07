\name{goodness}
\alias{goodness}
\title{
Model quality assessment.
}
\description{
Model quality assessment.
}
\usage{
goodness(model,data)
}
\arguments{
  \item{model}{
A modle.
}
  \item{data}{
Dataset.
}
}
\details{
Model quality index as follow:
MSE: the mean-squared-error;
RMSE: the root-mean-squared-error;
Rsquare: the variance of the predictions divided by the variance of the response;
adj.Rsquare: adjusted the variance of the predictions divided by the variance of the response;
MAE: the mean absolute error;
MAPE: the mean absolute percentage error;
RASE: the relative sum of absolute errors;
AIC: Akaike's An Information Criterion;
BIC: Schwarz's Bayesian criterion.
}
\value{
Result returns the results model quality index.
}
\author{
Zongzheng Chai, chaizz@126.com
}
\examples{
mod <- lm(mpg ~ wt, data = mtcars)
goodness(mod, mtcars)
}

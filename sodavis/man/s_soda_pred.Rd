\name{s_soda_pred}
\alias{s_soda_pred}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict the response y using S-SODA model.
}
\description{
S-SODA assumes within each slice the X vector follow multivariate normal distribution. This function predicts the response y by reverting the P(X | slice(y)) to P(slice(y) | X), and estimates the E(y|X) as sum_{h} E(y | slice(y)=h, X) P (slice(y)=h | X)
}
\usage{
s_soda_pred(x, model, po = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The design matrix, of dimensions n * p, without an intercept. Each row is an observation vector.
}
  \item{model}{
S-SODA model estimated from s_soda_model function.
}
  \item{po}{
Order of terms in X to approximate E(y | slice(y)=h, X).  If po=0, E(y | slice(y)=h, X) is the mean of y in slice h.  If po=1, E(y | slice(y)=h, X) is the linear regression of X to predict y in slice h.  If po=2, the linear regression also include 2nd order terms of X.
}
}
\value{
Predicted response.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ S-SODA }
\keyword{ Prediction }% __ONLY ONE__ keyword per line

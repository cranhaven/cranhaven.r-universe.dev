\name{s_soda_model}
\alias{s_soda_model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
S-SODA model estimation.
}
\description{
S-SODA assumes within each slice the X vector follow multivariate normal distribution. This function estimates the mean vector and covariance matrix of X for each slice.
}
\usage{
s_soda_model(x, y, H = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The design matrix, of dimensions n * p, without an intercept. Each row is an observation vector.
}
  \item{y}{
The response vector of dimension n * 1.
}
  \item{H}{
The number of slices.
}
}
\value{
  \item{int_h}{
Slice index.
}
  \item{int_p}{
Proportion of samples in each slice.
}
  \item{int_l}{
Length of each slice (max - min response).
}
  \item{int_m}{
Mean vector of covariates in each slice.
}
  \item{int_v}{
Covariance matrix of covariates in each slice.
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ S-SODA }

\name{ci}
\alias{ci}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Confidence Interval
}
\description{
Gets bootstrap confidence intervals for each of the coefficients of variables/predictors.
}
\usage{
ci(Beta, Beta_bootstrap, alpha = 0.05, type = c("basic", "quantile", "bca", "basic2"), 
   a, Beta2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Beta}{
An estimate of the coefficients.
}
 \item{Beta_bootstrap}{
Bootstrap estimates of the coefficients -- a B by p matrix, where B is the number of replications in the bootstrap and p is number of variables/predictors.
}
\item{alpha}{
Significance level -- default is 0.05.
}
\item{type}{
Different type of confidence interval: basic, quantile, bca (adjusted bootstrap confidence intervals) and basic2 (a modified basic confidence interval).
}
\item{a}{
Parameter in the bca confidence interval.
}
\item{Beta2}{
An estimator of the coefficients used in the basic2 confidence interval.
}
}
\value{
\item{interval}{
A 2 by p matrix containing the confidence intervals -- the first row is the lower bounds of the confidence intervals for each of the coefficients and the second row is the upper bounds of the confidence intervals.
} 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
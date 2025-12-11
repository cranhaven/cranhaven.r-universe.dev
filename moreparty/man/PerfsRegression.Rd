\name{PerfsRegression}
\alias{PerfsRegression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Performance measures for regressions
}
\description{
Computes various performance measures for regression tasks : sum of the squared errors (SSE), mean squared errors (MSE), root mean squared errors (RMSE), coefficient of determination (R2), Kendall's rank correlation (tau).
}
\usage{
PerfsRegression(pred, actual)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pred}{numerical vector of predicted values}
  \item{actual}{numerical vector of actual values}
}

\value{
A numeric vector of performance measures.
}

\examples{
  data(titanic)
  titanic <- titanic[complete.cases(titanic),]
  model <- partykit::ctree(Age ~ Sex + Pclass, data = titanic)
  pred <- predict(model)
  PerfsRegression(pred, titanic$Age)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tree}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

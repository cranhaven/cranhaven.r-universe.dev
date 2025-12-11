\name{PerfsBinClassif}
\alias{PerfsBinClassif}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Performance measures for binary classification tasks
}
\description{
Computes various performance measures for binary classification tasks : true positive rate, true negative rate, accuracy, balanced accuracy, area under curve (AUC).
}
\usage{
PerfsBinClassif(pred, actual)
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
  model <- partykit::ctree(Survived ~ Sex + Pclass, data = titanic)
  pred <- predict(model, type = "prob")[,"Yes"]
  PerfsBinClassif(pred, titanic$Survived)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tree}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

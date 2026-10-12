\name{s_soda_pred_grid}
\alias{s_soda_pred_grid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict the response y using S-SODA model in a 2-dimensional grid.
}
\description{
Calls function s_soda_pred in a 2-dimensional grid defined by x1 and x2.
}
\usage{
s_soda_pred_grid(xx1, xx2, model, po = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xx1}{
Grid breakpoints for predictor 1.
}
  \item{xx2}{
Grid breakpoints for predictor 2.
}
  \item{model}{
S-SODA model estimated from s_soda_model.
}
  \item{po}{
Order of terms in X to approximate E(y | slice(y)=h, X).
}
}
\value{
Predicted response.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ S-SODA }
\keyword{ Prediction }% __ONLY ONE__ keyword per line

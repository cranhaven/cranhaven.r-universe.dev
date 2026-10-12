\name{soda_trace_CV}
\alias{soda_trace_CV}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate a trace of cross-validation error rate for SODA forward-backward procedure
}
\description{
This function takes a SODA result variable as input, and calculates the cross-validation error for each step of the SODA procedure.
}
\usage{
soda_trace_CV(xx, yy, res_SODA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xx}{
The design matrix, of dimensions n * p, without an intercept. Each row is an observation vector.
}
  \item{yy}{
The response vector of dimension n * 1.
}
  \item{res_SODA}{
SODA result varaible. See example below.
}
}
\author{
Yang Li, Jun S. Liu
}
\examples{
# Michigan lung cancer dataset (uncomment the code to run)
#data(mich_lung);
#res_SODA = soda(mich_lung_xx, mich_lung_yy, gam=0.5);
#cv_SODA  = soda_trace_CV(mich_lung_xx, mich_lung_yy, res_SODA)
#cv_SODA
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ SODA }
\keyword{ cross-validation }

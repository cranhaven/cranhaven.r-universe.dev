\name{robustness}
\encoding{ascii}
\alias{robustness}
\alias{plot.robustness}
\alias{xtable.robustness}
\title{
Robustness check for the Implementation of the Panel Data Approach for Program Evaluation
}
\description{
Implements a leave-one-out robustness check for the Panel Data Approach for program evaluation as developed in Hsiao, Steve Ching and Ki Wan (2012). Robustness must be run after pampe and will automatically and iteratively remove each one of the controls resulted in pampe to check whether the results obtained in pampe are robust or are driven by a single control}
\usage{
robustness(pampe.object)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pampe.object}{
The object resulting from previously running the pampe function.
}
}


\author{
Ainhoa Vega-Bayo
}
\note{
Check the references in \code{\link{pampe}} for more information on the placebo studies. The leaps package by Lumley is required for pampe to run properly.
}


\seealso{
\code{\link{pampe}}
\code{\link{leaps}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\name{Chol_Eigen}
\alias{Chol_Eigen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Cholesky decomposition of a symmetric matrix.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function provides a faster Cholesky decomposition of a symmetric matrix using the Eigen package. 
}
\usage{
Chol_Eigen(R)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{
%%     ~~Describe \code{param} here~~
R is a symmetric matrix.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
lower triangular matrix of Cholesky decomposition}
\author{
%%  ~~who you are~~
\packageAuthor{RobustCalibration}

Maintainer: \packageMaintainer{RobustCalibration}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
% \code{\link{log_marginal_lik}}.
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")

\keyword{internal}
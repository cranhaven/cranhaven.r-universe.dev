\name{predict.scalreg}
\alias{predict.scalreg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prediction based on a scalreg object
}
\description{
When the type of a scalreg object is "regression", this predict method applies.
}
\usage{
\method{predict}{scalreg}(object, newX = NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
  	a fitted scalreg object.
}
  \item{newX}{
	X values at which the fit is required. If newX is NULL, return the fitted value of the object.}
  \item{\dots}{Additonal arguments for generic methods}  	
}

\value{
\item{y }{the predicted values.}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Tingni Sun
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
scalreg
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{predict}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{print.distfreereg}
\alias{print.distfreereg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Printing \code{distfreereg} Objects
}
\description{
	This is a \code{\link{print}} method for objects of class \code{distfreereg}.
}
\usage{
	\method{print}{distfreereg}(x, ..., digits = 3, col_sep = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	Object of class \code{distfreereg}.
}
  \item{\dots}{
	Additional parameters, currently ignored.
}
  \item{digits}{
	Integer; passed to \code{\link{signif}} to determine the number of significant digits to display.
}
  \item{col_sep}{
	Integer; specifies the padding (in units of spaces) between columns in the printed table of statistics.
}
}
\details{
	This function prints a useful summary of the \code{distfreereg} object.
}
\value{
	No return value (\code{NULL}).
}
\author{
	Jesse Miller
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{\link{distfreereg}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

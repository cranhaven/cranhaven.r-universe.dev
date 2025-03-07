\name{print.TukeyC}
\alias{print.TukeyC}

\title{
	Print Method for \code{TukeyC} objects.
}

\description{
	Returns (and prints) a list for objects of class \code{TukeyC}.
}

\usage{

\method{print}{TukeyC}(x, ...)

}

\arguments{
	\item{x}{A given object of the class \code{TukeyC}.}
	\item{\dots }{Further arguments (require by generic).}
}

\author{
	Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
	Enio G. Jelihovschi (\email{eniojelihovs@gmail.com})\cr
	Ivan Bezerra Allaman (\email{ivanalaman@gmail.com})\cr
}

\seealso{\code{\link{TukeyC}}
}

\examples{
data(RCBD) 

tk <- with(RCBD,
	   TukeyC(y ~ blk + tra,
		  data=dfm,
		  which='tra'))
tk

}

\keyword{package}
\keyword{TukeyC}

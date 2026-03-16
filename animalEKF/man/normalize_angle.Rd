\name{normalize_angle}
\alias{normalize_angle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrap angle measurements to the interval (-pi, pi).
}
\description{
Wrap angle measurements to the interval (-pi, pi).
}
\usage{
normalize_angle(theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
Numeric vector.
}
}
\author{
Samuel Ackerman
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{

x <- rnorm(n=1000, mean=1, sd=2)
xn <- normalize_angle(x)

plot(density(x), xlab="x", main="Unwrapped and wrapped normal density", las=1)
abline(v=1)

#this density is only estimated from -pi to pi
dens_wrapped <- density(xn, from=-pi, to=pi)
lines(dens_wrapped, col="red")

segments(x0=c(-pi, pi), x1=c(-pi, pi), y0=c(0,0), 
	y1=dens_wrapped$y[c(1, length(dens_wrapped$y))],
	col="red") 

legend("topleft", col=c(1,2), legend=c("unwrapped","wrapped"), lty=1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ distribution }% use one of  RShowDoc("KEYWORDS")

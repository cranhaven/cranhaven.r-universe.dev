\name{Level2Volume}
\alias{Level2Volume}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function for linear interpolation given the relationship of two variables
}
\description{
Given a relationship between two variables (e.g. level and volume), 
this function interpolates
the corresponding second variable (e.g. volume) for a known 
value of the first variable (e.g. level). This function is suitable to
represented e.g. the dynamics of water storage in 
a combined sewer overflow chamber.
}
\usage{
Level2Volume(lev, lev2vol)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lev}{
 A \code{numeric} object that represents the know variable e.g.
 the level in a storage chamber.
}
  \item{lev2vol}{
 A \code{list} of two elements. The first element is a \code{vector}
 named "lev" that contains the interpolation steps of the first variable (e.g. level).
 The second element is a \code{vector} that contains the interpolation steps 
 of the second variable (e.g. volume).
}
}
\details{
The function uses the \code{approx} function from the \code{stats} package with
"yleft" argument equal to the minimum value of the second variable and "yright" 
argument equal to the maximum value of the second variable.
}
\value{
A \code{numeric} object with one element representing the interpolated value 
of the second variable (e.g. volume).
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
J.A. Torres-Matallana; K. Klepiszewski; G. Schutz.
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
library(EmiStatR)

# definition of relationship level to volume
lev2vol <- list(lev = c(.06, 1.10, 1.30, 3.30), vol = c(0, 31, 45, 200))

interpolated_volume <- Level2Volume(lev=c(0, .25, 1.25, 2.25, 4.25), lev2vol = lev2vol)
interpolated_volume
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Level to Volume}% use one of  RShowDoc("KEYWORDS")
\keyword{Level2Volume}% __ONLY ONE__ keyword per line

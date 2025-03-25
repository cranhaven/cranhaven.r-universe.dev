\name{Volume2Level}
\alias{Volume2Level}
\title{
Function for linear interpolation given the relationship of two variables
}
\description{
Given a relationship between two variables (e.g. volume and level), 
this function interpolates
the corresponding second variable (e.g. level) for a known 
value of the first variable (e.g. volume). This function is suitable to
represented e.g. the dynamics of water storage in 
a combined sewer overflow chamber.}

\usage{Volume2Level(vol, lev2vol)}

\arguments{
  \item{vol}{
 A \code{numeric} object that represents the know variable e.g.
 the volume in a storage chamber.
}
  \item{lev2vol}{
 A \code{list} of two elements. The first element is a \code{vector}
 named "lev" that contains the interpolation steps of the first variable (e.g. level).
 The second element is a \code{vector} that contains the interpolation steps 
 of the second variable (e.g. volume).
}
}

%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}

\author{
J.A. Torres-Matallana, G. Schutz
}

\examples{
library(EmiStatR)

# definition of relationship level to volume
lev2vol <- list(lev = c(.06, 1.10, 1.30, 3.30), vol = c(0, 31, 45, 200))

interpolated_level <- Volume2Level(vol=c(0, 25, 41, 190, 220), lev2vol = lev2vol)
interpolated_level

}
\keyword{Volume2Level}

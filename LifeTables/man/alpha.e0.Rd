\name{alpha.e0}
\alias{alpha.e0}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Alpha to match life expectancy
}
\description{
Finds the alpha value to reproduce a desired life expectancy given a life table family/pattern
}
\usage{
alpha.e0(pattern, e0.target, sex="female")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pattern}{An integer (1-7) which indicates the desired life table family
}
  \item{e0.target}{The desired life expectancy
}
  \item{sex}{"male" or "female"
}
}
\details{
An appropriate life table family (pattern) can be found using \code{\link{hmd.DA}} or \code{\link{hmd.DA.mx}}
}
\value{The alpha value to produce the life expectancy entered in \code{e0.target}
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
David J. Sharrow
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{lt.mx}}, \code{\link{mortmod}}, \code{\link{hmd.DA}}
}
\examples{


alpha.e0(pattern=2, e0.target=65, sex="male")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
\keyword{ misc }% __ONLY ONE__ keyword per line

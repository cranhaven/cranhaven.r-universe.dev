\name{lt.mx}
\alias{lt.mx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Life tables from nmx
}
\description{
Builds a life table by using the mortality rate schedule to calculate the subsequent columns of the table
}
\usage{
lt.mx(nmx, sex="female", age = c(0,1,seq(5,110,5)), nax=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nmx}{
A vector of age-specific mortality rates
}
  \item{age}{
The start age of each age interval (will also be the start age for each interval of the subsequent life table)
}

  \item{sex}{
"male" or "female". The sex determines the nax value for the childhood age intervals using the Coale and Demeney West values when \code{nax=NULL} and when under 5 mortality is split into 0-1 and 1-4.}

  \item{nax}{A set of nax values for the life table. Must have the same length as \code{nmx} and if no values are provided, this argument defaults to half the length of the age interval except for the first two childhood age groups.}
}


%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{

 \item{e0 }{Life expectancy at birth}
 \item{lt }{A life table with age intervals matching those of the nmx schedule on which the table is built and with columns for age, nax, nmx, nqx, npx, ndx, lx, nLx, Tx, and ex.}
 \item{lt.5q0}{The probability a newborn will die before age 5}
 \item{lt.45q15}{The probability a 15-year old will die before reaching age 60}
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
David Sharrow
}
\note{
Depending on the mortality rates entered, the resulting life table may have fewer age groups than the length of the mortality rate schedule. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
# some data to build the table
data(MLTobs)
test.mx.m <- mlt.mx[,1]

# build the life table
lt.mx(nmx=test.mx.m, sex="male")

# another example with female data ages 0-85
test.mx.f <- flt.mx[1:19,1] ## ages 0-85
lt.mx(nmx=test.mx.f, sex="female", age=c(0,1,seq(5,85,5)))
        }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
\keyword{ methods }% __ONLY ONE__ keyword per line

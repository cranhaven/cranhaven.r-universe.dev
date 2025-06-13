\name{plotMLT}
\alias{plotMLT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting function for life table quantities
}
\description{
Plots either a single column or scrolls through a plot for each column from a life table calculated from a user supplied mortality rate schedule
}
\usage{
plotMLT(..., mx.out, sex, lt.col = "All", log = FALSE, 
	age = c(0, 1, seq(5, 110, 5)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
Arguments to pass to \code{par}
}
  \item{mx.out}{
A mortality rate schedule with age groups 0-1, 1-4, 5-9, 10-14,..., up to any open interval.
}
  \item{sex}{
The sex for the calculated life table. "male" or "female"
}
  \item{lt.col}{
The desired life table quantity to plot. lt.col="All", the default, will scroll through a plot of each quantity when the user keys "return" after each plot. Otherwise a specific column can be selected by using the following integers (1-nmx, 2-nqx, 3-npx, 4-ndx, 5-lx, 6-nLx, 7-Tx, 8-ex) 
}
  \item{log}{
Logical. If TRUE and \code{lt.col} is not set to "All", the y-axis will be in log scale. 
}
  \item{age}{
The start ages for each interval in the life table
}
}
\details{
If lt.col=3 (npx) and log=TRUE, a warning message will appear noting that a y value <= 0 is omitted. This is the probability of surviving the final interval, which is by definition, 0. 
}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
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
\code{\link{lt.mx}}
}
\examples{
	\dontrun{
mx.examp <- exp(mortmod(pattern=1, alpha=.05, sex="male")) 
plotMLT(mx.out=mx.examp, sex="male", lt.col="All")

plotMLT(mx.out=mx.examp, sex="male", lt.col=3, log=TRUE)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
\keyword{ plot }

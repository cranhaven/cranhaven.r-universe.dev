\name{plot.TPidm}
\alias{plot.TPidm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot method for a TPidm object
}
\description{
Plot method for an object of class \sQuote{TPidm}. It draws the estimated occupation probabilities (if s = 0) or transition probabilities (if s > 0) and confidence intervals (only available if \code{CI} is \code{TRUE}) in a basic scatterplot or in a multipanel layout.
}
\usage{
\method{plot}{TPidm}(x, chosen.tr = "ALL", col = "black", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class \sQuote{TPidm}.
}
  \item{chosen.tr}{
Character vector of the form \sQuote{c(\dQuote{from to}, \dQuote{from to})} specifying which transitions should be plotted. Default, all the transition probabilities are plotted.
}
  \item{col}{Character vector specifying colors for each curves. Default \code{col}=\dQuote{black}.
}
  \item{\dots}{
Further arguments to plot.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
%%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%%}
%%\references{
%% ~put references to the literature/web site here ~
%%}
%%\author{
%%  ~~who you are~~
%%}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
data(colonTP)

# create a TPidm object with s = 0, t = "last" (default),
# CI = FALSE and method = "NM" (default):
nm0 <- TPidm(colonTP, s = 0, CI=FALSE)

# plot all possible transitions without CI:
plot(nm0)

# or 
# create a TPidm object with s = 0, t = "last" (default),
# CI = TRUE (default) and method = "AJ":
aj0ci <- TPidm(colonTP, s = 0, method = "AJ")

# plot transitions "1 2" and "1 3" with CI:
plot(aj0ci, chosen.tr = c("1 2", "1 3"))

# create a TPidm object with s = 365, t = 1095, cov = "rx",
# CI = FALSE and method = "NM" (default):
nm365cov <- TPidm(colonTP, s = 365, t = 1095, cov = "rx", CI=FALSE)

# plot transitions "1 2" and "1 3" without CI:
plot(nm365cov, chosen.tr = c("1 2", "1 3"), col=c("blue","red","green"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%%\keyword{ ~kwd1 }
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

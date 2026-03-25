\name{solbeta}
\alias{solbeta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Recursive resolution of beta prior calibration
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
In the capture-recapture experiment of Chapter 5, the prior information
is represented by a prior expectation and prior confidence intervals. This
function derives the corresponding beta \eqn{B(\alpha,\beta)}
prior distribution by a divide-and-conquer scheme.
}
\usage{
solbeta(a, b, c, prec = 10^(-3))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
%%     ~~Describe \code{a} here~~
lower bound of the prior 95\%~confidence interval
}
  \item{b}{
%%     ~~Describe \code{b} here~~
upper bound of the prior 95\%~confidence interval
}
  \item{c}{
%%     ~~Describe \code{c} here~~
mean of the prior distribution
}
  \item{prec}{
%%     ~~Describe \code{prec} here~~
maximal precision on the beta coefficient \eqn{\alpha}
}
}
\details{
Since the mean \eqn{\mu} of the beta distribution is known, there is a single free parameter
\eqn{\alpha} to determine, since \eqn{\beta=\alpha(1-\mu)/\mu}. The function \code{solbeta} searches for
the corresponding value of \eqn{\alpha}, starting with a precision of \eqn{1} and stopping
at the requested precision \code{prec}.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{alpha }{first coefficient of the beta distribution}
\item{beta }{second coefficient of the beta distribution}
%% ...
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{probet}}
}
\examples{
solbeta(.1,.5,.3,10^(-4))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{capture-recapture}
\keyword{prior elicitation}
\keyword{beta distribution}
\keyword{divide-and-conquer}

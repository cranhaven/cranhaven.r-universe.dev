\name{jumpoints}
\alias{jumpoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Change point estimation in piecewise constant models
}
\description{
Estimation of change points and model selection via generalized BIC and other criteria
}
\usage{
jumpoints(y, x, k = min(30, round(length(y)/10)), output = "2", 
    psi = NULL, round = TRUE, control = fit.control(), 
    selection = sel.control(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
the observed (genomic) sequence supposed to have a piecewise constant mean function.
}
  \item{x}{
the `segmented' variable, e.g. the genomic location. If missing simple indices 1,2,... n (length of \code{y}) are assumed.
}
  \item{k}{
the starting number of changepoints. It should be quite larger than the supposed number of 
(true) changepoints. This argument is ignored if starting values of the changepoints are specified via
\code{psi}.
}
  \item{output}{
which output should be produced? Possible values are \code{"1"}, \code{"2"}, or \code{"3"}; see Details
}
  \item{psi}{
numeric vector to indicate the starting values for the changepoints. When \code{psi=NULL} (default), \code{k} 
  quantiles are assumed.
}
  \item{round}{
logical; should the values of the changepoints be rounded?
}
  \item{control}{
a list returned by \code{fit.control}.
}
  \item{selection}{
a list returned by \code{sel.control}.
}
  \item{\dots}{
additional arguments.
}
}
\details{
The algorithm works by suitably transforming the observed responses to fit a continuous piecewise linear model.
Starting from \code{k} changepoints, a large number of changepoints is first estimated. This number will be (typically slightly) lower than \code{k} since some changepoints will be discarded during the iterative steps when taking non admissible values. If \code{output="1"}, \code{jumpoints} returns them which typically will be more than the actual ones. If \code{output="2"} the appropriate number of changepoints is 
selected via the criterion specified in argument \code{selection} via \code{\link{sel.control}} (e.g. BIC, MDL, ..). %%At this aim the lars algorithm is employed. 
Finally if \code{output="3"}, the segmented algorithm is run again to try to improve the changepoint estimates returned by the previous step. 
}
\value{
A list including several components depending on the value of \code{output} \cr

If \code{output="1"} the most relevant components are 
\item{fitted.values }{the fitted values}
\item{n.psi}{the estimated number of changepoints}
\item{est.means}{the estimated means}
\item{psi}{the estimated changepoints}\cr

If \code{output="2"} the most relevant components are 
\item{fitted.values }{the fitted values}
\item{n.psi}{the estimated number of changepoints}
\item{criterion}{the values of the selection criterion}
\item{psi}{the estimated changepoints}
\item{est.means}{the estimated means}
\item{psi0}{the estimated changepoints at ouput 1 (before applying the selection criterion)}
\item{est.means0}{the estimated means at ouput 1 (before applying the selection criterion)}\cr

If \code{output="3"} the most relevant components are those of output 2 but
\item{psi0}{the estimated changepoints at ouput 1}
\item{psi1}{the estimated changepoints at ouput 2}
\item{psi}{the estimated changepoints at ouput 3 (after applying again the segmented algorithm).}
}

\references{
Muggeo, V.M.R., Adelfio, G., Efficient change point detection for genomic sequences 
  of continuous measurements, \emph{Bioinformatics} \bold{27}, 161-166.
}
\author{
Vito Muggeo
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{lars}}, \code{\link{sel.control}}, \code{\link{fit.control}}.
}
\examples{

\dontrun{
n<-100
x<-1:n/n
 
lp<-I(x>.1) -1*I(x>.15)+.585*I(x>.45)-.585*I(x>.6) -I(x>.9)
e<-rnorm(n,0,.154)
y<-lp+e #data

#fit the model without selecting the changepoints
o1<-jumpoints(y,output="1")
plot(o1, typeL="l")
lines(lp, col=2) #true regression function
legend("topright", c("true","fit with output=1"),bty="n", col=c(2,1), lty=1)

#fit model and select the changepoints
o2<-jumpoints(y,output="2")
par(mfrow=c(1,2))
plot(o2, what="c")
plot(o2, typeL="s")
lines(lp, col=3) #true regression function
legend("topright", c("true","fit with output=2"),bty="n", col=c(3,1), lty=1)
}
    }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ model }
\keyword{ regression }% 

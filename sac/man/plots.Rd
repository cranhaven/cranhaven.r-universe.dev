\name{plots}
\alias{Graf.Diagnostic}
\alias{Plot.ll}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Visualized Model Diagnostic and Loglikelihood Plot}
\description{
Plot and compare the empirical likelihood and semiparametric empirical
likelihood distribution functions, plot loglikelihood function. }
\usage{
Graf.Diagnostic(x, k, m, Alpha, Beta, Color, LTY, xlab = "x", 
    ylab = "Estimated DF's", main = "Model Diagnostic", 
    OneLegend = TRUE, lgnd1, lgnd2, arw1, arw2, ...)
Plot.ll(x, ll, col, xaxis.lab = NULL, xlab = "k", ylab = "Loglikelihood", 
    main = "Plot of Loglikelihood",...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a numeric vector or matrix containing the data, one row per observation;}
  \item{ll}{ loglikelihood function, output of \code{\link{SemiparChangePoint}} }
  \item{col}{ color code or character string for the loglikelihood curve }
  \item{xaxis.lab}{a vector of character strings or numeric values to be placed at
  the tickpoints as \code{labels} of \code{\link{axis}} }
  \item{k}{ the estimated change-point, output of \code{\link{SemiparChangePoint}} }
  \item{m}{ = \code{n}, the sample size, for \code{"one-change"} alternative,
  or the estimated second change-point for \code{"epidemic"} alternative,
  an output of \code{\link{SemiparChangePoint}} }
  \item{Alpha}{ estimated parameter \eqn{\alpha}{alpha}, output of \code{\link{SemiparChangePoint}}  }
  \item{Beta}{ estimated parameter \eqn{\beta}{beta}, output of \code{\link{SemiparChangePoint}}  }
  \item{Color}{a vector of character strings or color codes for
  curves of estimated distribution functions \eqn{\hat F}{F-hat}, \eqn{\tilde F}{F-tilde},
  \eqn{\hat G}{G-hat} and \eqn{\tilde G}{G-tilde}}
  \item{LTY}{ vector of lty's, LTY=c(lty1, lty2, lty3, lty4), corresponds to the above color codes }
  \item{xlab}{ character string for x-axis lable }
  \item{ylab}{ character string for y-axis lable }
  \item{main}{ character string for main title }
  \item{OneLegend}{ a logical indicating whether plot one or two legend. }
  \item{lgnd1}{ a numeric vector of two specify the position of the first legend box }
  \item{lgnd2}{ a numeric vector of two specify the position of the second legend box,
  if \code{OneLegend} = FALSE }
  \item{arw1}{ a numeric vector of four numbers indicating start and end positions of
  the first arrows point to curves }
  \item{arw2}{ a numeric vector of four numbers indicating start and end positions of
  the second arrows point to curves }
  \item{\dots}{ other arguments of function \code{plot}}
}
%\details{
%}
%\value{
%}
\references{
Guan, Z.(2001) Some Results About Empirical Likelihood Method,
\emph{Ph.D. Thesis, The University of Toledo};

Guan, Z.(2004) A semiparametric change-point model, \emph{Biometrika}, 91, 4, 849--862.

Guan, Z. Semiparametric Tests for Changepoints with Epidemic Alternatives.
 }
\author{ Zhong Guan \email{zguan@iusb.edu} }
%\note{}
\seealso{\code{\link{schapt}} }
\examples{
require(sac) #load the package
k<-30
n<-80
x<-rnorm(n,0,1)
x[(k+1):n]<-x[(k+1):n]+1.5
res<-SemiparChangePoint(x, alternative = "one.change")
Plot.ll(x, res$ll, col="blue")

## Nile data with one change-point: the annual flows drop in 1898 which corresponds 
## to k=28. It is believed to be caused by the building of the first Aswan dam.
if(! "package:sac" \%in\% search()) library(sac) 
    #if package sac has not been loaded, load it.
if(! "package:stats" \%in\% search()) library(stats)
data(Nile)
plot(Nile, type="p")
Nile.res<-SemiparChangePoint(Nile, alternative = "one.change")
Color<-c(1,2,3,4); LTY<-c(1,2,3,4)

## Plots of estimated distribution functions
Graf.Diagnostic(Nile, Nile.res$k.hat, length(Nile), Nile.res$alpha.hat, 
    Nile.res$beta.hat, Color, LTY, xlab = "x", ylab = "Estimated DF's", 
    main="Model Diagnostic for Nile Data", OneLegend = FALSE, lgnd1 = 
    c(1100, 0.15), lgnd2 = c(600, .99), arw1=c(780, .93, 1010, .9),
    arw2 = c(1165, .15, 1015, .24))

## Plot of loglikelihood function
Plot.ll(Nile, Nile.res$ll, col = "blue")
Plot.ll(Nile, Nile.res$ll, col = "blue", xaxis.lab = seq(1871,1970, length = 100), 
    xlab = "Year")
}
\keyword{device}% at least one, from doc/KEYWORDS

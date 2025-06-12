\name{bssn}
\alias{bssn}
\alias{dbssn}
\alias{pbssn}
\alias{qbssn}
\alias{rbssn}
\alias{rmixbssn}
\alias{mmmeth}

\title{Birnbaum-Saunders model based on Skew-Normal distribution}

\description{It provides the density, distribution function, quantile function, random number generator, likelihood function, moments and EM algorithm for Maximum Likelihood estimators for a given sample, all this for the three parameter Birnbaum-Saunders model based on Skew-Normal Distribution. Also, we have the random number generator for the mixture of Birbaum-Saunders model based on Skew-Normal distribution. Finally, the function mmmeth() is used to find the initial values for the parameters alpha and beta using modified-moment method.
}
\usage{
dbssn(ti, alpha=0.5, beta=1, lambda=1.5)
pbssn(q,  alpha=0.5, beta=1, lambda=1.5)
qbssn(p,  alpha=0.5, beta=1, lambda=1.5)
rbssn(n,  alpha=0.5, beta=1, lambda=1.5)
rmixbssn(n,alpha,beta,lambda,pii)
mmmeth(ti)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{ti}{
vector of observations.
}
  \item{q}{
vector of quantiles.
}
  \item{p}{
vector of probabilities.
}
  \item{n}{
number of observations.
}
  \item{alpha}{
shape parameter.
}
  \item{beta}{
scale parameter.
}
  \item{lambda}{
skewness parameter.
}

\item{pii}{Are weights adding to 1. Each one of them (alpha, beta and lambda) must be a vector of length g if you want to generate a random numbers from a mixture distribution BSSN.}

}
\details{
If \code{alpha}, \code{sigma} or \code{lambda} are not specified they assume the default values of 0.5, 1 and 1.5, respectively, belonging to the Birnbaum-Saunders model based on Skew-Normal distribution denoted by \eqn{BSSN(0.5,1,1.5)}.

As discussed in Filidor et. al (2011) we say that a random variable T is distributed as an BSSN with shape parameter \eqn{\alpha>0}, scale parameter \eqn{\beta>0} and skewness parameter \eqn{\lambda} in \eqn{R}, if its probability density function (pdf) is given by

\deqn{f(t)=2\phi(a(t;\alpha,\beta))\Phi(\lambda a(t;\alpha,\beta))A(t;\alpha,\beta), t>0}

where \eqn{\phi(.)} and \eqn{\Phi(.)} are the standard normal density and cumulative distribution function respectively. Also \eqn{a(t;\alpha,\beta)=(1/\alpha)(\sqrt{t/\beta}-\sqrt{\beta/t}}) and \eqn{A(t;\alpha,\beta)=t^{-3/2}(t+\beta)/(2\alpha \beta^{1/2})}
}


\value{
\code{dbssn} gives the density, \code{pbssn} gives the distribution function, \code{qbssn} gives the quantile function, \code{rbssn} generates a random sample and \code{rmixbssn} genrates a mixture random sample.

The length of the result is determined by n for \code{rbssn}, and is the maximum of the lengths of the numerical arguments for the other functions \code{dbssn}, \code{pbssn} and \code{qbssn}.
}

\references{
Vilca, Filidor; Santana, L. R.; Leiva, Victor; Balakrishnan, N. (2011). Estimation of extreme percentiles in Birnbaum Saunders distributions. Computational Statistics & Data Analysis (Print), 55, 1665-1678.

Santana, Lucia; Vilca, Filidor; Leiva, Victor (2011). Influence analysis in skew-Birnbaum Saunders regression models and applications. Journal of Applied Statistics, 38, 1633-1649.
}

\author{Rocio Maehara \email{rmaeharaa@gmail.com} and Luis Benites \email{lbenitesanchez@gmail.com}
}
%\note{
%The numerical arguments other than \code{n} are recycled to the length of the result.
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{EMbssn}}, \code{\link{momentsbssn}}, \code{\link{ozone}}, \code{\link{reliabilitybssn}}
}

\examples{
\dontrun{
## Let's plot an Birnbaum-Saunders model based on Skew-Normal distribution!

## Density
 sseq <- seq(0,3,0.01)
 dens <- dbssn(sseq,alpha=0.2,beta=1,lambda=1.5)
 plot(sseq, dens,type="l", lwd=2,col="red", xlab="x", ylab="f(x)", main="BSSN Density function")

# Differing densities on a graph
# positive values of lambda
 y   <- seq(0,3,0.01)
 f1  <- dbssn(y,0.2,1,1)
 f2  <- dbssn(y,0.2,1,2)
 f3  <- dbssn(y,0.2,1,3)
 f4  <- dbssn(y,0.2,1,4)
 den <- cbind(f1,f2,f3,f4)

 matplot(y,den,type="l", col=c("deepskyblue4", "firebrick1", "darkmagenta", "aquamarine4"), ylab
 ="Density function",xlab="y",lwd=2,sub="(a)")

 legend(1.5,2.8,c("BSSN(0.2,1,1)", "BSSN(0.2,1,2)", "BSSN(0.2,1,3)","BSSN(0.2,1,4)"),
 col = c("deepskyblue4", "firebrick1", "darkmagenta", "aquamarine4"), lty=1:4,lwd=2,
 seg.len=2,cex=0.8,box.lty=0,bg=NULL)


#negative values of lambda
 y   <- seq(0,3,0.01)
 f1  <- dbssn(y,0.2,1,-1)
 f2  <- dbssn(y,0.2,1,-2)
 f3  <- dbssn(y,0.2,1,-3)
 f4  <- dbssn(y,0.2,1,-4)
 den <- cbind(f1,f2,f3,f4)

 matplot(y,den,type="l", col=c("deepskyblue4", "firebrick1", "darkmagenta", "aquamarine4"),
 ylab ="Density function",xlab="y",lwd=2,sub="(a)")
 legend(1.5,2.8,c("BSSN(0.2,1,-1)", "BSSN(0.2,1,-2)","BSSN(0.2,1,-3)", "BSSN(0.2,1,-4)"),
 col=c("deepskyblue4","firebrick1", "darkmagenta","aquamarine4"),lty=1:4,lwd=2,seg.len=2,
 cex=1,box.lty=0,bg=NULL)


## Distribution Function
 sseq <- seq(0.1,6,0.05)
 df   <- pbssn(q=sseq,alpha=0.75,beta=1,lambda=3)
 plot(sseq, df, type = "l", lwd=2, col="blue", xlab="x", ylab="F(x)",
 main = "BSSN Distribution  function")
 abline(h=1,lty=2)


#Inverse Distribution Function
 prob <- seq(0,1,length.out = 1000)
 idf  <- qbssn(p=prob,alpha=0.75,beta=1,lambda=3)
 plot(prob, idf, type="l", lwd=2, col="gray30", xlab="x", ylab =
 expression(F^{-1}~(x)), mgp=c(2.3,1,.8))
 title(main="BSSN Inverse Distribution function")
 abline(v=c(0,1),lty=2)


#Random Sample Histogram
 sample <- rbssn(n=10000,alpha=0.75,beta=1,lambda=3)
 hist(sample,breaks = 70,freq = FALSE,main="")
 title(main="Histogram and True density")
 sseq   <- seq(0,8,0.01)
 dens   <- dbssn(sseq,alpha=0.75,beta=1,lambda=3)
 lines(sseq,dens,col="red",lwd=2)


##Random Sample Histogram for Mixture of BSSN
alpha=c(0.55,0.25);beta=c(1,1.5);lambda=c(3,2);pii=c(0.3,0.7)
sample <- rmixbssn(n=1000,alpha,beta,lambda,pii)
hist(sample$y,breaks = 70,freq = FALSE,main="")
title(main="Histogram and True density")
temp   <- seq(min(sample$y), max(sample$y), length.out=1000)
lines(temp, (pii[1]*dbssn(temp, alpha[1], beta[1],lambda[1]))+(pii[2]*dbssn(temp, alpha[2]
, beta[2],lambda[2])), col="red", lty=3, lwd=3) # the theoretical density
lines(temp, pii[1]*dbssn(temp, alpha[1], beta[1],lambda[1]), col="blue", lty=2, lwd=3)
# the first component
lines(temp, pii[2]*dbssn(temp, alpha[2], beta[2],lambda[2]), col="green", lty=2, lwd=3)
# the second component
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Birnbaum-Saunders Skew-Normal}
\keyword{bssn}


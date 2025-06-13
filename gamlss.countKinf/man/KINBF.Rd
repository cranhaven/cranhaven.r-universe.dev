\name{KINBF}
\alias{KINBF}
\alias{dKINBF}
\alias{pKINBF}
\alias{qKINBF}
\alias{rKINBF}

\title{K-inflated Negative Binomial Family distributions for fitting a GAMLSS model
}



\description{

The function \code{KINBF} defines the K-inflated  Negative Binomial Family distribution, a four  parameter distribution, for a \code{gamlss.family} object to be used in GAMLSS fitting  using the function \code{gamlss()}. The functions \code{dKINBF}, \code{pKINBF}, \code{qKINBF} and \code{rKINBF} define the density,
distribution function, quantile function and random generation for the K-inflated Negative Binomial Family, \code{KINBF()}, distribution.
}



\usage{

 KINBF(mu.link = "log", sigma.link = "log", nu.link = "log",
    tau.link = "logit", kinf="K")

dKINBF(x, mu = 1, sigma = 1, nu = 2, tau = 0.1, kinf=0, log = FALSE)

pKINBF(q, mu = 1, sigma = 1, nu = 2, tau = 0.1, kinf=0, lower.tail = TRUE,
            log.p = FALSE)

qKINBF(p, mu = 1, sigma = 1, nu = 2, tau = 0.1, kinf=0, lower.tail = TRUE,
            log.p = FALSE)

rKINBF(n, mu = 1, sigma = 1, nu = 2, kinf=0, tau = 0.1)
}



\arguments{

\item{mu.link}{ Defines the \code{mu.link}, with "log" link as the default for the mu parameter}

\item{sigma.link}{Defines the  \code{sigma.link}, with "log" link as the default for the sigma parameter}

\item{nu.link}{Defines the \code{nu.link}, with "log" link as the default for the nu parameter}

\item{tau.link}{Defines the \code{tau.link}, with "logit" link as the default for the tau parameter}



\item{x}{ vector of (non-negative integer) quantiles}

\item{mu}{vector of positive means}

\item{sigma}{vector of positive despersion parameter}

\item{nu}{vector of nu}

\item{tau}{vector of inflated point probability }

\item{p}{vector of probabilities}

\item{q}{vector of quantiles}

\item{n}{number of random values to return}

\item{kinf}{ defines inflated point in generating K-inflated distribution}

\item{log,log.p}{logical; if TRUE, probabilities p are given as log(p)}

\item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x]}
}



\details{
The  definition for the K-inflated Negative Binomial Family distribution.
}


\value{
The functions \code{KINBF} return a \code{gamlss.family} object which can be used to fit K-inflated Negative Binomial Family distribution in the \code{gamlss()} function.
}



\references{

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion),\emph{Appl. Statist.},\bold{54}, part 3, pp 507-554.



Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R. Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.org/}).



Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.



Rigby, R. A. and Stasinopoulos D. M. (2010) The gamlss.family distributions, (distributed with this package or see\url{http://www.gamlss.org/})



Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)\emph{Flexible Regression and Smoothing: Using GAMLSS in R}, Chapman and Hall/CRC.



Najafabadi, A. T. P. and MohammadPour, S. (2017). A k-Inflated Negative Binomial Mixture Regression Model: Application to Rate-Making Systems. Asia-Pacific Journal of Risk and Insurance, 12.
}



\author{
Saeed Mohammadpour <\email{s.mohammadpour1111@gamlil.com}>, Mikis Stasinopoulos <\email{d.stasinopoulos@londonmet.ac.uk}>
}


\seealso{
 \code{\link{gamlss.family}}, \code{\link{KINBF}} }


\examples{
#--------------------------------------------------------------------------------

# gives information about the default links for the  Negative Binomial Family distribution
KINBF()
#--------------------------------------------------------------------------------

# generate zero inflated Negative Binomial Family distribution
gen.Kinf(family=NBF, kinf=0)

# generate random sample from zero inflated Negative Binomial Family distribution
x<-rinf0NBF(1000,mu=1, sigma=.5, nu=-.2, tau=.2)

# fit the zero inflated Negative Binomial Family distribution using gamlss
data<-data.frame(x=x)
\dontrun{
gamlss(x~1, family=inf0NBF, data=data)
histDist(x, family=inf0NBF)}
#--------------------------------------------------------------------------------

# generated one inflated Negative Binomial Family distribution
gen.Kinf(family=NBF, kinf=1)

# generate random sample from one inflated Negative Binomial Family distribution
x<-rinf1NBF(1000,mu=1, sigma=.5, nu=-.2, tau=.2)

# fit the one inflated Negative Binomial Family distribution using gamlss
data<-data.frame(x=x)
\dontrun{
gamlss(x~1, family=inf1NBF, data=data)
histDist(x, family=inf1NBF)}
#--------------------------------------------------------------------------------

mu=4; sigma=.5; nu=.2; tau=.2;
par(mgp=c(2,1,0),mar=c(4,4,4,1)+0.1)

#plot the pdf using plot
plot(function(x) dinf1NBF(x, mu=mu, sigma=sigma, nu=nu, tau=tau), from=0, to=20,
n=20+1, type="h",xlab="x",ylab="f(x)",cex.lab=1.5)
#--------------------------------------------------------------------------------

#plot the cdf using plot
cdf <- stepfun(0:19, c(0,pinf1NBF(0:19, mu=mu, sigma=sigma, nu=nu, tau=tau)), f = 0)
plot(cdf, xlab="x", ylab="F(x)", verticals=FALSE, cex.points=.8, pch=16, main="",cex.lab=1.5)
#--------------------------------------------------------------------------------

#plot the qdf using plot
invcdf <- stepfun(seq(0.01,.99,length=19), qinf1NBF(seq(0.1,.99,length=20),mu,        sigma), f = 0)
plot(invcdf, ylab=expression(x[p]==F^{-1}(p)), do.points=FALSE,verticals=TRUE,
     cex.points=.8, pch=16, main="",cex.lab=1.5, xlab="p")
#--------------------------------------------------------------------------------

# generate random sample
Ni <- rinf1NBF(1000, mu=mu, sigma=sigma, nu=nu, tau=tau)
 hist(Ni,breaks=seq(min(Ni)-0.5,max(Ni)+0.5,by=1),col="lightgray", main="",cex.lab=2)
barplot(table(Ni))
#--------------------------------------------------------------------------------
}

\keyword{distribution}
\keyword{regression}%




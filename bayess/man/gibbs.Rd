\name{gibbs}
\alias{gibbs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gibbs sampler and Chib's evidence approximation 
for a generic univariate mixture of normal distributions%%  ~~function to do ... ~~
}
\description{
This function implements a regular Gibbs sampling algorithm on the
posterior distribution associated with a mixture of normal distributions, 
taking advantage of the missing data structure. It then runs an averaging 
of the simulations over all permutations of the component indices in order
to avoid incomplete label switching and to validate Chib's representation
of the evidence. This function reproduces \code{gibbsnorm} as its first stage,
\emph{however it may be much slower because of its second stage}.
}
\usage{
gibbs(niter, datha, mix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{niter}{
number of Gibbs iterations%%
}
  \item{datha}{
sample vector%%     ~~Describe \code{varp} here~~
}
  \item{mix}{
list made of \code{k}, number of components, \code{p}, \code{mu},
and \code{sig}, starting values of the paramerers, all of size \code{k} (see
example below)
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{k }{number of components in the mixture (superfluous as it is invariant over the execution of the
R code)}
\item{mu }{matrix of the Gibbs samples on the \eqn{\mu_i}{mu} parameters}
\item{sig }{matrix of the Gibbs samples on the \eqn{\sigma_i}{sigma} parameters}
\item{prog }{matrix of the Gibbs samples on the mixture weights}
\item{lolik }{vector of the observed log-likelihoods along iterations}
\item{chibdeno }{denominator of Chib's approximation to the evidence (see example below)}
}
\references{
Chib, S. (1995) Marginal likelihood from the Gibbs output. %% ~put references to the literature/web site here ~
\emph{J. American Statist. Associ.} \bold{90}, 1313-1321.
}
\seealso{
\code{\link{gibbsnorm}}
}
\examples{
faithdata=faithful[,1]
mu=rnorm(3,mean=mean(faithdata),sd=sd(faithdata)/10)
sig=1/rgamma(3,shape=10,scale=var(faithdata))
mix=list(k=3,p=rdirichlet(par=rep(1,3)),mu=mu,sig=sig)
resim3=gibbs(100,faithdata,mix)
lulu=order(resim3$lolik)[100]
lnum1=resim3$lolik[lulu]
lnum2=sum(dnorm(resim3$mu[lulu,],mean=mean(faithdata),sd=resim3$sig[lulu,],log=TRUE)+
dgamma(resim3$sig[lulu,],10,var(faithdata),log=TRUE)-2*log(resim3$sig[lulu,]))+
sum((rep(0.5,mix$k)-1)*log(resim3$p[lulu,]))+
lgamma(sum(rep(0.5,mix$k)))-sum(lgamma(rep(0.5,mix$k)))
lchibapprox3=lnum1+lnum2-log(resim3$deno)
}
\keyword{Gibbs}
\keyword{mixture}
\keyword{evidence}
\keyword{Bayes factor}

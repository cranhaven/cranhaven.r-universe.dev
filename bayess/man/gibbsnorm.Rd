\name{gibbsnorm}
\alias{gibbsnorm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gibbs sampler for a generic mixture posterior distribution%%  ~~function to do ... ~~
}
\description{
This function implements the generic Gibbs sampler of Diebolt and Robert (1994)
for producing a sample from the posterior distribution associated
with a univariate mixture of \eqn{k}{k} normal components with all \eqn{3k-1}{3k-1} parameters
unknown.%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gibbsnorm(niter, dat, mix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{niter}{
number of iterations in the Gibbs sampler%%     ~~Describe \code{niter} here~~
}
  \item{dat}{
mixture sample
}
  \item{mix}{
list defined as \code{mix=list(k=k,p=p,mu=mu,sig=sig)},
where \code{k} is an integer and the remaining entries are
vectors of length \code{k}
}
}
\details{
Under conjugate priors on the means (normal distributions), variances (inverse gamma
distributions), and weights (Dirichlet distribution), the full conditional distributions
given the latent variables are directly available and can be used in a straightforward 
Gibbs sampler. This function is only the first step of the function \code{gibbs}, but it
may be much faster as it avoids the computation of the evidence via Chib's approach.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{k}{number of components (superfluous)}
  \item{mu}{Gibbs sample of all mean parameters}
  \item{sig}{Gibbs sample of all variance parameters}
  \item{p}{Gibbs sample of all weight parameters}
  \item{lopost}{sequence of log-likelihood values along Gibbs iterations}
%% ...
}
\references{
Chib, S. (1995) Marginal likelihood from the Gibbs output. %% ~put references to the literature/web site here ~
\emph{J. American Statist. Associ.} \bold{90}, 1313-1321.

Diebolt, J. and Robert, C.P. (1992) Estimation of finite mixture distributions by Bayesian sampling.
\emph{J. Royal Statist. Society} \bold{56}, 363-375.
}
\seealso{
\code{\link{rdirichlet}}, \code{\link{gibbs}}%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(datha)
datha=as.matrix(datha)
mix=list(k=3,mu=mean(datha),sig=var(datha))
res=gibbsnorm(10,datha,mix)
plot(res$p[,1],type="l",col="steelblue3",xlab="iterations",ylab="p")

}
\keyword{Gibbs}
\keyword{mixtures}
\keyword{conjugate priors}

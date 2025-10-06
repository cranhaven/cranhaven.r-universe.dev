\name{equilibrium}
\alias{equilibrium}
\title{
Nash Optimal Party Positions
}
\description{
Nash Optimal Party Positions
}
\usage{
equilibrium(start, model, data, tolerance = 1e-05, max.iter = 100, 
coal = 0, alpha = 0, margin = NULL, fixed = NULL, gamma = 0, 
boot = 0, MC = 0, self.var = "self", prox.var="prox", 
position=NULL, votes=NULL, quadratic=TRUE, conf.level = 0.95)
}
\arguments{
  \item{start}{initial party positions. Numerical vector. Optional.}
  \item{model}{the \code{mlogit} model analysis}
  \item{data}{the data set}
  \item{tolerance}{tolerance in the convergence of Nash equilibrium. Default \code{1e-5}}
  \item{max.iter}{max iteration to convergence in Nash equilibrium. Default \code{100}}
  \item{coal}{a \code{list} specificing electoral coalitions. See Details.}
  \item{alpha}{the weight of coalition vote-share in party utility function. Default = 0. See Details.}
  \item{margin}{a \code{list} specifing the vote share margin to be maximized of a party/coalition against other party/coalition. See Details.}
  \item{fixed}{a \code{list} of fixed party positions. See Details.}
  \item{gamma}{the weight among nash and fixed arty position. Default=0. See Details.}
  \item{boot}{number of boostrap replications. See Details.}
  \item{MC}{number of Monte Carlo replications. See Details.}
  \item{self.var}{\code{character}: name of self-placement of respondent. See Details.}
  \item{prox.var}{\code{character}: name of party-placement variable. See Details.}
  \item{position}{a named \code{list}: of perceived position of parties. See Details.}
  \item{votes}{a named \code{list}: of actual vote share at election. See Details.}
  \item{quadratic}{a logical value: if  \code{FALSE} the linear utility function is used to calculate the proximity. See Details.}
  \item{conf.level}{significant level for empirical Monte Carlo or bootstrap confidence intervals.}
}
\details{
See vignette.
}
\value{an object of class \code{nash.eq}
}
\references{
 Adams, James F., Samuel Merrill III, and Bernard Grofman (2005). A Unified Theory of Party Competition. Cambridge: Cambridge University Press
 
 Merrill, Samuel III, and James Adams (2001), Computing Nash Equilibria in Probabilistic, Multiparty Spatial Models with Nonpolicy Components, Political Analysis, 9, 347--61

 Curini, L., and Iacus, S.M. (2017) Nash Optimal Party Positions: The nopp R Package, Journal of Statistical Software, 81(11), 1--25
}
\author{
Luigi Curini, Stefano M. Iacus
}
\note{
See the vignette for detailed explanations and other working examples.
}


\seealso{
See Also as \code{\link{plot.nash.eq}}
}
\examples{
\dontrun{
data(italy2006)

str(italy2006)
italy2006[1:2,1:14]

election <- set.data(italy2006 , shape="wide", choice="vote",
varying=c(5:14), sep="_")
str(election)

m <- mlogit(vote~prox+partyID | gov_perf+sex+age+education, 
election, reflevel = "UL")
summary(m)

true.pos <- list(FI=7.59, UL=3.50, RC=1.95, AN=8.08, UDC=5.66)
true.votes <- list(FI=.24, UL=.40, RC=.10, AN=.18, UDC=.08)
# model 1: comparison against true votes and party positions
nash.eq <- equilibrium(model=m, data=election, pos=true.pos, 
 votes=true.votes)
nash.eq

par(mfrow=c(3,1))
plot(nash.eq)
par(mfrow=c(1,1))

# model 2: colation behaviours
coal1 <- list(FI=1, UL=2, RC=2, AN=1, UDC=1)
alpha1 <- list(FI=0.5, UL=0.5, RC=0.5, AN=0.5, UDC=0.5)
nash.eq <- equilibrium(model=m, data=election, coal=coal1, 
 alpha=alpha1)
nash.eq

# model 3: colation behaviours
coal1 <- list(FI=1, UL=2, RC=2, AN=1, UDC=1)
alpha1 <- list(FI=0.7, UL=0.8, RC=0.1, AN=0.5, UDC=0.9)
nash.eq <- equilibrium(model=m, data=election, coal=coal1, 
 alpha=alpha1)
nash.eq

# model 4: rivals tends to separate each other
nash.eq <- equilibrium(model=m, data=election, margin=list(FI="UL", UL="FI"))
nash.eq

# model 5: fixed position averaged with Nash equilibrium solution
nash.eq <- equilibrium(model=m, data=election, fixed=list(RC=1), gamma=0.2)
nash.eq

# model 6: rivals tends to separate each other with 
# fixed position averaged with Nash equilibrium solution
nash.eq <- equilibrium(model=m, data=election,  
margin=list(FI="UL", UL="FI"), fixed=list(RC=1), gamma=0.2)
nash.eq

# model 7: coalition and fixed position averaged with 
# Nash equilibrium solution
coal1 <- list(FI=1, UL=2, RC=2, AN=1, UDC=1)
alpha1 <- list(FI=0.7, UL=0.8, RC=0.5, AN=0.5, UDC=0.5)
nash.eq <- equilibrium(model=m, data=election,  coal=coal1, 
 alpha=alpha1, fixed=list(RC=1), gamma=0.2)
nash.eq

# model 8: Bootstrap analysis
set.seed(280715)
nash.eq <- equilibrium(model=m, data=election, boot=10)
nash.eq

# model 9: Monte Carlo simulation
set.seed(280715)
nash.eq <- equilibrium(model=m, data=election, MC=10)
nash.eq
}
}


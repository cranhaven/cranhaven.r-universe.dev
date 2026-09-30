\name{bapred-internal} 
\title{Internal bapred functions}
\alias{Dt}
\alias{VarInflation}
\alias{aprior}
\alias{avedistTwo}
\alias{bivprob}
\alias{bprior}
\alias{build.design}
\alias{design.mat}
\alias{diffexprmAfterBR}
\alias{emfahighdim}
\alias{extractAffybatch}
\alias{fuzzywilcox}
\alias{it.sol}
\alias{kldistTwo}
\alias{list.batch}
\alias{mypvcaBatchAssess}
\alias{nbfactors}
\alias{normalizeAffyBatchqntval}
\alias{normalizeqntadd}
\alias{normalizeqntadd2}
\alias{normalizeqntval}
\alias{plotcomp}
\alias{postmean}
\alias{postvar}
\alias{print.combat}
\alias{print.fabatch}
\alias{print.meancenter}
\alias{print.noba}
\alias{print.qunormtrain}
\alias{print.ratioa}
\alias{print.ratiog}
\alias{print.rmatrain}
\alias{print.standardize}
\alias{print.svatrain}
\alias{sepscoreTwo}
\alias{skewdivTwo}
\alias{summarizeadd2} 
\alias{summarizeval2}
\description{
  Internal bapred functions.
}
\usage{
Dt(rho) 
VarInflation(dta, Blist, maxnbfactors, dig) 
aprior(gamma.hat) 
avedistTwo(xb1, xb2) 
bivprob(rho, lower, upper = -lower, mean = 0) 
bprior(gamma.hat) 
build.design(vec, des = NULL, start = 2) 
design.mat(mod, numCov) 
diffexprmAfterBR(x, xbrlist, y, batch, batchessuited) 
emfahighdim(eps, nbf, minerr = 1e-06, maxiter = 100) 
extractAffybatch(inds, abo)
fuzzywilcox(x, y) 
it.sol(sdat, g.hat, d.hat, g.bar, t2, a, b, conv = 1e-04) 
kldistTwo(xb1, xb2) 
list.batch(mod) 
mypvcaBatchAssess(X, factordata, threshold) 
nbfactors(dta, maxnbfactors = 12, diagnostic.plot = FALSE, minerr = 0.001, 
  maxiter = 100) 
normalizeAffyBatchqntval(abatch, type = c("separate", "pmonly", "mmonly", "together")) 
normalizeqntadd(abo, qnt.scale)
normalizeqntadd2(x, qnt.scale)
normalizeqntval(x)
plotcomp(xp, groups, y, alpha, col = rainbow(length(groupsun)), 
  xlim, ylim, ...) 
postmean(g.hat, g.bar, n, d.star, t2) 
postvar(sum2, n, a, b) 
\method{print}{combat}(x, ...) 
\method{print}{fabatch}(x, ...) 
\method{print}{meancenter}(x, ...) 
\method{print}{noba}(x, ...) 
\method{print}{qunormtrain}(x, ...) 
\method{print}{ratioa}(x, ...) 
\method{print}{ratiog}(x, ...) 
\method{print}{rmatrain}(x, ...)
\method{print}{standardize}(x, ...) 
\method{print}{svatrain}(x, ...) 
sepscoreTwo(xb1, xb2, k = 10) 
skewdivTwo(xb1, xb2)
summarizeadd2(abo, probe.effects) 
summarizeval2(abo)
}
\details{
  These internal \pkg{bapred} functions are not usually called
  directly by the user.
}
\keyword{internal}

\name{validateRS-package}
\alias{validateRS-package}
\alias{validateRS}
\docType{package}
\title{
Multiple testing procedures for validation of rating systems. 
}
\description{
This package implements statistical tests described in the ECB Working paper ''Advances in multivariate back-testing for credit risk underestimation'', by F. Coppens, M. Mayer, L. Millischer, F.  Resch, S. Sauer, K. Schulze (ECB WP series, forthcoming).  The empirical results in the paper were obtained using the package's functions.   

The data on the rating classes, i.e. the PDs (table 1 of the ECB Working paper) and the scenarios for the class sizes (table 3 of the ECB Working paper), can be found in the object \code{ratingData}. 

The power of the test can be computed for different ''scenarios'' for the alternative hypothesis H1. There are three scenario's for H1 (see ECB Working paper, table 2): (1) H1A where H1 is obtained by increasing all the PDs under the null hypothesis towards 1, (2) H1B where H1 is obtained by increasing one PD at a time towards 1 and (3) H1C where the PDs under H1 are drawn from a multivariate normal distribution.  

H1A and H1B can be computed using the function \code{power.target.Nclasses} and H1C with the function \code{sample.knowledge.H1}. Examples are given below. 

Questions on the package can be asked via the 'stackexhange' network, in particular the 'crossvalidated' community. 
}


\author{
Coppens F., Mayer M., Millischer L., Resch F., Sauer S., Schulze K. 
}
\references{
F. Coppens, M. Mayer, L. Millischer, F.  Resch, S. Sauer, K. Schulze, ''Advances in multivariate back-testing for credit risk underestimation'',  ECB WP series, forthcoming
}

\examples{
\dontrun{
data(ratingData)
rc.selected<-1:5
si.selected<-1:2
p.0<-ratingData$p.0[rc.selected]
sizes<-ratingData$sizes[si.selected,rc.selected]

# ===============================================
# compute alternative hypothesis H1A
# ===============================================
H1A<-power.target.Nclasses(p.0=p.0, size=sizes[1,], 
                           N=length(p.0), 
                           target=0.50)

# compute minP's region and its power under H1A (the latter must be close to 0.5)
r.mp<-region.acceptance(hypo.test="minP", p.0=p.0, size=sizes[1,], alpha=0.05)
region.power(region=r.mp, p.1=H1A[1,])

# compute enhanced test's region and its power under H1A 
# (result in table 4 of the ECB Working paper)
r.mpp<-region.acceptance(hypo.test="minPp", p.0=p.0, size=sizes[1,], alpha=0.05)
region.power(region=r.mpp, p.1=H1A[1,])

# compute envelope test's region and its power under H1A 
# (result in table 4 of the ECB Working paper)
r.sh<-region.acceptance(hypo.test="sterneHull", p.0=p.0, size=sizes[1,], alpha=0.05)
region.power(region=r.sh, p.1=H1A[1,])

# ===============================================
# compute alternative hypothesis H1B
# ===============================================
H1B<-power.target.Nclasses(p.0=p.0, size=sizes[1,], 
                           N=1, 
                           target=0.30)

# compute minP's region and its power under H1B (the latter must be close to 0.3)
r.mp<-region.acceptance(hypo.test="minP", p.0=p.0, size=sizes[1,], alpha=0.05)
power.vec<-vector(mode="numeric", length=nrow(H1B))
for ( i in 1:nrow(H1B) ) {
  power.vec[i]<-region.power(region=r.mp, p.1=H1B[i,])
}
cat(paste("mean :", mean(power.vec), "\n"))

# compute enhanced test's region and its power under H1B 
# (result in table 4 of the ECB Working paper)
r.mpp<-region.acceptance(hypo.test="minPp", p.0=p.0, size=sizes[1,], alpha=0.05)
power.vec<-vector(mode="numeric", length=nrow(H1B))
for ( i in 1:nrow(H1B) ) {
  power.vec[i]<-region.power(region=r.mpp, p.1=H1B[i,])
}
cat(paste("mean :", mean(power.vec), "\n"))

# compute envelope test's region and its power under H1B 
# (result in table 4 of the ECB Working paper)
r.sh<-region.acceptance(hypo.test="sterneHull", p.0=p.0, size=sizes[1,], alpha=0.05)
power.vec<-vector(mode="numeric", length=nrow(H1B))
for ( i in 1:nrow(H1B) ) {
  power.vec[i]<-region.power(region=r.sh, p.1=H1B[i,])
}
cat(paste("mean :", mean(power.vec), "\n"))


# ===============================================
# compute alternative hypothesis H1C
# ===============================================
param<-par.dist.default(dist="tr.normal", p.0=p.0)
set.seed(1)
sample.h1<-sample.knowledge.H1(n=10000, par=param, p.0=p.0)

sim.mp<-simul.scenario.rs(hypo.test="minP", p.0=p.0, sampleH1=sample.h1, sizes=sizes, alpha=0.05)
sim.mp$power$weighted.average

sim.mpp<-simul.scenario.rs(hypo.test="minPp", p.0=p.0, sampleH1=sample.h1, sizes=sizes, alpha=0.05)
sim.mpp$power$weighted.average
}
}


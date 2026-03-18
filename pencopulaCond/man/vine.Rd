\name{vine}
\alias{vine}
\title{"Estimating Non-Simplified Vine Copulas Using Penalized Splines"}
\description{Estimating Non-Simplified Vine Copulas Using Penalized Splines}
\usage{
vine(data,d=2,d2=2,D=4,D3=6,lambda=c(100,50),type="Rvine",order.Dvine=FALSE,m=2,
cores=NULL,q=1,mod.cond=TRUE,max.iter=51,fix.lambda=FALSE,RVM=NULL,cal.cond=FALSE,
id=NULL,test.ind=FALSE,test.cond=2,lambda.search=FALSE,lam1.vec=NULL,lam2.vec=NULL)
}
     
\arguments{
  \item{data}{'data' contains the data. 'data' has to be a matrix or a
    data.frame with two columns.}
  \item{d}{refers to the hierachy level of the marginal hierarchical
    B-spline for copulas in the first tree of the vine, default is d=2.}
  \item{d2}{refers to the hierachy level of the marginal hierarchical
    B-spline for copulas in the second tree and in the following trees of the vine, default is d2=2.}
  \item{D}{referes to the maximum hierachy level for copulas in the first tree of the vine, default is D=4. If
    D<d, it follows D<-d.}
  \item{D3}{referes to the maximum hierachy level for copulas in the second tree and in the following trees of the vine, default is D3=6.}
  \item{lambda}{Starting values for lambda, first start values for copulas in the first tree, second start value for copulas in the second tree and in the following trees of the vine, default is lambda=c(100,50).}
  \item{type}{Default is type="Rvine", fitting a regular vine copula. An alternative is type="Dvine", fitting a D-vine copula.}
  \item{order.Dvine}{Only relevant for type="Dvine". Indicating if the first level of the Dvine is
    ordered, default order.Dvine=TRUE.}
  \item{m}{Indicating the order of differences to be penalised. Default is "m=2".}
  \item{cores}{Default=NULL, the number of cpu cores used for parallel computing can be specified.}
  \item{q}{Degree of B-splines. Default is q=1.}
  \item{mod.cond}{Default=TRUE. If mod.cond=FALSE each pair-copula in the vine is estimated as simplified copula. The argument test.cond varies the test for the simplyfing assumption, which is imported from the R-package pacotest.}
  \item{max.iter}{maximum number of iteration, the default is max.iter=51.}
  \item{fix.lambda}{Default=FALSE, using the algorithm in the paper for estimating the optimal penalty parameter. If fix.lambda=TRUE, lambda is constant throughout the estimation.}
  \item{RVM}{Default=NULL. If RVM is a RVine-Matrix, this matrix determines the structure of the vine.}
  \item{cal.cond}{Default=FALSE. If cal.cond=TRUE each copula in the second tree and in the following trees of the vine is estimated as conditional copula.}
  \item{id}{Optional, one set id to any value. Especially important for simulations, starting with several starting values for lambda.}
  \item{test.ind}{Default=FALSE. If test.ind=TRUE, the fitted log-likelihood of each pair-copula is evaluated. If ("log.like"/"n"<0.001), where "n" is the sample size, the program set the corresponding pair copula as independence copula. We do not use this in our simulations or applications in the article.}
  \item{test.cond}{If test.cond=2, testType='ECORR' is chosen for the test of the simplyfing assumption as proposed in the article. There is an additional second test available in the R-package pactotest. testType="VI" is chosen with test.cond=1.}
  \item{lambda.search}{TRUE/FALSE, indicating if a search about several starting values for lambda should be performed. If search is selected, the starting value 'lambda' does not work anymore.}
  \item{lam1.vec}{Vector of candidate values for penalty parameter lambda for copulas in the first tree of the vine}.
  \item{lam2.vec}{Vector of candidate values for penalty parameter lambda for copulas in the second tree and in the following trees of the vine}.
}
\details{The calculation of the vine is done stepwise. The specifications in 'vine' are done for every paircopula in the
  vine with the identical specification. There is no option to change parameters for some pair-copulas.
}
\value{Returning a list containing
  \item{vine}{The estimated vine copula, an object of class 'pencopulaCond'}
  \item{log.like}{the estimated log-likelihood}
  \item{log.like.vec}{A vector with the estimated log.like.vec of each pair-copula}
  \item{AIC}{AIC value}
  \item{AIC.vec}{A vector with the estimated AIC of each pair-copula}
  \item{cAIC}{corrected AIC value}
  \item{cAIC.vec}{A vector with the estimated cAIC of each pair-copula}
  \item{d}{Used d}
  \item{d2}{Used d2}
  \item{D}{Used D}
  \item{D3}{Used D3}
  \item{order}{the used order of the first level (reported only for D-vines)}
  \item{S}{Sequence seq(1:(dim(data)[2]))}
  \item{N}{Number of observations, that is dim(data)[1]}
  \item{base}{Used basis function}
  \item{q}{Used degree of the B-spline basis}
  \item{no.cond.dens}{Estimated number of condtional copulas}
  \item{pca}{Indicating the used number of pca}
  \item{D.struc}{Used D.struc}
  \item{type}{Selected type of the vine copula}
  \item{VineMatrix}{VineMatrix, reported for type="Rvine"}
  }

\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}
\references{Flexible Copula Density Estimation with Penalized
  Hierarchical B-Splines, Kauermann G., Schellhase C. and Ruppert, D. (2013), Scandinavian Journal of Statistics 40(4), 685-705.

Estimating Non-Simplified Vine Copulas Using Penalized Splines, Schellhase, C. and Spanhel, F. (2017), Statistics and Computing.}

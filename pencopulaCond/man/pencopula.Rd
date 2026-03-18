\name{pencopula}
\alias{pencopula}

\title{Calculating penalized (conditional) copula density with penalized hierarchical B-splines}
\description{Calculating penalized (conditional) copula density with penalized hierarchical B-splines }
\usage{
pencopula(data,d=3,D=d,q=1,base="B-spline",max.iter=20,test.ind=FALSE,
         lambda=c(100,100),pen.order=2,data.frame=parent.frame(),cond=FALSE,
         fix.lambda=FALSE,id=NULL) 
}

\arguments{
  \item{data}{'data' contains the data. 'data' has to be a matrix or a
    data.frame. The number of columns of 'data' is p.}
  \item{d}{refers to the hierachy level of the marginal hierarchical
    B-spline, default is d=3.}
  \item{D}{referes to the maximum hierachy level, default is D=3. If
    D<d, it follows D<-d.}
  \item{q}{degree of the marginal hierarchical B-spline.}
  \item{base}{By default, the used marginal basis is a
    'B-spline'. Second possible option is 'Bernstein', using a Bernstein
    polynomial basis.}
  \item{max.iter}{maximum number of iteration, the default is max.iter=20.}
  \item{test.ind}{Default=FALSE. If test.ind=TRUE, the fitted log-likelihood of each pair-copula is evaluated. If ("log.like"/"n"<0.001), where "n" is the  sample size, the program set the corresponding pair copula as independence copula. We do not use this in our simulations or applications in the article.}
  \item{lambda}{p-dimensional vector of penalty parameters, the values
    can be different. Default is lambda=c(100,100).}
  \item{pen.order}{The order of differences for the penalization,
    default is pen.order=2.}
  \item{data.frame}{reference to the data. Default reference is the parent.frame().}
  \item{cond}{Determining if a conditional copula is estimated. Default=FALSE, only suitable for p=3.}
  \item{fix.lambda}{Default=FALSE, using the algorithm in the paper for estimating the optimal penalty parameter. If fix.lambda=TRUE, lambda is constant throughout the estimation.}
 \item{id}{Optional, one set id to any value. Especially important for simulations, starting with several starting values for lambda.}
}


\value{
Returning an object of class pencopula. The class pencopula consists of
the environment 'penden.env', which includes all calculated values of
the estimation approach.
For a fast overview of the main results, one can use the function 'print.pencopula()'.
}


\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}

\keyword{nonparametric}

\references{Flexible Copula Density Estimation with Penalized
  Hierarchical B-Splines, Kauermann G., Schellhase C. and Ruppert, D. (2013), Scandinavian Journal of Statistics 40(4), 685-705.

Estimating Non-Simplified Vine Copulas Using Penalized Splines, Schellhase, C. and Spanhel, F. (2017), Statistics and Computing.}

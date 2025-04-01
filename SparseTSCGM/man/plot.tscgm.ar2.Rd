
\name{plot.tscgm.ar2}
\alias{plot.tscgm.ar2}
\title{
Plot sparse.tscgm objects from fitting chain graphical models with vector 
autoregressive process of order 2. 
}
\description{
  \command{plot.tscgm.ar2} is a generic plot function that is adapted
  for objects of class \code{sparse.tscgm}.
}
\usage{
 \method{plot}{tscgm.ar2}(x, mat=c("precision","autoregression1", "autoregression2"),...)
}

\arguments{
  \item{x}{
    an object of class \code{sparse.tscgm}. 
     }
  \item{mat}{
   Name of matrix to be plotted,i.e., either the precision matrix 
   or vector autoregression matrices of lag 1 or 2.
  }
  
  \item{\dots}{
    Arguments to be passed to graphical parameters (see \code{\link{par}}).
  }
}

\value{
Undirected or directed networks.
}
\author{
Fentaw Abegaz and Ernst Wit
}


\seealso{
  \code{\link{network}}
}
\examples{
## Data generation from time series chain graphical model with vector 
## autoregressive model of order 2
seed = 321
datas <- sim.data(model="ar2", time=10,n.obs=20, n.var=5,seed=seed,prob0=0.35,
         network="random")
data.fit <-  datas$data1
\donttest{
## Model fitting with vector autoregressive order 2
 res.tscgm <- sparse.tscgm(data=data.fit, lam1=NULL, lam2=NULL, nlambda=NULL, 
 model="ar2", penalty="scad",optimality="bic_mod",control=list(maxit.out = 10, maxit.in = 100))
  
#Network visualization
oldpar<-par(mfrow=c(3,2))
plot.tscgm.ar2(datas, mat="precision",main="True precision matrix")
plot.tscgm.ar2(res.tscgm, mat="precision",main="Estimated precision matrix")
     
plot.tscgm.ar2(datas, mat="autoregression1", 
        main="True autoregression coef. matrix of lag 1" )    
plot.tscgm.ar2(res.tscgm, mat="autoregression1",
           main="Estimated autoregression coef. matrix of lag 1")
            
plot.tscgm.ar2(datas, mat="autoregression2",
      main="True autoregression coef. matrix of lag 2")    
plot.tscgm.ar2(res.tscgm, mat="autoregression2",
           main="Estimated autoregression coef. matrix of lag 2") 
par(oldpar)
 }
}


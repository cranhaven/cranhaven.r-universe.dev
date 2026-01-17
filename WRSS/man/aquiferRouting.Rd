\name{aquiferRouting}
\alias{aquiferRouting}
\title{
  base function for aquifer simulation
}
\description{
  Given a sort of demand(s), \code{aquiferRouting} function simulates a lumped and simple model of an unconfined aquifer under an optional givn recharge time series, \code{rechargeTS}, and specific yield, \code{Sy}.
}
\usage{
  aquiferRouting(demand, priority = NA, area, volume,
                 rechargeTS = NA, leakageFraction = NA,
                 initialStorage = NA, Sy, simulation)
}
\arguments{
  \item{demand}{(optional) A matrix: is column-wise matrix of demands, at which the rows present demands for each monthly time step and columns are for different individual demand sites (MCM).}
  \item{priority}{(optional) A vector: is a vector of priorities associated to \code{demand}}
  \item{area}{The area of aquifer (Km^2)}
  \item{volume}{The aquifer volume (MCM)}
  \item{rechargeTS}{(optional) A vector : a vector of water flowing into the aquifer (MCM)}
  \item{leakageFraction}{(optional) The leakage coeffcient of aquifer storage. The leakage is computed as the product of \code{leakageFraction} and aquifer storage. It is in \code{[0, 1]} interval}
  \item{initialStorage}{(optional) The initial volume of aquifer at the first step of the simulation (MCM). If missing, the function iterates to carry over the aquifer}
  \item{Sy}{Specific yield (default: 0.1)}
  \item{simulation}{A list: \code{simulation} is a list of three vectors: \code{start}, \code{end}, and \code{interval}. the \code{start} and \code{end} components must be in \code{'YYYY-MM-DD'} format. the \code{interval} component can takes either of \code{'day','week'}, or \code{'month'}.}
}
  \value{
    the \code{aquiferRouting} function returns a list of objects as bellow:
      \itemize{
        \item \code{release}: a matrix of release(s) equivalant to each \code{demand} (MCM) 
        \item \code{leakage}: a vector of leakage time series (MCM) 
        \item \code{storage}: a vector of storage time series (MCM) 
      }
  }
  \references{
    Mart nez-Santos, P., and J. M. Andreu. "Lumped and distributed approaches to model natural recharge in semiarid karst aquifers." Journal of hydrology 388.3 (2010): 389-398.
  }
  \author{
    Rezgar Arabzadeh
  }
  
  \seealso{\code{\link[WRSS]{reservoirRouting}}}
  \examples{
    
    area           <-200
    leakageFraction<-0.01
    Sy             <-0.15
    volume         <-20000
    priority       <-c(3,1,1,2)
    rechargeTS     <-rnorm(120,60,8)
    demand         <-matrix(rnorm(480,10,3),120)
    simulation     <-list(start='2000-01-01',end='2009-12-29',interval='month')
    
    res<-
      aquiferRouting(demand         =demand         ,
                     priority       =priority       ,
                     area           =area           ,
                     volume         =volume       ,
                     rechargeTS     =rechargeTS     ,
                     leakageFraction=leakageFraction,
                     Sy             =Sy             ,
                     simulation     =simulation)
    
    plot(res$storage$storage,ylab='Storage (MCM)',xlab='time steps(month)',type='o')
  }
  \keyword{list}
  \keyword{matrix}
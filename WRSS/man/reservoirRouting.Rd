\name{reservoirRouting}
\alias{reservoirRouting}
\title{
  base function for reservoir simulation
}
\description{
  Given a sort of demand(s), \code{reservoirRouting} function simulates the effect of a dam  under givn hydrometeorological time series, e.g. \code{inflow} and \code{netEvaporation}, on the drainage network.
}
\usage{
  reservoirRouting(type='storage',
                   inflow,
                   netEvaporation=NA,
                   demand=NA,
                   priority=NA,
                   seepageFraction=NA,
                   geometry=list(storageAreaTable=NULL,
                                 storageElevationTable=NULL,
                                 dischargeElevationTable=NULL,
                                 deadStorage=0,
                                 capacity=NULL),
                   plant=list(installedCapacity=NULL,
                              efficiency=NULL,
                              designHead=NULL,
                              designFlow=NULL,
                              turbineAxisElevation=NULL,
                              submerged=FALSE,
                              loss=0),
                   penstock=list(diameter=NULL,
                                 length=0,
                                 roughness=110),
                   initialStorage=NA,
                   simulation)
}
\arguments{
  \item{type}{A string: the type of the reservoir being instantiated: by default 'storage', however, it can be 'hydropower'}
  \item{inflow}{A vector : a vector of water flowing into the diversion (MCM)}
  \item{netEvaporation}{A vector: is a vector of net evaporation depth time series at the location of dam site (meter). If omitted, the evaporation is assumed to be zero.}
  \item{demand}{A matrix: is column-wise matrix of demands, at which the rows presents demands for each monthly time steps and columns are for different individual demand sites (MCM).}
  \item{priority}{(optional) A vector: is a vector of priorities associated to \code{demand}}
  \item{seepageFraction}{(optional) The seepage coeffcient of reservoir storage. The seepage is computed as the product of \code{seepageFraction} and reservoir storage.}
  \item{geometry}{A list of reservoir geometric specifications:
      \itemize{
        \item \code{storageAreaTable}: is a matrix whose first column includes reservoir volume (MCM) for different elevation levels and the second column contains reservoir area (in Km^2) corresponding to the first column
        \item \code{storageElevationTable}: is a matrix whose first column includes reservoir volume (MCM) for different elevation levels and the second column contains elevation (in meter) corresponding to the first column
        \item \code{dischargeElevationTable}: is a matrix whose first column includes the capacity of reservoir tailwater discharge rate (in cms) for different elevation levels and the second column contains elevation levels corresponding to the first column, required if the \code{type = 'hydropower'} and the item \code{submerged = TRUE}
        \item \code{deadStorage}: refers to water in a reservoir that cannot be drained by gravity through the dam outlet works (MCM)
        \item \code{capacity}: The maximum capacity of the reservoir}
  }
  \item{plant}{A list of power plant specifications. It is provided if \code{type = 'hydropower'}:
      \itemize{
        \item \code{efficiency}: is a matrix whose first column includes discharge rate (in cms) and the second column turbine effeciency, in \code{[0 1]} interval, corresponding to the first column
        \item \code{designHead}: A vector of length of two, containing the minimum and maximum design water head (in meter) of the turbine respecively, that the it is in active state
        \item \code{designFlow}: A vector of length of two, containing the minimum and maximum design flow rate (in cms) of the turbine respecively, that the it is in active state
        \item \code{turbineAxisElevation}: The elevation of axis of the installed turbine (in meter)
        \item \code{submerged}: logical: if the turbine is of type of submeged on, \code{TRUE}, otherwise, \code{FALSE}
        \item \code{loss}: losses associated with the turbine (in meter)}
  }
  \item{penstock}{(optional) A list of penstock specifications. It is provided if \code{type = 'hydropower'}
    \itemize{
      \item \code{diameter}: The diameter of the penstock (in meter)
      \item \code{length}: The length of the penstock (in meter)
      \item \code{roughness}: pipe roughness coefficient used for Hazen-Williams formulation}
  }
  \item{initialStorage}{(optional) The initial stored water at the reservoir in the first step of the simulation (MCM). If is missing the the function iterate to carry over the reservoir.}
  \item{simulation}{A list: \code{simulation} is a list of three vectors: \code{start}, \code{end}, and \code{interval}. the \code{start} and \code{end} components must be in \code{'YYYY-MM-DD'} format. the \code{interval} component can takes either of \code{'day','week'}, or \code{'month'}.
  }
}
  \value{
    the \code{reservoirRouting} function returns a list of features given as folows: 
    \itemize{
      \item \code{release}: a matrix of release(s) equivalant to each \code{demand} (MCM)
      \item \code{spill}  : a vector of spilage time series (MCM) 
      \item \code{seepage}: a vector of steepage time series (MCM) 
      \item \code{storage}: a vector of storage time series (MCM) 
      \item \code{loss}   : a vector of evaporation loss time series (MCM) 
     }
  }
  \references{
    Yeh, William WG. "Reservoir management and operations models: A state of the art review." Water resources research 21.12 (1985): 1797-1818.
  }
  \author{
    Rezgar Arabzadeh
  }
  
  \seealso{\code{\link[WRSS]{aquiferRouting}}}
  
  \examples{
    type           <-c('storage','hydropower')
    demand         <-matrix(rnorm(480,10,3),120)
    priority       <-sample(1:3,4,replace=TRUE)
    inflow         <-rlnorm(120,log(50),log(4))
    netEvaporation <-rnorm(120,0.4,0.1)
    simulation     <-list(start='2000-01-01',end='2009-12-29',interval='month')
    seepageFraction<-0.05
    geometry       <-list(storageAreaTable=cbind(seq(0,100,10),seq(0,10,1)),
                          storageElevationTable=cbind(seq(0,100,10),seq(0,200,20)),
                          dischargeElevationTable=cbind(seq(0,50,10),seq(0,10,2)),
                          deadStorage=50,
                          capacity=100)
    plant          <-list(installedCapacity=50,
                          efficiency=cbind(c(5,25,45),c(0.5,0.9,0.7)),
                          designHead=c(100,200),
                          designFlow=c(10,40),
                          turbineAxisElevation=5,
                          submerged=TRUE,
                          loss=2)
    penstock       <-list(diameter=2,
                          length=50,
                          roughness=110)
    
    #-----Storage Reservoir----------
    reservoirRouting(type=type[1],
                     inflow=inflow,
                     netEvaporation=netEvaporation,
                     demand=demand,
                     priority=priority,
                     seepageFraction=seepageFraction,
                     geometry=geometry,
                     plant=plant,
                     penstock=penstock,
                     simulation=simulation)
\dontrun{
    ##-----Takes Several Minutes----------

    #-----Hydropower Reservoir with demand----------
    reservoirRouting(type=type[2],
                     inflow=inflow,
                     netEvaporation=netEvaporation,
                     demand=demand,
                     priority=priority,
                     seepageFraction=seepageFraction,
                     geometry=geometry,
                     plant=plant,
                     penstock=penstock,
                     simulation=simulation)
    #-----Hydropower Reservoir----------
    reservoirRouting(type=type[2],
                     inflow=inflow,
                     netEvaporation=netEvaporation,
                     priority=priority,
                     seepageFraction=seepageFraction,
                     geometry=geometry,
                     plant=plant,
                     penstock=penstock,
                     simulation=simulation)
    
  }
  }
  \keyword{ list }
  \keyword{ matrix }
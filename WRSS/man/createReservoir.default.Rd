\name{createReservoir.default}
\alias{createReservoir.default}
\title{
  default function for class of \code{createReservoir}
}
\description{
 this function constructs an object from class of \code{createReservoir}, which is able to simulate a storage reservoir under given a sort of demand(s).
}
\usage{
  \method{createReservoir}{default}(type='storage',
                          name='unknown',
                          priority=NA,
                          downstream=NA,
                          netEvaporation=NA,
                          seepageFraction=NA,
                          seepageObject=NA,
                          geometry=list(storageAreaTable=NULL,
                                        storageElevationTable=NULL,
                                        dischargeElevationTable=NULL,
                                        deadStorage=NULL,
                                        capacity=NULL),
                          plant=list(installedCapacity=NULL,
                                     efficiency=NULL,
                                     designHead=NULL,
                                     designFlow=NULL,
                                     turbineAxisElevation=NULL,
                                     submerged=FALSE,
                                     loss=0),
                          penstock=list(diameter=NULL,
                                        length=NULL,
                                        roughness=110),
                          initialStorage=NA)
}
\arguments{
  \item{type}{A string: the type of the reservoir being instantiated: by default 'storage', however, it can be 'hydropower'}
  \item{name}{(optional) A string: the name of the reservoir.}
  \item{priority}{(optional) An integer: the supplying priority. \code{priority} is a value in [1, 99] interval. If missing, the \code{priority} is set to Inf.}
  \item{downstream}{(optional) An object; from either of classes of \code{\link[WRSS]{createAquifer}} , \code{\link[WRSS]{createRiver}}, \code{\link[WRSS]{createReservoir}}, \code{\link[WRSS]{createJunction}}, \code{\link[WRSS]{createDiversion}}, or \code{\link[WRSS]{createDemandSite}}; which spillage volume pours to it.}
  \item{netEvaporation}{A vector: is a vector of net evaporation depth time series at the location of dam site (meter). If omitted, the evaporation is assumed to be zero.}
  \item{seepageFraction}{(optional) The seepage coeffcient of reservoir storage. The seepage is computed as the product of \code{seepageFraction} and reservoir storage. It is in \code{[0, 1]} interval}
  \item{seepageObject}{(optional) An object; from either of classes of \code{\link[WRSS]{createAquifer}} , \code{\link[WRSS]{createRiver}}, \code{\link[WRSS]{createReservoir}}, \code{\link[WRSS]{createJunction}}, \code{\link[WRSS]{createDiversion}}, or \code{\link[WRSS]{createDemandSite}};  which seepage volume pours to it.}
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
          \item \code{installedCapacity}: the plant installed capacity (MW)
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
}

\value{
An object from class of \code{createReservoir}
}

\seealso{\code{\link[WRSS]{createReservoir}}}
\keyword{ list }
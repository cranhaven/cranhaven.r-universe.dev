\name{getIntamapParams}
\alias{getIntamapParams}
\title{ Setting parameters for the intamap package}
\description{ This function sets a range of the parameters for the \code{\link{intamap-package}}, 
to be included in the object described in \code{\link{intamap-package}}}

\usage{getIntamapParams(oldPar,newPar,...)
}
\arguments{
\item{oldPar}{An existing set of parameters for the interpolation process, 
              of class \code{IntamapParams} or a list of parameters for modification
              of the default parameters}
\item{newPar}{A \code{list} of parameters for updating \code{oldPar} or for
              modification of the default parameters. 
              Possible parameters with their defaults are given below}
\item{...}{Individual parameters for updating \code{oldPar} or for
              modification of the default parameters.
              Possible parameters with their defaults are given below 
\describe{
\item{\code{doAnisotropy = FALSE}}{Defining whether anisotropy should be calculated}
\item{\code{removeBias = NA}}{Defining whether biases should be removed, and in case yes, which ones
                   (\code{localBias} and \code{regionalBias} implemented}
\item{\code{addBias = NA}}{Defining which biases to be added in the \code{\link{postProcess}} function.
                    This has not yet been implemented.}
\item{\code{biasRemovalMethod = "LM"}}{character; specifies which methods to use to remove bias. See below.}
\item{\code{doSegmentation = FALSE}}{Defining if the predictions should be subject to segmentation.
                 Segmentation has been implemented, but not the use of it.}
\item{\code{testMean}}{logical; for copula method only; whether or not the predictive means 
                       (if calculated) should be tested for being reasonable}
\item{\code{nmax = 50}}{for local kriging: the number of nearest observations that
            should be used for a kriging prediction or simulation, where
            nearest is defined in terms of the space of the spatial locations.
            By default, 50 observations are used. }
\item{\code{maxdist = Inf}}{for local kriging: Maximum distance to neighbouring locations to be used in kriging
                or simulations}
\item{\code{ngrid = 100}}{The number of grid points to be used if an Averaged Cumulative
             Distribution Function (ACDF) needs to be computed for unbiased kriging}
\item{\code{nsim=100}}{Number of simulations when needed}
\item{\code{block = numeric(0)}}{Block size; a vector with 1, 2 or 3 values containing the size
                  of a rectangular in x-, y- and z-dimension respectively
                  (0 if not set), or a data frame with 1, 2 or 3 columns,
                  containing the points that discretize the block in the
                  x-, y- and z-dimension to define irregular blocks relative to
                  (0,0) or (0,0,0) - see also the details section of \code{\link[gstat]{predict.gstat}}.
                  By default, predictions or simulations refer to the support of the data values. }
\item{\code{processType = "gaussian"} }{If known - the distribution of the data. Defaults to gaussian,
                   analytical solutions also exists in some cases for logNormal.
                   This setting only affects a limited number of methods, e.g. the
                   block prediciton}
\item{\code{confProj = FALSE}}{If set, the program will attempt conform projections in \code{\link{preProcess}},
                calling the function \code{\link{conformProjections}}.}
\item{\code{debug.level = 0}}{Used in some functions for giving additional output. See 
                individual functions for more information. }
\item{\code{nclus = 1}}{it is possible to use parallel processing for some interpolation methods
        (currently only the copula method), nclus defines the number of processes to 
        spawn. This requires previous installation of the \code{doParallel} package}
\item{\code{...}}{Additional parameters that do not exist in the default parameter set,
           this could be parameters necessary for new methods within or outside the 
           \code{\link{intamap-package}}
           }
}
}
}
\value{ 
A list of the parameters with class \code{intamapParams} to be included in the 
\code{object} described in \code{\link{intamap-package}}}




\references{ 

Pebesma, E., Cornford, D., Dubois, G., Heuvelink, G.B.M., Hristopulos, D., Pilz, J., Stohlker, U., Morin, G., Skoien, J.O. INTAMAP: The design and implementation f an interoperable automated interpolation Web Service. Computers and Geosciences 37 (3), 2011. 

}

\author{ Jon Olav Skoien }
\note{
This function will mainly be called by \code{\link{createIntamapObject}}, but 
can also be called by the user to create a parameter set or update an 
existing parameter set. If none of the arguments is a list of class
\code{IntamapParams}), the function will assume that the argument(s) are
modifications to the default set of parameters.

If the function is called with two lists of parameters (but the first one is
not of class \code{IntamapParams}) they are both seen as modifications to the 
default parameter set. If they share some parameters, the parameter values from
the second list will be applied.


}
\seealso{
\code{\link{createIntamapObject}}
}
\examples{
# Create a new set of intamapParameters, with default parameters:
params = getIntamapParams()
# Make modifications to the default list of parameters
params = getIntamapParams(newPar=list(removeBias = "local",
              secondParameter = "second"))
# Make modifications to an existing list of parameters
params = getIntamapParams(oldPar = params,newPar = list(predictType = list(exc=TRUE)))

}
\keyword{spatial}

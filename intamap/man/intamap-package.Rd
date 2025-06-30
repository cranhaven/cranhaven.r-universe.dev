\name{intamap-package}
\alias{intamap-package}

\title{A package providing methods for automatic interpolation:
pre-processing, parameter estimation, spatial prediction and post processing }

\description{
This package provides functionality for automatic interpolation of spatial data. 
The package was originally developed as the computational back-end of the intamap web service, but 
is now a stand-alone package as maintenance of the web service has ended. 
}

\section{ General setup }{

The normal work flow for working with the \code{intamap} package can best be illustrated
with the following R-script. The procedure starts with reading data and meta data,
then setting up an object which is used in the following functions: preprocess data,
estimate parameters, compute spatial predictions, and post process them
(i.e., write them out):

\preformatted{
library(intamap)
library(sf)
# set up intamap object, either manually:
data(meuse)
coordinates(meuse) = ~x + y
data(meuse.grid)
gridded(meuse.grid) = ~x + y
obj = list(
        observations = meuse,
        predictionLocations = meuse.grid,
        targetCRS = "+init=epsg:3035",
        params = getIntamapParams()
)
class(obj) = c("idw")

# or using createIntamapObject
obj = createIntamapObject(
        observations = meuse,
        predictionLocations = meuse.grid,
        targetCRS = "+init=epsg:3035",class = c("idw")
)




# run test:
checkSetup(obj)

# do interpolation steps:
obj = preProcess(obj)
obj = estimateParameters(obj) # faster
obj = spatialPredict(obj)
obj = postProcess(obj)
}

Our idea is that a script following this setup will allow the full
statistical analysis required for the R back-end to the automatic
interpolation service, and provides the means to extend the current
(over-simplistic) code with the full-grown statistical analysis routines
developed by INTAMAP partners. Running the package independently under
R gives the user more flexibility in the utilization than what is possible 
through the web-interface.

Let us look into detail what the code parts do:
\preformatted{
library(intamap)
}
The command \code{library(intamap)} loads the R code of the \code{intamap}
package to the current R session, along with the packages required for
this (sp, gstat, akima, automap, mvtnorm, evd, MASS). After the retirement
of rgdal, it is recommended to use sf-objects from the sf package.
All packages need to be available to the
R session, which is possible after downloading them from 
the Comprehensive R Network Archives (CRAN) (\url{https://cran.r-project.org})

\preformatted{
# set up intamap object:
obj = createIntamapObject(
        observations = meuse,
        predictionLocations = meuse.grid,
        targetCRS = "+init=epsg:3051", 
        class = "idw"
)
}

This code sets up a list object called \code{obj}, and assigns a class
(or a group of classes) to it. This list should hold anything we need in the next steps, and the
bare minimum seems to be measured point data (which will be extended to 
polygon data) and prediction locations,
and a suggestion what to do with it. Here, the data are read from a
PostGIS data base running on localhost; data base connections over a
network are equally simple to set up. From the data base \code{postgis}
the tables \code{eurdep.data} and \code{inspire1km.grid} are read; it
is assumed that these have their SRID (spatial reference identifier) set.

The suggestion what to do with these data is put in the classes,
\code{idw}. This will determine which \emph{versions} of \code{preProcess},
\code{parameterEstimate} etc will be used: \code{intamap} provides \emph{methods}
for each of the \emph{generic} functions 
\code{preProcess}, 
\code{estimateParameters},
\code{spatialPredict}, 
\code{postProcess}.
Although it would be possible to apply two classes in this case (\code{dataType} in addition to \code{idw}), 
as the choice of pre- and post-processing steps 
tend to be data-dependent, we have tried to limit the number of classes to one for most applications.

The S3 method mechanism (used here) hence requires these versions to
be called \code{preProcess.idw}, \code{estimateParameters.idw},
\code{spatialPredict.idw}, and \code{postProcess.idw} (and eventually 
also \code{preProcess.eurdep} and \code{preProcess.eurdep}).

To see that, we get in an interactive session:
\preformatted{
> library(intamap)
Loading required package: sp
Loading required package: gstat
Geospatial Data Abstraction Library extensions to R successfully loaded
> methods(estimateParameters)
[1] estimateParameters.automap*         estimateParameters.copula*         
[3] estimateParameters.default*         estimateParameters.idw*            
[5] estimateParameters.linearVariogram* estimateParameters.transGaussian*  
[7] estimateParameters.yamamoto*           
}

Now if a partner provides additional methods for BayesianKriging, one could integrate them by
\preformatted{
class(obj) = "BayesianKriging"
}

and provide some or all of the functions
\code{preProcess.BayesianKriging},
\code{estimateParameters.BayesianKriging},
\code{spatialPredict.BayesianKriging}, and
\code{postProcess.BayesianKriging}, which would be called automatically
when using their generic form (\code{preProcess} etc).  

It is also possible to provide a method that calls another
method. Further, for each generic there is a default method. For
\code{estimateParameter} and \code{spatialPredict} these print an
error message and stop, for the pre- and postprocessing the default
methods may be the only thing needed for the full procedure; if no
\code{preProcess.BayesianKriging} is found, \code{preProcess.default}
will be used when the generic (\code{preProcess}) is called.

If a method does something, then it adds its result to the object it
received, and returns this object. If it doesn't do anything, then it
just passes (returns) the object it received.

To make these different methods exchangable, it is needed that they can
all make the same assumptions about the contents of the object that they
receive when called, and that what they return complies with what the
consequent procedures expect. The details about that are given in the
descriptions of the respective methods, below.

Because a specific interpolation method implemented may have its peculiar
characteristics, it may have to extend these prescriptions by passing
more information than described below, for example information about
priors from \code{estimateParameters} to \code{spatialPredict}.

The choice between methods is usually done based on the type of problem
(extreme values present, computation time available etc.). The possibility
for parallel processing of the prediction step is enabled for some of the main methods.
To be able to take advantage of multiple CPUs on a computer, the package
\code{doParallel} must be installed, additionally the parameter nclus must be set to
a value larger than 1.

}

\section{Input object components}{
\describe{
\item{\code{observations}}{ object of class 
                \code{\link[sp:SpatialPoints]{SpatialPointsDataFrame}}, containing
                a field \code{value} that is the target variable.}
\item{\code{predictionLocations}}{ object extending class \code{\link[sp:Spatial-class]{Spatial}}, containing
prediction locations.}
\item{\code{targetCRS}}{ character; target CRS or missing}
\item{\code{formulaString}}{formula string for parameter estimation and prediction functions}  
\item{\code{params}}{\code{list} of parameters, to be set in \code{\link{getIntamapParams}}. These parameters include: }

\describe{
\item{doAnisotropy = FALSE}{Defining whether anisotropy should be calculated}
\item{removeBias = NA}{Defining whether biases should be removed, and in case yes, which ones
                   (\code{localBias} and \code{regionalBias} implemented}
\item{addBias = NA}{Defining which biases to be added in the \code{\link{postProcess}} function.
                    This has not yet been implemented.}
\item{biasRemovalMethod = \code{"LM"}}{character; specifies which methods to use to remove bias. See below.}
\item{doSegmentation = FALSE}{Defining if the predictions should be subject to segmentation.
                 Segmentation has been implemented, but not the use of it.}
\item{nmax = 50}{for local kriging: the number of nearest observations that
            should be used for a kriging prediction or simulation, where
            nearest is defined in terms of the space of the spatial locations.
            By default, 50 observations are used. }
\item{ngrid = 100}{The number of grid points to be used if an Averaged Cumulative
             Distribution Function (ACDF) needs to be computed for unbiased kriging}
\item{nsim=100}{Number of simulations when needed}
\item{block = numeric(0)}{Block size; a vector with 1, 2 or 3 values containing the size
                  of a rectangular in x-, y- and z-dimension respectively
                  (0 if not set), or a data frame with 1, 2 or 3 columns,
                  containing the points that discretize the block in the
                  x-, y- and z-dimension to define irregular blocks relative to
                  (0,0) or (0,0,0) - see also the details section of \code{\link[gstat]{predict.gstat}}.
                  By default, predictions or simulations refer to the support of the data values. }
\item{processType = \code{"gaussian"} }{If known - the distribution of the data. Defaults to gaussian,
                   analytical solutions also exists in some cases for logNormal.
                   This setting only affects a limited number of methods, e.g. the
                   block prediciton}
\item{confProj = FALSE}{If set, the program will attempt conform projections in \code{\link{preProcess}},
                calling the function \code{\link{conformProjections}}.}
\item{nclus = 1}{The number of clusters to use, if applying to a method which can 
   run processes in parallel. Currently implemented for methods \code{automap},
   \code{copula} and \code{psgp}.}
\item{debug.level = 0}{Used in some functions for giving additional output. See 
                individual functions for more information. }
\item{...}{Additional parameters that do not exist in the default parameter set,
           particularly parameters necessary for new methods within the \code{intamap}
           package}
}
}
}
\references{ 

Pebesma, E., Cornford, D., Dubois, G., Heuvelink, G.B.M., Hristopulos, D., Pilz, J., Stohlker, U., Morin, G., Skoien, J.O. INTAMAP: The design and implementation f an interoperable automated interpolation Web Service. Computers and Geosciences 37 (3), 2011. 

}
\keyword{spatial}

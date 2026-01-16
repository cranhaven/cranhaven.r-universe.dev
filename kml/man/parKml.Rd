\name{parKml}
\Rdversion{1.1}
\alias{parKml}
\alias{parALGO}
\alias{[<-,ParKml,ANY,ANY,ANY-method}
\alias{[,ParKml,ANY,ANY-method}
\title{ ~ Function: parKml ~}

\description{
  \code{parKml} and \code{parALGO} are constructor for the object \code{\linkS4class{ParKml}}.
}
\usage{
parKml(saveFreq,maxIt,imputationMethod,distanceName,power,distance,
   centerMethod,startingCond,nbCriterion,scale)

parALGO(saveFreq=100,maxIt=200,imputationMethod="copyMean",
   distanceName="euclidean",power=2,distance=function(){},
   centerMethod=meanNA,startingCond="nearlyAll",nbCriterion=1000,scale=TRUE)
}
\arguments{
  \item{saveFreq}{\code{[numeric]}: Long computations can take several
    days. So it is possible to save the object \code{\linkS4class{ClusterLongData}}
    on which works \code{\link{kml}} once in a while. \code{saveFreq}
    defines the frequency of the saving
    process. The \code{ClusterLongData} is saved every \code{saveFreq}
    clustering calculations. The object is saved in the file
    \code{objectName.Rdata} in the curent folder. If \code{saveFreq} is
    set to \code{Inf}, the object is never saved.
  }
  \item{maxIt}{\code{[numeric]}: Set a limit to the number of iteration if
    convergence is not reached.
  }
  \item{imputationMethod}{\code{[character]}: the calculation of quality
    criterion can not be done if some value are
    missing. \code{imputationMethod} define the method use to impute the
    missing value. See \code{\link[longitudinalData]{imputation}} for detail.
  }
  \item{distanceName}{\code{[character]}: name of the
    \code{distance} used by k-means. If the \code{distanceName} is one of
    "manhattan", "euclidean", "minkowski", "maximum", "canberra" or
    "binary", a compiled optimized version specificaly design for
    trajectories version is used. Otherwise, the function define in
    the slot \code{distance} is used.
  }
  \item{power}{\code{[numeric]}: If \code{distanceName="minkowski"}, this define
    the power that will be used.
  }
  \item{distance}{\code{[numeric <- function(trajA,trajB)]}: function that computes the
    distance between two trajectories. If no function is specified, the Euclidian
    distance with Gower adjustment (to deal with missing value) is
    used.
  }
  \item{centerMethod}{\code{[numeric <-
      function(vector(numeric))]}: k-means algorithm computes the centers of
    each cluster. It is possible to personalize the definition of
    "center" by defining a function "centerMethod". This function should
    take a vector of numeric as argument and return a single numeric -the
    center of the vector-.
  }
  \item{startingCond}{\code{[character]}: specifies the starting
    condition. Should be one of "randomAll", "randomK", "maxDist",
    "kmeans++", "kmeans+", "kmeans-" or "kmeans--" (see
    \code{\link[longitudinalData]{initializePartition}} for details). It
    also could take two specifics values: "all" stands for
    c("maxDist","kmeans-") then an alternance of "kmeans--" and
    "randomK" while "nearlyAll" stands for
    "kmeans-" then an alternance of "kmeans--" and "randomK".
  }

  %  \item{distanceStartingCond}{\code{[numeric <- function(trajA,trajB)]}: some starting condition needs
 %   to compute the distance matrix of the
  %  trajectories. \code{distanceStartingCond} define the distance that will be
   % use to calculate this matrix. See \code{\link{partitionInitialise}} for detail.
  %}
  \item{nbCriterion}{\code{[numeric]}: set the maximum number of
    quality criterion that are display on the graph (since displaying
    a high criterion number an slow down the overall process). The
    default value is 100.
  }

  \item{scale}{\code{[logical]}: if TRUE, then the data will be
    automaticaly scaled (using the function \code{\link{scale}} with
    default values) before the execution of k-means on joint
    trajectories. Then the data
    will be restore (using the function \code{\link[longitudinalData]{restoreRealData}})
    just before the end of the function \code{\link[kml3d]{kml3d}}. This option
    has no effect on \code{\link{kml}}.
  }
}

\details{
  \code{parKml} is the constructor of object \code{\linkS4class{ParKml}}.
}
\value{
  An object \code{\linkS4class{ParKml}}.
}



\examples{
\donttest{
### Move to tempdir
wd <- getwd()
setwd(tempdir()); getwd()

### Generation of some data
cld1 <- generateArtificialLongData()

### Setting two different set of option :
(option1 <- parALGO())
(option2 <- parALGO(distanceName="maximum",centerMethod=function(x)median(x,na.rm=TRUE)))

### Running kml We suspect 3, 4 or 5 clusters, we want 3 redrawing.
kml(cld1,3:5,3,toPlot="both",parAlgo=option1)
kml(cld1,3:5,3,toPlot="both",parAlgo=option2)

### Go back to current dir
setwd(wd)
}
}

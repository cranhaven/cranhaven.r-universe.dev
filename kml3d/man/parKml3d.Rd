\name{parKml3d}
\alias{parKml3d}

\title{ ~ Function: parKml3d ~}

\description{
  \code{parKml3d} is a constructor of object \code{ParKml} (from package \code{kml})
  that provide adequate default value for the use of function \code{\link{kml3d}}.
}
\usage{
parKml3d(saveFreq = 100, maxIt = 200, imputationMethod = "copyMean",
    distanceName = "euclidean3d", power = 2, distance = function() {
    }, centerMethod = meanNA, startingCond = "nearlyAll", nbCriterion =100,scale=TRUE)
}
\arguments{
  \item{saveFreq}{\code{[numeric]}: Long computations can take several
    days. So it is possible to save the object \code{\linkS4class{ClusterLongData3d}}
    on which works \code{\link{kml3d}} once in a while. \code{saveFreq}
    defines the frequency of the saving
    process. The \code{ClusterLongData3d} is saved every \code{saveFreq}
    clustering calculations. The object is saved in the file
    \code{objectName.Rdata} in the curent folder.
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
    \code{distance} used by k-means. If the \code{distanceName} is "euclidean3d", a compiled optimized version specificaly design for
    joint-trajectories version is used. Otherwise, the function define in
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

  \item{nbCriterion}{\code{[numeric]}: set the maximum number of
    quality criterion that are display on the graph (since displaying
    a high criterion number an slow down the overall process, the
    default value is 100).
  }

  \item{scale}{\code{[logical]}: if TRUE, then the data will be
    automaticaly scaled (using the function \code{\link{scale}} with
    default values) before the execution of k-means on joint
    trajectories. Then the data
    will be restore (using the function \code{\link[longitudinalData]{restoreRealData}})
    just before the end of the function \code{\link[kml3d]{kml3d}}. This option
    has no effect on \code{\link[kml]{kml}}.
  }
}

\details{
  \code{parKml3d} is a constructor of object \code{ParKml} (from package \code{kml})
  that provide adequate default value for the use of function \code{\link{kml3d}}.
}
\value{
  An object \code{ParKml} (see package \code{kml}).
}


\examples{
### Move to tempdir
wd <- getwd()
setwd(tempdir()); getwd()

### Generation of some data
cld1 <- generateArtificialLongData3d(c(15,15,15))

### Setting two different set of option :
(option1 <- parKml3d())
(option2 <- parKml3d(centerMethod=function(x)median(x,na.rm=TRUE)))

### Running kml. Formaly, the second exemple is 'k-median'
kml3d(cld1,4,1,toPlot="both",parAlgo=option1)
kml3d(cld1,4,1,toPlot="both",parAlgo=option2)

### Go back to current dir
setwd(wd)
}

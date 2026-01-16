\name{ParKml-class}
\Rdversion{1.1}
\docType{class}
\alias{ParKml-class}
\alias{[,ParKml-method}
\alias{[<-,ParKml-method}
%\alias{show,ParKml-method}

\title{~ Class: "ParKml" ~ }
\description{\code{ParKml} is an object containing some parameter used by \code{\link{kml}}.}

\section{Slots}{
  \describe{
    \item{\code{saveFreq}}{\code{[numeric]}: Long computations can take several
      days. So it is possible to save the object \code{\linkS4class{ClusterLongData}}
      on which works \code{\link{kml}} once in a while. \code{saveFreq}
      defines the frequency of the saving
      process. The \code{ClusterLongData} is saved every \code{saveFreq}
      clustering calculations. The object is saved in the file
      \code{objectName.Rdata} in the curent folder. If \code{saveFreq} is
    set to \code{Inf}, the object is never saved.
    }
    \item{\code{maxIt}:}{\code{[numeric]}: Set a limit to the number of iteration if
      convergence is not reached.
    }
    \item{\code{imputationMethod}:}{\code{[character]}: the calculation of quality
      criterion can not be done if some value are
      missing. \code{imputationMethod} define the method use to impute the
      missing value.
      See \code{\link[longitudinalData]{imputation}} for detail.
    }
    \item{\code{distanceName}:}{\code{[character]}: name of the
      \code{distance} used by k-means. If the \code{distanceName} is one of
      "manhattan", "euclidean", "minkowski", "maximum", "canberra" or
      "binary", a compiled optimized version specificaly design for
      trajectories version is used. Otherwise, the function define in
      the slot \code{distance} is used.
    }
    \item{\code{power}:}{\code{[numeric]}: If \code{distanceName="minkowski"}, this define
      the power that will be used.
    }
    \item{\code{distance}:}{\code{[numeric <- function(trajA,trajB)]}: function that computes the
      distance between two trajectories. This field is used only if
      'distanceName' is not one of the classical function.
    }
    \item{\code{centerMethod}:}{\code{[numeric <-
	function(vector(numeric))]}: k-means algorithm computes the centers of
      each cluster. It is possible to personalize the definition of
      "center" by defining a function "centerMethod". This function should
      take a vector of numeric as argument and return a single numeric -the
      center of the vector-.
    }
    \item{\code{startingCond}:}{\code{[character]}: specifies the starting
      condition. Should be one of "randomAll", "randomK", "maxDist",
      "kmeans++", "kmeans+", "kmeans-" or "kmeans--" (see
      \code{\link[longitudinalData]{initializePartition}} for details). It
      also could take two specifics values: "all" stands for
      c("maxDist","kmeans-") then an alternance of "kmeans--" and
      "randomK" while "nearlyAll" stands for
      "kmeans-" then an alternance of "kmeans--" and "randomK".
    }
%    \item{\code{distanceStartingCond}:}{\code{[numeric <- function(trajA,trajB)]}: some starting condition needs
 %     to compute the distance matrix of the
  %    trajectories. \code{distanceStartingCond} define the distance that will be
   %   use to calculate this matrix. See \code{\link{partitionInitialise}} for detail.
    %}
    \item{\code{nbCriterion}}{\code{[numeric]}: set the maximum number of
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
}

\section{Methods}{
  \describe{
    \item{\code{object['xxx']}}{Get the value of the field \code{xxx}.}
  }
}


\examples{
\donttest{
### Move to tempdir
wd <- getwd()
setwd(tempdir()); getwd()

### Building data
myCld <- gald()

### Standard kml
kml(myCld,,3,toPlot="both")

### Using median instead of mean
parWithMedian <- parALGO(centerMethod=function(x){median(x,na.rm=TRUE)})
kml(myCld,,3,toPlot="both",parAlgo=parWithMedian)

### Using distance max
parWithMax <- parALGO(distanceName="maximum")
kml(myCld,,3,toPlot="both",parAlgo=parWithMax)

### Go back to current dir
setwd(wd)
}
}

\keyword{classes}

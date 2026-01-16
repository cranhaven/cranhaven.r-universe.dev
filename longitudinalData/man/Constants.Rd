\name{Constants}
\alias{MAX_CLUSTERS}
\alias{CLUSTER_NAMES}
\alias{CRITERION_MIN_OR_MAX}
\alias{CRITERION_NAMES}
\alias{DISTANCE_METHODS}
\alias{CHOICE_STYLE}

\docType{data}
\title{~ Constants ~}
\description{
  Constants define in the package ~
}
\usage{
MAX_CLUSTERS
%CLUSTERS_NAMES
%CRITERION_MIN_OR_MAX
CRITERION_NAMES
DISTANCE_METHODS
CHOICE_STYLE
}

\value{
  MAX_CLUSTERS = 26

  CLUSTER_NAMES = paste("c",2:MAX_CLUSTERS,sep="")

  CRITERION_NAMES <- c(\cr
    "Calinski.Harabatz","Kryszczuk.Calinski","Genolini.Calinski","Ray.Turi","Davies.Bouldin",\cr
    "BIC","BIC2","AIC","AICc","AICc2","postProbaGlobal","random"\cr
  )

%  CRITERION_MIN_OR_MAX <- c(Calinski.Harabatz=1,Kryszczuk.Calinski=1,Genolini.Calinski=1,Ray.Turi=-1,Davies.Bouldin=1,random=1)

  DISTANCE_METHODS = c("manhattan", "euclidean", "minkowski", "maximum", "canberra", "binary")

  CHOICE_STYLE = list(\cr
    typeTraj=c("l","l","n"),\cr
    colTraj=c("clusters","black","black"),\cr
    typeMean=c("b","b","b","b","l","l","n"),\cr
    colMean=c("clusters","black","clusters","black","clusters","black","black"),\cr
    pchMean=c("letters","letters","symbols","symbols","letters","letters","letters")\cr
  )\cr
}

\examples{
### Maximum number of clusters that kml can deal with
MAX_CLUSTERS

### Names of the field that save clusters in object 'ClusterLongData'
cat(CLUSTER_NAMES,"\n")

### List of the available criterion
CRITERION_NAMES

### Distance available
DISTANCE_METHODS[2]

### Define the style use by choice
CHOICE_STYLE[['typeTraj']][2]
}
\keyword{datasets}

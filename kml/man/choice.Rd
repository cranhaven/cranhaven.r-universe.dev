\name{choice}
\alias{choice}
\alias{[,ParChoice-method}
\alias{choice,ClusterLongData-method}

\title{ ~ Function: choice ~}

\description{
  \code{choice} lets the user choose some \code{Partition} he wants to export.
}

\usage{
choice(object, typeGraph = "bmp")
}

\arguments{
  \item{object}{\code{[ClusterLongData]}: Object containing the trajectories and all the
   %\code{\linkS4class{Partition}} 
   \code{Partition} found by \code{kml}.}

  \item{typeGraph}{\code{[character]} for every selected \code{Partition}, \code{choice} export some graphs. \code{typeGraph} set
    the format that will be used. Possible formats are the ones available for \code{\link{savePlot}}.}
}

\details{
  \code{choice} is a function that lets the user see the 
  %\code{\linkS4class{Partition}} 
  \code{Partition} found by \code{kml}.
  At first, \code{choice} opens a graphics window (for Linux user, the windows should be explicitly
  open using \code{x11(type = "Xlib")}). On the left side, all
  the \code{Partition} contain in \code{Object} are ploted by a
  number (the number of cluster of the Partition). The level of the
  number is proportionnal to a quality criteria (like Calinski &
  Harabatz). One \code{Partition} is 'active', it is the one marked by a
  black dot.\cr

  On the right side, the trajectories of Object are drawn, according to the active \code{Partition}.

  From there, \code{choice} offers numerous options :
  \describe{
    \item{Arrow}{Change the active \code{Partition}.}
    \item{Space}{Select/unselect a \code{Partition} (the selected
      \code{Partition} are surrounded by a circle).}
    \item{Return}{Export all the selected \code{Partition}, then
      quit the function \code{choice}.}
    \item{'e'}{Change the display (Trajectories alone / quality
      criterion alone / both)}
    \item{'d'}{Change actif criterion.}
    \item{'c'}{Sort the Partition according to the actif criterion.}
    \item{'r'}{Change the trajectories' style.}
    \item{'f'}{Change the means trajectories's style.}
    \item{'g/t'}{Change the symbol size.}
    \item{'y/h'}{Change the number of symbols.}
  }

  When 'return' is pressed (or 'm' using Linux), the selected \code{Partition} are
  exported. Exporting is done in a specific named
  \code{objectName-Cx-y} where x is the number of cluster and y is the
  order in the sublist. Four files are created
  :
  \describe{
    \item{objectName-Cx-y-Clusters.csv}{Table with two columns. The first is the
      identifier of each trajectory (idAll); the second holds the cluster's
      affectation of the trajectory.}
    \item{objectName-Cx-y-Detail.csv}{Table containing information about the
      clusteration (percentage of individual
      in each cluster, various qualities criterion, algorithm used to
      find the partition and convergence time.)}
    \item{objectName-Cx-y-Traj.bmp}{Graph representing the
      trajectories. All the parameters set during the visualization (color
      of the trajectories, symbols used, mean color) are used
      for the export. Note that the 'typeGraph' argument can be used to
      export the graph in a format different than 'bmp'.}
    \item{objectName-Cx-y-TrajMean.bmp}{Graph representing the
      means trajectories of each clusterss. All the parameters set during the visualization (color
      of the trajectories, symbols used, mean color) are used
      for the export.}
  }

  This four file are created for each selected Partition. In addition, two
  'global' graphes are created :
  \describe{
    \item{objectName-criterionActif.bmp}{Graph presenting the values of
      the criterionActifall for all the Partition.}
    \item{objectName-criterionAll.bmp}{For each cluster's number, the
      first Partition is considered. This graph presents on a single
      display the values of all the criterion
      for each first Partition. It is helpfull to compare the various qualities
      criterion.}
  }
}

\value{For each selected \code{Partition}, four files are saved, plus two global files.}



\seealso{
  Overview: \code{\link{kml-package}} \cr
  Classes : \code{\linkS4class{ClusterLongData}}, 
  %\code{\linkS4class{Partition}} \cr
  \code{Partition} in package longitudinalData \cr
  Methods : \code{\link{kml}}\cr
  Plot : \code{\link{plot}}
}


\examples{
### Move to tempdir
wd <- getwd()
setwd(tempdir()); getwd()

### Creation of artificial data
cld1 <- gald(25)

### Clusterisation
kml(cld1,3:5,nbRedrawing=2,toPlot='both')

### Selection of the clustering we want
#     (note that "try" is for compatibility with CRAN only,
#     you probably can use "choice(cld1)")
try(choice(cld1))

### Go back to current dir
setwd(wd)
}

\keyword{iplot}    % Interacting with Plots
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{nonparametric} % Nonparametric Statistics [w/o 'smooth']
\keyword{ts}       % Time Series

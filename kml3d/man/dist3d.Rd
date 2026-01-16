\name{dist3d}
\alias{dist3d}

\title{
   ~ Function: dist3d ~
}
\description{
  Compute the distante between two joint trajectories.
}
\usage{
dist3d(x, y, method = "euclidian", power = 2)
}

\arguments{
  \item{x}{\code{[matrix(numeric)]}: first trajectory. The colomn are
    time, the line are variables.}
  \item{y}{\code{[matrix(numeric)]}: second trajectory. The colomn are
    time, the line are variables.}
  \item{method}{\code{[character]}: method used. Should be one of the
    method used by the function \code{\link[stats:dist]{dist}}.}
  \item{power}{\code{[numeric]}: if \code{method="minkowski"},
  \code{power}is the power used.}
}

\details{
  Compute the distante between two joint trajectories, using one of the
  distance define by \code{\link[stats:dist]{dist}}.
}

\value{
A numeric
}

\examples{
  ### Move to tempdir
  wd <- getwd()
  setwd(tempdir()); getwd()

  ### Generate artificial data
  myCld <- gald3d()

  ### Distance between individual 1 and 3 (there are in the same group)
  dist3d(myCld['traj'][1,,],myCld['traj'][3,,])

  ### Distance between individual 1 and 51 (there are in two different groups)
  dist3d(myCld['traj'][1,,],myCld['traj'][51,,])
  
  ### Go back to current dir
  setwd(wd)
}
\name{timeout}
\alias{timeout}
\title{
timeout
}
\description{
This function limits elapsed time spent on single model development. It uses low-level functions of parallel packege and sets the fork process with time limit. If the result is not returned within set time, it kills fork. Function shouldn't be called from R console.
The function is not used under Windows OS. Only Unix-like systems have fork functionality.
}
\usage{
timeout(..., seconds)
}

\arguments{
  \item{...}{
Expression to be time limited
}
  \item{seconds}{
Number of seconds
}

}
\author{
Original code by Jeroen Ooms <jeroen.ooms at stat.ucla.edu> of OpenCPU package.
Modifications by Jakub Szlek and Aleksander Mendyk.
}

\keyword{ methods }
\keyword{ error }
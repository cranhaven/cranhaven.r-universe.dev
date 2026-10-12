\name{OH2012}
\alias{OH2012}
\title{Body Mass Estimates Using O'Gorman & Hone (2012)}
\description{This function is meant for dinosaur mass estimation from femoral length as presented by O'Gorman & Hone (2012)}
\usage{OH2012(FL, eqn, data=NULL)}
\arguments{
  \item{FL}{a value or vector representing femoral length (in mm)}
  \item{eqn}{a character specifying the desired prediction equation (see Details)}
  \item{data}{an optional object of \code{class = "data.frame"} or \code{class = "matrix"}}
}
\details{Possible estimation equations (\code{eqn =}) include: \code{"theropods"}, \code{"sauropods"}, and \code{"ornithischians"}. See O'Gorman & Hone (2012) for specific details and Campione & Evans (2020) for evaluations of the models.
If a data object is specified, the mass estiamtes will be added as a column to the data.frame or matrix within that object.}
\value{
	\item{OH2012}{A vector representing the mass estimate(s) in log grams and grams}
}
\author{Nicolas E. Campione}
\references{
	O'Gorman, E. J. and Hone, D. W. E.  (2012) Body Size Distribution of the Dinosaurs. PLoS ONE 7(12): e51925.
	
	Campione, N. E. and Evans, D. C. (2020) The accuracy and precisions of body mass estimation in non-avian dinosaurs. Biological Reviews.
}
\seealso{
	\code{\link{cQE}}
	\code{\link{QE}}
}

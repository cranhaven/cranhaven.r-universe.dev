\name{animalEKF-package}
\alias{animalEKF-package}
\alias{animalEKF}
\docType{package}
\title{
\packageTitle{animalEKF}
}
\description{
%\packageDescription{animalEKF}
Synthetic generation of 1-D and 2-D correlated random walks (CRWs)
 for animal movement with behavioral switching, and particle filter estimation
 of movement parameters from observed trajectories using Extended Kalman Filter (EKF) model.
 See Ackerman (2018)\cr \url{https://digital.library.temple.edu/digital/collection/p245801coll10/id/499150}.
}
%\details{
%The DESCRIPTION file:
%\packageDESCRIPTION{animalEKF}
%\packageIndices{animalEKF}
%Simulation and Estimation of EKFs for Animal Movement
%}
%\details{
%\tabular{ll}{
%Package: \tab animalEKF\cr
%Type: \tab Package\cr
%Version: \tab 1.1\cr
%Date: \tab 2020-09-21\cr
%License: \tab GPL (>=2)\cr
%Description: \tab \packageDESCRIPTION{animalEKF}
%}
%\author{
%\packageAuthor{animalEKF}
%
%Maintainer: \packageMaintainer{animalEKF}
%}
\references{
Ackerman, Samuel.  "A Probabilistic Characterization of Shark Movement Using Location Tracking Data."  Temple University doctoral thesis, 2018.  \url{https://digital.library.temple.edu/digital/collection/p245801coll10/id/499150}

Carvalho, Carlos M., Johannes, Michael S., Lopes, Hedibert F., and Nicholas G. Polson.  "Particle Learning and Smoothing."  Statistical Science, 2010.

Espinoza, Mario, Farrugia, Thomas J., Lowe, Christopher G.  "Habitat Use, Movements and Site Fidelity of the Gray Smooth-Hound Shark in a Newly Restored Southern California Estuary." Journal of Experimental Marine Biology and Ecology, 2011.

}
\note{
I am indebted to Dr. Mario Espinoza and co-authors for providing the shark observation data that inspired this work, and for allowing it to be included in this package.  I am
especially grateful to Dr. Espinoza for many discussions regarding the nuances of modeling animal movement, particularly for suggesting the idea of behavioral switching models.

I am grateful to my doctoral thesis advisors, Dr. Marc Sobel, Dr. Richard Heiberger, and Dr. Mike O'Connor for supervising my research in this topic.  I am particularly grateful to 
Dr. Heiberger for his many hours in advising me on package design and on the \code{shiny} simulations in this package. 

}


\keyword{ EKF }
\keyword{ particle filter }
\keyword{ animal movement }
\keyword{ telemetry }

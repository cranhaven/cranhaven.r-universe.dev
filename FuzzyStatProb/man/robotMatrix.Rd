% File FuzzyStatProb/man/robotMatrix.Rd

\name{robotMatrix}
\docType{data}
\alias{robotStates}
\alias{transRobot}
\alias{linguisticTransitions}
\title{Transition matrix of a Markov chain that guides the movement of an autonomous patrolling robot}
\description{
Transition matrix of a Markov chain that guides the movement of an autonomous patrolling robot. The chain
has been computed according to game-theoretic techniques as the equilibrium solution of a leader-follower game
between a potential intruder and a patroller. See the references section.
}
\author{Pablo J. Villacorta Iglesias, Department of Computer Science and Artificial Intelligence, University of Granada (Spain).

\email{pjvi@decsai.ugr.es} - \url{http://decsai.ugr.es/~pjvi/r-packages.html}
}
\usage{
robotStates
transRobot
linguisticTransitions
}
\format{\code{transRobot} is a 10x10 2D matrix, \code{linguisticTransitions} is another 10x10 matrix of strings,
and \code{robotStates} is a vector of state names of length 10.}
\details{
In the game-theoretic patrolling model proposed in Amigoni et al., the equilibrium solution of the leader-follower game is a Markov chain
that can be computed by solving a set of independent linear programming problems. The transition probabilites are described in Fig. 1 of Amigoni et al.
\code{linguisticTransitions} is a matrix of labels whose names should match the tags of the \code{fuzzynumbers} list argument in the
call to \code{\link{fuzzyStationaryProb}} when \code{linguisticTransitions} is passed as first argument.
}
\references{
Amigoni, F., Basilico, N., Gatti, N. Finding the Optimal Strategies for Robotic Patrolling
with Adversaries in Topologically-Represented Eenvironments. In Proc. of ICRA 2009, pp. 819-824.
}
\keyword{fuzzy}
\keyword{Markov chain}
\keyword{transition matrix}
\keyword{stationary probability}
\keyword{probabilities}
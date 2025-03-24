\name{penta}
\alias{penta}
\title{
C code for solving pentadiagonal linear equations.
}
\description{
This function is only intended for an internal use.
}
\usage{
penta(dim, E, A, D, C, F, B, X)
}

\arguments{
	\item{dim}{vector containing dimension of linear system.}
	\item{E}{Internal vector storing for one of the sub-diagonals.}
	\item{A}{Internal vector storing for one of the sub-diagonals.}
	\item{D}{Internal vector storing for one of the sub-diagonals.}
	\item{C}{Internal vector storing for one of the sub-diagonals.}
	\item{F}{Internal vector storing for one of the sub-diagonals.}
	\item{B}{Internal vector storing for the right hand side of linear equation.}
	\item{X}{Vector to store the solution.}
}

\value{
Does not return anything. Changes the inputs according to the algorithm.
}

\author{Arun Kumar Kuchibhotla, arunku@wharton.upenn.edu.}

\keyword{Pentadiagonal Equation Solving}
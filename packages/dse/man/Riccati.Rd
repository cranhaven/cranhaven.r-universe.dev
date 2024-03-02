\name{Riccati}
\alias{Riccati}
\title{Riccati Equation}
\description{Solve a Matrix Riccati Equation}
\usage{
    Riccati(A, B, fuzz=1e-10, iterative=FALSE)
}
\arguments{
    \item{A}{A matrix.}
    \item{B}{A matrix.}
    \item{fuzz}{The tolerance used for testing convergence.}
    \item{iterative}{If TRUE an iterative solution technique is used.}
}
\value{xxx}
\details{
    Solve  Riccati equation  P = APA' + B 
    by eigenvalue decompostion of a symplectic matrix or by iteration.
}
\references{
    Vaccaro, R. J. and Vukina, T. (1993),
      A Solution to the Positivity Problem in the State-Space Approach 
      to Modeling Vector-Valued Time Series. 
      \emph{Journal of Economic Dynamics and Control}, \bold{17}, 401--421.

    Anderson, B. D. O. and Moore, J. B. (1979) \emph{Optimal Filtering}. 
    Prentice-Hall. (note sec 6.7.)  

    Vaughan, D. (1970) A Nonrecursive Algebraic Solution for the Discrete
    Riccati Equation. \emph{IEEE Tr AC}, 597--599.

    Laub, A. J. (1983) Numerical Aspects of Solving Algebraic Riccati Equations
      \emph{Proc IEEE conf Decision and Control}, 183--186. 

    Gudmundsson T., Kenney, C., and Laub, A. J. (1992) Scaling of the
    Discrete-Time Algebraic Riccati Equation to Enhance Stability of the Schur
    Solution Method  \emph{IEEE Tr AC}, \bold{37}, 513--518.
}
\seealso{
    \code{\link{eigen}}
}
\note{This procedure has not been tested.}

\concept{DSE}
\keyword{ts}
\keyword{algebra}


\name{distFrechet}
\alias{distFrechet}
%\alias{distFrechetRec}

\title{~ Function: Frechet distance ~}

\description{
Compute Frechet distance between two trajectories.
}
\usage{
distFrechet(Px,Py,Qx, Qy, timeScale=0.1, FrechetSumOrMax = "max")
%distFrechetRec(Px,Py,Qx, Qy, FrechetSumOrMax = "max")
}

\arguments{
  \item{Px}{[vector(numeric)] Times (abscisse) of the first trajectories.}
  \item{Py}{[vector(numeric)] Values of the first trajectories.}
  \item{Qx}{[vector(numeric)] Times of the second trajectories.}
  \item{Qy}{[vector(numeric)] Values of the second trajectories.}
  \item{timeScale}{[\code{numeric}]: allow to modify the time scale,
    increasing or decreasing the cost of the horizontal shift. If timeScale is
    very big, then the Frechet's distance is equal to the
    euclidienne distance. If timeScale is very small, then it is equal to
    the Dynamic Time Warping.}
  \item{FrechetSumOrMax}{[\code{character}]: The Frechet's distance can
    be define using the 'sum' function or the 'max' function. This option
    let the user to chose one or the other.}
}
\details{
  Given two curve P and Q, Frechet distance between P and Q is define as
  \code{inf_{a,b} max_{t} d(P(a(t)),Q(b(t)))}. It's computation is a
  NP-complex problem. When P and Q are trajectories (discrete curve), the
  problem is polynomial.

  The Frechet distance can also be define using a sum instead of a max:
  \code{inf_{a,b} sum_{t} d(P(a(t)),Q(b(t)))}

  The function \code{distFrechet} is C compiled,
  the function \code{distFrechetR} is in R,
  the function \code{distFrechetRec} is in recursive (the slowest) in R.

}

\value{
  A numeric value.
}


\section{Author}{
Christophe Genolini\cr
1. UMR U1027, INSERM, Université Paul Sabatier / Toulouse III / France\cr
2. CeRSM, EA 2931, UFR STAPS, Université de Paris Ouest-Nanterre-La Défense / Nanterre / France
}

\references{
  [1] Thomas Eiter & Heikki Mannila: \cr
  "Computing Discrete Fr´echet Distance"\cr

  [2] C. Genolini and B. Falissard\cr
  "KmL: k-means for longitudinal data"\cr
  Computational Statistics, vol 25(2), pp 317-328, 2010\cr

  [3] C. Genolini and B. Falissard\cr
  "KmL: A package to cluster longitudinal data"\cr
  Computer Methods and Programs in Biomedicine, 104, pp e112-121, 2011\cr
}

\seealso{distTraj}

\examples{
   Px <- 1:20
   Py <- dnorm(1:20,12,2)
   Qx <- 1:20
   Qy <- dnorm(1:20,8,2)

   distFrechet(Px,Py,Qx,Qy)

   ### Frechet using sum instead of max.
   distFrechet(Px,Py,Qx,Qy,FrechetSumOrMax="sum")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\name{condrmaxstab}
\alias{condrmaxstab}
\title{
  Conditional simulation of max-stable processes
}
\description{
  This function performs conditional simulation of various max-stable processes.
}
\usage{
condrmaxstab(k = 1, coord, cond.coord, cond.data, cov.mod = "powexp",
\dots, do.sim = TRUE, thin = n.cond, burnin = 50, parts)
}
\arguments{
  \item{k}{An integer. The number of conditional simulations to be generated.}
  \item{coord}{A vector or matrix that gives the coordinates of each
    location. Each row corresponds to one location - if any.}
  \item{cond.coord}{A vector or matrix that gives the coordinates of
    each conditional location. Each row corresponds to one location - if
    any.}
  \item{cond.data}{A vector that gives the conditional values at the
    corresponding conditioning locations. Each row corresponds to one
    location - if any.}
    \item{cov.mod}{A character string that gives the max-stable
    model. This must be one of "brown" for the Brown-Resnick model, or
    "whitmat", "cauchy", "powexp" and "bessel" for the Schlather model
    with the given correlation family.}
  \item{\dots}{The parameters of the max-stable model. See
    \code{\link{rmaxstab}} for more details.}
  \item{do.sim}{A logical value. If \code{TRUE} (the default), the
    conditional simulations are performed; otherwise only the simulated
    random partitions, i.e., the hitting scenarios, are returned.}
  \item{thin}{A positive integer giving by which amount the generated Markov
    chain should be thinned. This is only useful when the number of
    conditioning locations is greater than 7.}
  \item{burnin}{A positive integer giving the duration of the burnin
    period of the Markov chain.}
  \item{parts}{A matrix giving the hitting scenarios. Each row
    corresponds to one hitting scenarios. If missing then a Gibbs
    sampler will  be used to generate such hitting scenarios.}
}
\details{
  The algorithm consists in three steps:
  \enumerate{
    \item Draw a random partition \eqn{\theta}{\theta} from
    \deqn{\Pr\{\theta = \tau \mid Z(x) = z\}}{Pr{\theta = \tau | Z(x) = z}}
    \item Given the random partition, draw the extremal functions from
    \deqn{\Pr\{\varphi^+ \in \cdot \mid Z(x) = z, \theta =
      \tau\}}{Pr{\phi^+ in . | Z(x) = z, \theta = \tau}}
    \item Independently, draw the sub-extremal functions, i.e.,
    \deqn{\max_{i \ge 1} \varphi_i 1_{\{\varphi_i(x) < z\}}}{max_{i
	\geq 1} \phi_i 1_{\phi_i(x) < z}.}
  }

  The distribution in Step 1 is usually intractable and in such cases a
  random scan Gibbs sampler will be used to sample from this
  distribution.
}
\value{
  This function returns a list whose components are
  \item{sim}{The conditional simulations. Beware the first values
    corresponds to the conditioning values.}
  \item{sub.ext.fct}{The values of the sub-extremal functions.}
  \item{ext.fct}{The values of the extremal functions.}
  \item{timings}{The timings in seconds for each step of the algorithm.}
}
\references{
  Dombry, C. and Eyi-Minko, F. (2012) Regular conditional distributions
  of max infinitely divisible processes. \emph{Submitted}.
  
  Dombry, C., Eyi-Minko, F. and Ribatet, M. (2012) Conditional
  simulation of max-stable processes. To appear in \emph{Biometrika}.
}
\section{Warning}{
  This function can be extremely time consuming when the number of
  conditioning locations is large.
}
\author{Mathieu Ribatet}
\seealso{
\code{\link{rmaxstab}}, \code{\link{condrgp}}
}
\examples{
n.sim <- 50
n.cond <- 5

range <- 10
smooth <- 1.5

n.site <- 200
coord <- seq(-5, 5, length = n.site)
cond.coord <- seq(-4, 4, length = n.cond)
all.coord <- c(cond.coord, coord)

all.cond.data <- rmaxstab(1, all.coord, "powexp", nugget = 0, range = range,
                      smooth = smooth)
cond.data <- all.cond.data[1:n.cond]

ans <- condrmaxstab(n.sim, coord, cond.coord, cond.data, range = range,
                    smooth = smooth, cov.mod = "powexp")

idx <- order(all.coord)
matplot(coord, t(log(ans$sim)), type = "l", col = "grey", lty = 1,
        xlab = expression(x), ylab = expression(Z(x)))
lines(all.coord[idx], log(all.cond.data)[idx])
points(cond.coord, log(cond.data), pch = 15, col = 2)
}
\keyword{distribution}
\keyword{spatial}

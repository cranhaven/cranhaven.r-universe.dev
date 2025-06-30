\name{calc_lscp}
\alias{calc_lscp}
\alias{calc_lscp.kriged_surfaces}
\alias{calc_lscp.all_lscps}
\alias{wtd_lscp}

\title{
Calculate a single weighted adaptive landscape
}
\description{
\code{calc_lscp()} calculates a single weighted landscape from a set of kriged surfaces of functional characteristics and a set of weights for those characteristics. This landscape can then be plotted using \code{\link{plot.wtd_lscp}}. Additionally computes the fitness values for a sample of additional coordinates.
}
\usage{
calc_lscp(data, weights, ...)

\method{calc_lscp}{kriged_surfaces}(data, weights, ...)

\method{calc_lscp}{all_lscps}(data, weights, i, ...)
}
\arguments{
  \item{data}{
a \code{kriged_surfaces} or \code{all_lscps} object; the output of a call to \code{\link{krige_surf}} or \code{\link{calc_all_lscps}}, repsectively. If no \code{new_data} component is included in \code{data}, only the adaptive landscape will be produced.
}
  \item{weights}{
a vector of weights, one for each functional characteristic. These weights should be nonnegative and sum to 1.
}
  \item{i}{
when \code{data} is an \code{all_lscps} object, the index of the set of weights in the \code{grid_weights} object supplied to \code{calc_all_lscps()} to use to create the weighted landscape.
}
  \item{\dots}{
ignored.
}
}
\details{
\code{calc_lscp()} operates on the kriged surfaces stored in \code{data} by multiplying the functional characteristic values of each point on the surface grid by the weights and computing the sum of those values to arrive at a "fitness" value that is represented by the maximum height of the combined adaptive landscape. When a \code{new_data} component is present in \code{data} (e.g., because a \code{new_data} argument was supplied to \code{krige_surf()} or \code{data} is the output of a call to \code{krige_new_data()}), the weighted fitness values will be computed for the coordinates in \code{new_data} as well.
}
\value{
A \code{wtd_lscp} object, which contains the following components:
\item{W}{a named vector of the supplied weights}
\item{Wprime}{a list containing the weighted \code{grid} and \code{new_data} components of \code{data}, where the values of the functional characteristics for each location on the surface are weighted by the supplied weights and an additiona column, \code{Z}, has been added containing the height of the adaptive landscape at that point.}
}

\seealso{
\code{\link{plot.wtd_lscp}} for plotting the resulting weighted landscape.

\code{\link{generate_weights}} for generating a matrix of weights.

\code{\link{calc_all_lscps}} for computing weighted landscapes for a matrix of weights (i.e., rather than the single set of weights that can be used with \code{calc_lscp}). For finding an optimal set of weights, \code{calc_all_lscps} should be used, though it only produces the weighted fitness values for each set of weightd and not the weighted functional characteristic surfaces.
}
\examples{
data("warps")

warps_fnc <- as_fnc_df(warps, func.names = c("hydro", "fea"))

kr_surf <- krige_surf(warps_fnc)

weights <- c(hydro = .5, fea = .5)

w_lscp <- calc_lscp(kr_surf, weights = weights)

plot(w_lscp)

# Adding new_data
data("turtles")
kr_surf <- krige_new_data(kr_surf, new_data = turtles)

w_lscp <- calc_lscp(kr_surf, weights = weights)
w_lscp
plot(w_lscp)

## See further use with calc_all_lscps() 
## at help("calc_all_lscps")
}

\name{calc_all_lscps}
\alias{calc_all_lscps}

\title{
Calculate adaptive landscapes for a matrix of weights
}
\description{
\code{calc_all_lscps()} calculates adaptive landscapes from a set of kriged surfaces of functional characteristics and sets of weights for those characteristics.
}
\usage{
calc_all_lscps(kr_data, grid_weights, file = NULL)
}
\arguments{
  \item{kr_data}{
a \code{kriged_surfaces} object; the output of a call to \code{\link{krige_surf}}.
}
  \item{grid_weights}{
a \code{grid_weights} object; the output of a call to \code{\link{generate_weights}}.
}
  \item{file}{
the path of a file to save the resulting output object, which may be quite large. The file path should contain an .rds or .rdata extension, which will be saved using \code{\link{saveRDS}} or \code{\link{save}}, respectively. See Details on how to load these files after saving them.
}
}
\details{
\code{calc_all_lscps()} computes a combined adaptive landscape for each of the supplied sets of weights. The optimal landscape overall or for certain subsets of the sample data can be found using \code{\link{calcGrpWprime}} or \code{\link{calcWprimeBy}}. \code{\link{calc_lscp}} can be used to extract the surface of the weighted functional characteristics for each set of weights (see Examples).

Because the resulting objects are so large, it can be a good idea to save them after creation, which can be done automatically using the \code{file} argument. If the supplied file extension is \code{.rds}, \code{\link{saveRDS}} will be used to save the object to the supplied file path, and the file can be loaded using \code{\link{readRDS}}. If the supplied file extension is \code{.RData}, \code{\link{save}} will be used to save the object to the supplied file path, and the file can be loaded using \code{\link{load}}.
}
\value{
An \code{all_lscps} object containing the following components:
\item{dataframe}{a list of the \code{grid} and \code{new_data} data frames stored in \code{kr_data}.}
\item{wtd_lscps}{a list containing the weightred fitness values for each set of weights for the \code{grid} and \code{new_data} datasets. These are stored in matrices with a row for each data point in \code{grid} and \code{new_data} and a column for each set of weights.}
\item{grid_weights}{the \code{grid_weights} object supplied to \code{grid_weights}.}
}

\seealso{
\code{\link{calc_lscp}} for computing a single weighted landscape or extracting the weighted surface of functional characteristics for a single set of weights.

\code{\link{generate_weights}} for generating the required matrix of weights.

\code{\link{calcGrpWprime}} and \code{\link{calcWprimeBy}} for finding optimal sets of weights and adaptive landscapes for subgroups.
}
\examples{
data("warps")
data("turtles")

warps_fnc <- as_fnc_df(warps, 
                       func.names = c("hydro", "fea"))

kr_surf <- krige_surf(warps_fnc, new_data = turtles)

grid_weights <- generate_weights(n = 20, data = kr_surf)

all_lscps <- calc_all_lscps(kr_surf,
                            grid_weights = grid_weights)
all_lscps

# Extract the weighted surface for a single set
# of weights (here, the 6th set of weights)

grid_weights[6,]

wtd_lscp_6 <- calc_lscp(all_lscps, i = 6)
wtd_lscp_6

# This aligns with the weighted fitness value:
mean(all_lscps$wtd_lscps$new_data[,6])
}

\name{calcGrpWprime}
\alias{calcGrpWprime}
\alias{print.grp_Wprime}

\title{
Compute optimally weighted adaptive landscapes
}
\description{
\code{calcGrpWprime()} computes the optimally weighted adaptive landscape by searching through the adaptive landscapes formed from sets of weights and performance surfaces, and finding the set of weights that yields the greatest overall (average) fitness value (Z) across a sample of data or a subset thereof.
} 
\usage{
calcGrpWprime(x, index, method = "chi-squared",
              quantile = 0.05)
\method{print}{grp_Wprime}(x, 
      digits = max(3L, getOption("digits") - 3L), ...)
}
\arguments{
  \item{x}{
for \code{calcGrpWprime()}, an \code{all_lscps} object; the output of a call to \code{\link{calc_all_lscps}}.

for \code{print()}, a \code{grp_Wprime} object; the output of a call to \code{calcGrpWprime()}
}
  \item{index}{
an optional vector of indices indicating which subset of the \code{new_data} dataset originally supplied to \code{\link{krige_surf}} should be calculated. Can be specified as a vector of numerical indicies, logical indices, or row names. If unspecified, the optimal weights will be computed using the full sample. Supplied to \code{\link{subset}}, so the name of the dataset containing the subsetting variable does not need to be included if the subsetting variable is in \code{new_data}. See Examples.
}
  \item{method}{
the method used to compute the optimal weights. Allowable options include \code{"chi-square"} (the default), \code{"quantile"}, or \code{"max"}. \code{"chi-square"} and \code{"quantile"} involve averaging across the best several sets of weights, whereas \code{"max"} uses the singular best set of weights. Abbreviations allowed. See Details.
}
  \item{quantile}{
when \code{method} is \code{"chi-square"} or \code{"quantile"}, the top quantile used to determine the best sets of weights to be included in the average to compute the optimal set of weights. Should be a number between 0 and 1, with a low value indicating that only the few top sets of weights will be used. Ignored when \code{method = "max"}.
}
  \item{digits}{
the number of significant digits to print.
}
  \item{\dots}{
passed to \code{\link{print.default}}.
}
}
\details{
\code{calcGrpWprime()} calculates an overall fitness score for each set of weights based on the average weighted fitness values of the indexed subgroup. The set of weights that optimizes this score is then produced as the weights defining the optimal adaptive landscape for that subgroup. The way the final set of weights is computed depends on the argument to \code{method}. When \code{method = "max"}, the single best set of weights is used. However, often many of the upper sets of weights perform equally or nearly equally as well as the best set. It is instead recommended to use \code{"quantile"} or \code{"chi-squared"} methods. When \code{method = "quantile"}, the top \eqn{X\%} of weights are averaged to compute the optimal weights, where \eqn{X} corresponds to the value supplied to \code{quantile}. When \code{method = "chi-square"},  the chi-squared value \eqn{\chi^2_i} is computed for each set of weights \eqn{i} as \deqn{\chi^2_i = -2 \log \frac{Z_{max}}{Z_i}}
where \eqn{Z_{max}} is the largest \eqn{Z} among the weights, and a p-value is computed for each \eqn{\chi^2_i} value using a \eqn{\chi^2} distribution with 2 d.f.; any set of weights with a p-value less than \code{quantile} is included to be averaged to compute the optimal set of weights.
}
\value{
A \code{grp_Wprime} object, which contains the following components:

\item{Zprime}{
a list containing the optimal weights and the Z value they yield (\code{wn}), and, if \code{method} is \code{"chi-square"} or \code{"quantile"}, summary statistics about the best sets of weights used to compute the optimal weights, including the standard error (\code{wn.se}), standard deviation (\code{wn.sd}), and range (\code{wn.range}).
}
\item{W}{
a matrix containing all sets of weights (i.e., those supplied to the \code{grid_weights} argument of \code{calc_all_lscps()}) along with the Z value each yields, ordered in descending order by the yielded Z value. When \code{index} is specified, the resulting Z values are computed only using the indexed subset.
}
\item{Wprime}{
a \code{\link{wtd_lscp}} object containing the optimal weights (\code{W}) and the landscape grid and sample functional characteristcs weighted by the optimal weights.
}
}

\references{

Dickson, B. V., Clack, J. A., Smithson, T. R., & Pierce, S. E. (2021). Functional adaptive landscapes predict terrestrial capacity at the origin of limbs. Nature, 589(7841), 242-245.

Jones, K. E., Dickson, B. V., Angielczyk, K. D., & Pierce, S. E. (2021). Adaptive landscapes challenge the "lateral-to-sagittal"" paradigm for mammalian vertebral evolution. Current Biology, 31(9), 1883-1892.
}

\seealso{
\code{\link{calc_all_lscps}} for computing the landscapes which are to be optimized.

\code{\link{calcWprimeBy}} for finding optimal sets of weights for multiple subgroups defined by a subgrouping variable.

\code{\link{plot.grp_Wprime}} for plotting the resulting adaptive landscape.
}
\examples{
data("warps")
data("turtles")

warps_fnc <- as_fnc_df(warps, 
                       func.names = c("hydro", "fea"))

kr_surf <- krige_surf(warps_fnc, new_data = turtles)

grid_weights <- generate_weights(n = 3, data = kr_surf)

all_lscps <- calc_all_lscps(kr_surf,
                            grid_weights = grid_weights)
                            
wprime_S <- calcGrpWprime(all_lscps,
                          index = Ecology == "S")
wprime_S
plot(wprime_S)
}

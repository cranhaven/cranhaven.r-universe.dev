\name{calcWprimeBy}
\alias{calcWprimeBy}
\alias{print.by_Wprime}
\alias{summary.by_Wprime}
\alias{print.summary.by_Wprime}

\title{
Compute optimally weighted adaptive landscapes by subgroup
}
\description{
\code{calcWprimeBy()} computes the optimally weighted adaptive landscape by searching through the adaptive landscapes formed from sets of weights and performance surfaces, and finding the set of weights that yields the greatest overall (average) fitness value (Z) across subsets of a sample dataset.
}
\usage{
calcWprimeBy(x, by, method = "chi-squared", quantile = 0.05)

\method{print}{by_Wprime}(x, 
      digits = max(3L, getOption("digits") - 3L), ...)

\method{summary}{by_Wprime}(object, ...)

\method{print}{summary.by_Wprime}(x, 
      digits = max(3L, getOption("digits") - 3L), ...)
}
\arguments{
  \item{x}{
for \code{calcWprimeBy()}, an \code{all_lscps} object; the output of a call to \code{\link{calc_all_lscps}}.

for \code{print.by_Wprime()}, a \code{by_Wprime} object; the output of a call to \code{calcWprimeBy()}.

for \code{print.summary.by_Wprime()}, a \code{by_Wprime} object; the output of a call to \code{summary.by_Wprime()}.
}
  \item{by}{
a one-sided formula containing the grouping variable on the right hand side (e.g., \code{~g}) or a vector containing the subgrouping variable. When supplied as a formula, the grouping variable must be present in the global environment or in the \code{new_data} component in the \code{kriged_surfaces} object originally supplied to \code{calc_all_lscps()}.
}
  \item{method}{
the method used to compute the optimal weights. Allowable options include \code{"chi-square"} (the default), \code{"quantile"}, or \code{"max"}. \code{"chi-square"} and \code{"quantile"} involve averaging across the best several sets of weights, whereas \code{"max"} uses the singular best set of weights. Abbreviations allowed. See \code{\link{calcGrpWprime}} for details.
}
  \item{quantile}{
when \code{method} is \code{"chi-square"} or \code{"quantile"}, the top quantile used to determine the best sets of weights to be included in the average to compute the optimal set of weights. Should be a number between 0 and 1, with a low value indicating that only the few top sets of weights will be used. Ignored when \code{method = "max"}. See \code{\link{calcGrpWprime}} for details.
}
  \item{digits}{
the number of significant digits to print.
}
  \item{\dots}{
passed to \code{\link{print.default}} and \code{\link{print.table}}.
}
  \item{object}{
a \code{by_Wprime} object; the output of a call to \code{calcWprimeBy()}.
}
}
\details{
\code{calcWprimeBy()} splits the sample data based on the \code{by} variable and then calls \code{\link{calcGrpWprime}} on each subset. The main benefit of using \code{calcWprimeBy()} is that the subgrouping variable is part of the output object and therefore can be used in plotting using \code{\link{plot.by_Wprime}}.
}
\value{
A \code{by_Wprime} object contaning the following components:
\item{by}{the subgrouping variable supplied to \code{by}, stored as a factor and with a \code{"by_name"} attribute containing the name of the variable.}
\item{grp_Wprimes}{a list of \code{grp_Wprime} objects, one for each level of the subgrouping variable.}
}

\seealso{
\code{\link{calc_all_lscps}} for computing the landscapes which are to be optimized.

\code{\link{calcGrpWprime}} for finding optimal sets of weights for a single subgroup.

\code{\link{plot.by_Wprime}} for plotting the resulting adaptive landscapes.
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
                            
wprime_Ecology <- calcWprimeBy(all_lscps, by = ~Ecology)
wprime_Ecology
summary(wprime_Ecology)
plot(wprime_Ecology)
}
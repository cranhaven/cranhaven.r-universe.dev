\name{lands.grp.test}
\alias{lands.grp.test}
\alias{print.lands.grp.test}
\alias{multi.lands.grp.test}
\alias{print.multi.lands.grp.test}

\title{
Significance tests between sets of weights
}
\description{
\code{lands.grp.test()} performs a statistical test for whether the optimal adaptive landscape for two subgroups are significantly different from each other. The p-value of the test is the proportion of weight sets that are shared between the two subgroups among their respective top weight sets. \code{multi.lands.grp.test()} performs this test for all pairs of subgroups.
}
\usage{
lands.grp.test(grpa, grpb, method = "chi-squared",
               quantile = 0.05)

multi.lands.grp.test(x, method = "chi-squared",
                     quantile = 0.05)

\method{print}{lands.grp.test}(x, digits = max(3L, getOption("digits") - 3L), ...)

\method{print}{multi.lands.grp.test}(x, digits = max(3L, getOption("digits") - 3L), 
      style = "matrix", ...)
}

\arguments{
  \item{grpa, grpb}{
for \code{lands.grp.test()}, the two \code{grp_Wprime} objects containing the adaptive landscapes to be compared; these are the output of calls to \code{\link{calcGrpWprime}}.
}
  \item{x}{
for \code{multi.lands.grp.test()}, a \code{by_Wprime} object, the output of a call to \code{\link{calcWprimeBy}}.

for \code{print()}, the output of a call to \code{lands.grp.test()} or \code{multi.lands.grp.test()}.
}
  \item{method}{
the method used to determine which sets of weights are in the "best" sets of weights that are to be comapred between the two groups. Allowable options include \code{"chi-squared"} and \code{"quantile"}. See \code{\link{calcGrpWprime}} for details.
}
  \item{quantile}{
the top quantile used to determine the best sets of weights to be included in the average to compute the optimal set of weights. Should be a number between 0 and 1, with a low value indicating that only the few top sets of weights will be used. See \code{\link{calcGrpWprime}} for details.
}
  \item{digits}{
the number of significant digits to print.
}
  \item{style}{
how to display the results of the pairwise tests; allowable options include \code{"matrix"} and \code{"table"}. Abbreviations allowed.
}
  \item{\dots}{
passed to \code{\link{print.default}}.
}
}
\details{
\code{lands.grp.test()} performs pairwise comparisons between two adaptive groups by comparing the number of shared landscapes \eqn{n_{A+B}} in the top percentile of each group with the total number of landscapes in this top percentile \eqn{n_{total}}. The probability \eqn{P(A=B)} thus is calculated as:

\deqn{
P(A=B) = n_{A+B}/n_{total}
}

If \code{method = "quantile"} is used, then the top percentile is defined by \code{quantile}. If \code{method = "chi-squared"} is used, then the top percentile is calculated from the chi-squared value \eqn{\chi^2_i} as: \deqn{\chi^2_i = -2 \log \frac{Z_{max}}{Z_i}} where \eqn{Z_{max}} is the largest \eqn{Z} among the weights, and a p-value is computed for each \eqn{\chi^2_i} value using a \eqn{\chi^2} distribution with 2 d.f.; any set of weights with a p-value less than \code{quantile} is included in the optimal set of weights.

\code{multi.lands.grp.test()} is a wrapper for \code{lands.grp.test()}, applying the function pairwise to all combinations of groups calculated by \code{\link{calcWprimeBy}}.

}
\value{
For \code{lands.grp.test()}, a \code{lands.grp.test} object containing the following components:
\item{n.match}{the number of sets of weights that match between the two supplied subgroups}
\item{p.val}{the p-value of the test, computed as the number of sets of weights that match divided by the number of sets of weights compared}
\item{matching}{a matrix containing the sets of weights that match between the two subgroups}
\item{method}{the argument supplied to \code{method}}
\item{quantile}{the argument supplied to \code{quantile}}

For \code{multi.lands.grp.test()}, a \code{multi.lands.grp.test} object containing the following components:
\item{res}{a data frame containing the results of the tests, with the columns \code{Group A} and \code{Group B} indicating the groups involved in the comparison, the column \code{Matches} containing the number of matching sets of weights in the comparison, and the column \code{p value} containing the p-value of the test.}
\item{method}{the argument supplied to \code{method}}
\item{quantile}{the argument supplied to \code{quantile}}

For \code{print.multi.lands.grp.test()}, setting \code{style = "table"} prints the \code{res} component as-is; setting \code{style = "matrix"} creates a matrix where the p-values of the test are below the diagonal and the number of matches of the test are above the diagonal.
}
\references{

Jones, K. E., Dickson, B. V., Angielczyk, K. D., & Pierce, S. E. (2021). Adaptive landscapes challenge the "lateral-to-sagittal"" paradigm for mammalian vertebral evolution. Current Biology, 31(9), 1883-1892.

}
\seealso{
\code{\link{calcGrpWprime}} and \code{\link{calcWprimeBy}} for creatign the objects used as inputs to these functions
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

# Comparing adaptive landscapes of Ecology groups S and M
wprime_S <- calcGrpWprime(all_lscps,
                          index = Ecology == "S")
wprime_M <- calcGrpWprime(all_lscps,
                          index = Ecology == "M")
lands.grp.test(wprime_S, wprime_M)

# Comparing adaptive landscapes of all Group subgroups
wprime_by_Group <- calcWprimeBy(all_lscps, by = ~Group)
tests <- multi.lands.grp.test(wprime_by_Group)
tests
print(tests, style = "table")
}

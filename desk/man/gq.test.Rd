\name{gq.test}
\alias{gq.test}
\title{Goldfeld-Quandt Test}
\usage{
gq.test(
  mod,
  data = list(),
  split = 0.5,
  omit.obs = 0,
  ah = c("increasing", "unequal", "decreasing"),
  order.by = NULL,
  sig.level = 0.05,
  details = FALSE,
  hyp = TRUE
)
}
\arguments{
\item{mod}{estimated linear model object or formula. If only a model formula is passed then the \code{data} argument must be specified.}

\item{data}{if \code{mod} is a formula then the corresponding data frame has to be specified.}

\item{split}{partitions the data set into two groups. If <= 1 then \code{split} is a percentage value such that \code{T*split} observations are in the first partition. If \code{split} >= 1 it is interpreted as the index of the partitioning observation, i.e. the number of observations in the first group.}

\item{omit.obs}{the number of central observations to be omitted. Might increase the power of the test. If <= 1 then \code{split} is the percentage value of all observations, otherwise it is interpreted as absolute number.}

\item{ah}{character string specifying the type of the alternative hypothesis: \code{"increasing"} (variance increases from group 1 to group 2), \code{"decreasing"} (variance decreases from group 1 to group 2), \code{"unequal"} (variances are unequal between the groups). The default is to test for increasing variances.}

\item{order.by}{either a vector \code{z} or a formula with a single explanatory variable like \code{~ z}. The observations in the model are ordered by the size of \code{z}. If set to \code{NULL} (the default) the observations are assumed to be ordered.}

\item{sig.level}{significance level. Default value: \code{sig.level = 0.05}.}

\item{details}{logical value indicating whether specific details about the test should be returned.}

\item{hyp}{logical value indicating whether the Hypotheses should be returned.}
}
\value{
A list object including:
\tabular{ll}{
\code{hyp} \tab character matrix of hypotheses (if \code{hyp = TRUE}).\cr
\code{results} \tab a data frame of basic test results.\cr
\code{hreg1} \tab matrix of regression results in Group I.\cr
\code{stats1} \tab additional statistic of regression in Group I.\cr
\code{hreg2}\tab matrix of regression results in Group II.\cr
\code{stats2}\tab additional statistic of regression in Group II.\cr
\code{nulldist} \tab type of the Null distribution with its parameters.\cr
}
}
\description{
Goldfeld-Quandt test for heteroskedastic errors. The object of test results returned by this command can be plotted using the \code{plot()} function.
}
\examples{
## 5 observations in group 1 with the hypothesis that the variance of group 2 is larger
gq.test(rent ~ dist, split = 5, ah = "increasing", data = data.rent)

## Ordered by population size
eu.mod <- ols(expend ~ pop + gdp + farm + votes + mship, data = data.eu)
results <- gq.test(eu.mod, split = 13, order.by = data.eu$pop, details = TRUE)
results

plot(results)

}
\references{
Goldfeld, S.M. & Quandt, R.E. (1965): Some Tests for Homoskedasticity. Journal of the American Statistical Association 60, 539-547.
}
\seealso{
\code{\link{wh.test}}, \code{\link[lmtest]{gqtest}}.
}
\concept{heteroskedastic errors}
\concept{heteroskedasticity}

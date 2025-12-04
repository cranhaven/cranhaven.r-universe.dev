\name{summary.mcmcOutput}
\alias{summary.mcmcOutput}
\alias{sumryList}
\alias{print.mcmcOutput}
\title{
Print and summary methods for objects of class \code{mcmcOutput}
}
\description{
\code{summary} generates a data frame with summary and diagnostic statistics for each of the MCMC chains in the \code{mcmcOutput} object, and (if \code{verbose = TRUE}) prints a brief overview to the Console. The data frame will be displayed in the Console unless assigned to an object or passed to \code{View}.

\code{sumryList} generates summary and diagnostic statistics for each of the MCMC chains in the \code{mcmcOutput} object and returns them as a list-of-lists, see Value.

\emph{\code{summary} and \code{sumryList} now have a \code{CRItype="none"} option and arguments to include \code{overlap0} and \code{f} in the summary table.}

\code{print} displays characteristics of the \code{mcmcOutput} object. It does \emph{not} display summaries of the values: for that use \code{summary}.
}
\usage{
\method{summary}{mcmcOutput}(object, digits=3, median=TRUE, mode=FALSE,
    CRItype=c("hdi", "symmetrical", "none"),
    CRImass=0.95, Rhat=TRUE, MCEpc = TRUE, n.eff=FALSE,
    overlap0=FALSE, f=FALSE, verbose=TRUE, ...)

\method{print}{mcmcOutput}(x, ...)

sumryList(object, median=TRUE, mode=FALSE,
    CRItype=c("hdi", "symmetrical", "none"),
    CRImass=0.95, Rhat=TRUE, MCEpc = TRUE, n.eff=FALSE,
    overlap0=FALSE, f=FALSE, ...)
}
\arguments{
  \item{x, object}{
an object of class \code{mcmcOutput}.
}
  \item{digits}{
the number of digits for rounding of values in the output.
}
  \item{median}{
if TRUE, the median will be included as a column in the data frame produced.
}
  \item{mode}{
if TRUE, the mode will be included as a column in the data frame produced.
}
  \item{CRItype}{
if \code{hdi}, the credible interval will be a highest density interval; if \code{symmetrical}, a symmetrical CRI will be generated with \code{quantile}; ignored if \code{CRImass=NA}.
}
  \item{CRImass}{
the probability mass to include in the credible interval; if \code{CRImass=NA}, no CRI will be included in the output.
}
  \item{Rhat}{
if TRUE, estimates of Rhat will be included; ignored if only 1 chain or < 100 values per chain. See \code{\link{getRhat}}.
}
  \item{MCEpc}{
if TRUE, estimates of the Monte Carlo standard error as a percentage of the posterior SD will be included; ignored if < 100 values per chain. See \code{\link{getMCE}}.
}
  \item{n.eff}{
if TRUE, estimates of the effective number of draws allowing for autocorrelation will be included; ignored if < 100 values per chain. See \code{\link{getNeff}}.
}
  \item{overlap0}{
if TRUE, a column with TRUE/FALSE will be included, indicating whether the credible interval includes zero. Ignored if no CRI is calculated.
}
  \item{f}{
if TRUE, a column with the proportion of the posterior with the same sign as the mean.
}
  \item{verbose}{
if FALSE, suppresses output to the Console, other than the table of statistics.
}
  \item{\dots}{
further arguments for the print or summary function.
}
}
%%% Details and References moved to the getXXX help page.
\value{
\code{print} returns \code{x} invisibly.

\code{summary} returns a data frame with columns for summary and diagnostic statistics and a row for each node; mean and SD are always included, other columns if selected. It has attributes for \code{nChains} and \code{simsList} derived from the input object. The output will appear in the Console unless assigned to an object or passed to \code{View}.

\code{sumryList} returns a list with a component for each statistic; each component is itself a list with the values for each parameter. See examples.
}
\author{
Mike Meredith.
}

\examples{
data(mcmcListExample)
mco <- mcmcOutput(mcmcListExample)
mco  # equivalent to ...
print(mco)
summary(mco)
# just the summary data frame
summary(mco, verbose=FALSE)
# assign the output to an object
tmp <- summary(mco, median=FALSE, CRItype="sym", CRImass=0.8,
    n.eff=TRUE, MCEpc=FALSE)
tmp

mcos <- sumryList(mco)
str(mcos)
mcos$mean$p
mcos$MCEpc$z
}
\keyword{print}
\keyword{methods}

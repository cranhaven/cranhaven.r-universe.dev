\name{plot.eff.biv}
\alias{plot.eff.biv}
\title{Plot method for eff.biv objects}
\description{Plot One or Two Bivariate Confidence Ellipses or a TRACE display for
 efficiently shrunken regression coefficients.  The default is to display two ellipses
 at confidence levels 0.95 and 0.50 on a single plot for the first 2 of p >= 2
 coefficients. The projection of the eff.ridge() Path from the OLS solution (at m = 0)
 to the Knot at the most likely Minimum MSE Risk solution (at m = mStar) ...plus the
 continuation of this Path towards the Shrinkage Terminus at (0,0) is shown in "red".
 To show the (outer) Ellipse as large as possible, the (0,0) point may be "off the plot".
}
\usage{
  \method{plot}{eff.biv}(x, type = "ellip", ...)
}
\arguments{
  \item{x}{Output list object of class eff.ridge.}
  \item{type}{One of 2 options: "ellip" or "trace". The default option of "ellip"
   displays the Confidence Ellipse(s) specified by arguments to eff.biv().}
  \item{\dots}{Optional argument(s) passed on to plot().}
}
\value{NULL}
\author{Bob Obenchain <wizbob@att.net>}
\examples{
  # Cost-Effectiveness inferences using Linear Models and eff.ridge...
  \dontrun{
    library(ICEinfer)
    data(sepsis)
    ndr <- ICEpref(sepsis$icu, sepsis$qalypres, sepsis$totcost, lambda=50000, beta=0.1)
    sndr <- data.frame(cbind(ndr$pref, sepsis)) # ndr: non-linear diminishing returns...
    form4 <- ndr.pref ~ icu + age + orgfails + apache
    usndra <- eff.aug(eff.ridge(form4, sndr)) # compare ndr of 2 Intensive Care Units...
    plot(efobj <- eff.biv(usndra, 2, 4))
    efobj          # implicit print...
    # plot(efobj, type = "tr")
  }
}
\keyword{trace}

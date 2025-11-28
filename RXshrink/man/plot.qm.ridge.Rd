\name{plot.qm.ridge}
\alias{plot.qm.ridge}
\title{Plot method for qm.ridge objects}
\description{Plot TRACE displays for 2-parameter (q, m) generalized ridge estimators.  The
 default is to display all five Traces on one page with no legend. When the number, p, of
 non-constant x-variables is at most 30, the lines [straight or curved] displayed on Traces
 avoid (or postpone) use of lty = 3 [dotted].
}
\usage{
  \method{plot}{qm.ridge}(x, trace = "all", LP = 0, HH = 0, \dots )
}
\arguments{
  \item{x}{Output list object of class qm.ridge.}
  \item{trace}{One of seven possible options: "all" to display 5 traces in one graph,
   "seq" to display 5 full-sized traces in sequence in response to user prompts,
   "coef" to display only the estimated beta coefficient trace,
   "rmse" to display only the estimated relative MSE risk trace,
   "exev" to display only the estimated excess MSE eigenvalue (OLS minus larlso),
   "infd" to display only the estimated inferior direction cosine trace, or
   "spat" to display only the delta-factor pattern trace.}
  \item{LP}{The "Legend Position" must be an integer between 0 and 9, inclusive:
   LP = 0 is the default and yields NO legend display,
   LP = 1 displays a crude legend at the Lower-Right of each trace plot, ... ,
   LP = 7 displays the legend in the Upper-Right position, . ,
   LP = 9 displays the legend in the Middle-Middle position.}
  \item{HH}{The "Half Height" plot option is an integer between 0 and 2, inclusive:
   HH = 0 is the default and yields trace plots with either 1 or 3 rows,
   HH = 1 calls par(mfrow=c(2,1)) before displaying the current trace plot,
   HH = 2 displays a 2nd (or sebsequent) trace in "Half Height" mode. NOTE that
   these "Half Height" options allow users to display both a qm.ridge() and an
   eff.ridge() trace on a single plot.}  
  \item{\dots}{Optional argument(s) passed on to plot().}
}
\value{NULL}
\author{Bob Obenchain <wizbob@att.net>}
\examples{
  data(longley2)
  form <- GNP~GNP.deflator+Unemployed+Armed.Forces+Population+Year+Employed
  rxrobj <- qm.ridge(form, data=longley2)
  plot(rxrobj, "rmse", LP = 5)
}
\keyword{trace}

\name{plot.YonX}
\alias{plot.YonX}
\title{Plot method for YonX objects}
\description{Graphics for Shrinkage in "Simple" Linear Regression: Models with only p=1
 X-variable. The default is to [1] display four TRACES in one plot and then to [2]
 display the "Y on X" scatter-plot with Three Fitted Lines: the OLS fit is BLUE, the
 optimally Shrunken fit is "purple", and the largest m-Extent of shrinkage with Relative
 MSE at most that of OLS is "red". Note: Whenever model lack-of-fit is small, these 3
 m-Extents can be quite close to each other.
}
\usage{
  \method{plot}{YonX}(x, trace = "all", \dots )
}
\arguments{
  \item{x}{Output list object of class "YonX".}
  \item{trace}{One of EIGHT possible options: "all" to display 4 traces on the first plot,
    then the "YonX" scatter plot; "seq" to display 5 full-sized plots in sequence (in
    response to user prompts); "coef" to display only the estimated beta coefficient trace
    (a straight line); "rmse" to display only the (quadratic) estimated relative MSE risk
    trace; "spat" to display only the delta-factor trace (a straight line); "lglk" to
    display only the "-2 log(Likelihood Ratio)" trace; "YonX" to display only the Y-vs-X
    scatter plot with 3 fitted-lines: the OLS fitted line (BLUE), the "purple" Maximum-Likelihood
    (optimally biased) line, and the "red" line marking the most shrinkage with estimated
    MSE Relative Risk less than or equal to that of OLS; or "exev" to display only the Excess
	Eigenvalue trace that is redundant with the "rmse" trace.}
  \item{\dots}{Optional argument(s) passed on to plot().}
}
\details{The effects of Shrinkage on Simple Linear Regression models (p = 1) are, in reality,
 NOT easier to illustrate than the corresponding effects on Multiple Linear Regressiom models
 (p > 1). In both situations, alternative estimates of effects and risks abound. For example,
 the estimate chosen can be [1] Maximum Likelihood under Normal-theory, [2] Unbiased under
 Normal-theory or [3] have "Correct Range". [See Obenchain (1978), equations (3.3) to (3.5),
 and corresponding text.] When a graphic contains only a single curve, a "reality" is that
 the general "shape" of the curve (plus any highlighted "points" on that curve) should "look
 right" or, at the very least, "reasonable".
 
 My choices among alternative estimates of (nonlinear) MSE risk were initially made roughly
 30 years ago ...and have remained mostly unchanged (primarily) for consistency with earlier
 versions of RXshrink.

 The NEW "rmse" TRACE for class "YonX" displays MSE Relative Risk estimates from the "qrsk"
 vector rather than the (traditional) "rmse" estimates for all p = 1 models. This allows MSE
 Relative Risk estimates to \emph{satisfy a Quadratic equation} and give the CORRECT visual
 impression that Relative Risk is MINIMIZED at the ML "purple" point and dotted-line at m =
 (1-dMSE). Note that the Relative Risk then starts to increase for m > (1-dMSE) and returns
 to its initial starting level at m = 0 ["blue" point and dotted-line] when the m-Extent
 reaches m = 2*(1-dMSE) ["red" point and dotted-line.] Finally, when 0.5 < dMSE < 1, the
 Relative Risk then continues to increase, reaching its Maximum at m = 1. As argued in
 Obenchain(1978), the "Good" Shrinkage Range is 0 < m < 2*(1-dMSE), between the "blue" and
 "red" vertical dotted-lines on the "rmse" TRACE.
}
\value{NULL}
\references{
  Obenchain RL. (1978) Good and Optimal Ridge Estimators. \emph{Annals of Statistics}
  \bold{6}, 1111-1121. \doi{10.1214/aos/1176344314}
  
  Obenchain RL. (2022) Efficient Generalized Ridge Regression. \emph{Open Statistics}
  \bold{3}: 1-18. \doi{10.1515/stat-2022-0108}
  
  Obenchain RL. (2022) \emph{RXshrink_in_R.PDF} RXshrink package vignette-like document,
  Version 2.1. \url{http://localcontrolstatistics.org}
}
\author{Bob Obenchain <wizbob@att.net>}
\examples{
  data(haldport)
  form <- heat ~ p4caf
  YXobj <- YonX(form, data=haldport)
  plot(YXobj)
}
\keyword{trace}

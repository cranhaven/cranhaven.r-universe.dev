\name{ICEcolor}
\alias{ICEcolor}
\title{Compute Preference Colors for Outcomes within a Bootstrap Confidence Wedge}
\description{
 Assuming the input ICEw object is of class ICEwedge, ICEcolor uses lambda = lfact * ICEw$lambda
 to define an ICE Preference Map with specified parameters (lfact, beta, gamma) to compute
 Economic Preference values. Only the Bootstrap re-sampled points within the ICE confidence wedge
 are used. Thus, assuming that the overall level of confidence (statistical size of the wedge) is
 held fixed, the points to be colored are always the very same points for all choices of lambda.
 However, the numerical value of preference (and thus the color) of each such point as well as
 the overall symmetry or asymmetry in the resulting ICE map can depend greatly upon lambda. 
}
\usage{
 ICEcolor(ICEw, lfact = 1, beta = 1, gamma = 3+2*sqrt(2)) 
}
\arguments{
  \item{ICEw}{Existing ICEwedge object.}
  \item{lfact}{Strictly positive multiplier for ICEw$lambda.}
  \item{beta}{Strictly positive Returns-to-Scale power parameter for the ICE
   Preference Map.  beta = 1 implies linear (constant) Returns to Scale.  beta > 0 and < 1
   implies diminishing Returns to Scale.  beta > 1 implies increasing Returns to Scale.}
  \item{gamma}{Strictly positive Directional power parameter.  The smallest reasonable value
   for gamma is usually gamma = beta, which yields a (generalized) linear map.  The largest
   reasonable value for gamma is usually gamma = beta*(3+2*sgrt(2)), which yields a map that
   satisfies the Cartesian Monotonicity Axiom and also admits all possible finite values for
   WTP and WTA, i.e. all values greater than or equal to 0 but less than +Inf.}
}
\details{Multiple calls to ICEcolor() are usually made for different lfact multipliers of
  LCEw$lambda item as well as different choices for the ICE Preference power parameters, beta
  and gamma.  Calls to plot(x, alibi) for these alternative ICEcolor x-objects can be used
  to illustrate that exogenous Economic Uncertainty can literally SWAMP the Statistical
  Uncertainty endogenous to patient level data on the relative cost and effectiveness of two
  treatments.  
}
\value{
  Object of class ICEcolor containing an output list with the following items:
  \item{df}{Saved value of the name of the data.frame input to ICEcolor.}
  \item{lambda}{Saved positive value of lambda input to ICEcolor.}
  \item{unit}{Saved value of unit, cost or effe, input to ICEcolor.}
  \item{R}{Saved integer value for number of bootstrap replications input to ICEcolor.}
  \item{trtm}{Saved name of the treatment indicator within the df data.frame.}
  \item{xeffe}{Saved name of the treatment effectiveness variable within the df data.frame.}
  \item{ycost}{Saved name of the treatment cost variable within the df data.frame.}
  \item{effcst}{Saved value of the sorted 3-variable (trtm,effe,cost) data.frame.}
  \item{t1}{Observed value of (DeltaEffe, DeltaCost) when each patient is sampled exactly once.}
  \item{t}{R x 2 matrix of values of (DeltaEffe, DeltaCost) computed from bootstrap resamples.}
  \item{seed}{Saved value of the seed used to start pseudo random number generation.}
}
\references{
 Cook JR, Heyse JF.  Use of an angular transformation for ratio estimation in cost-effectiveness
 analysis.  \emph{Statistics in Medicine}  2000; \bold{19}: 2989-3003.
 
 Obenchain RL.  ICE Preference Maps: Nonlinear Generalizations of Net Benefit and Acceptability.
 \emph{Health Serv Outcomes Res Method}  2008; \bold{8}: 31-56. DOI 10.1007/s10742-007-0027-2.
 Open Access.
}
\author{Bob Obenchain <wizbob@att.net>}
\seealso{\code{\link{ICEwedge}}, \code{\link{plot.ICEcolor}} and \code{\link{print.ICEcolor}}}
\examples{
  # Read in previously computed ICEwedge output list.
  data(dpwdg)
  dpcol <- ICEcolor(dpwdg)
  # Display preference coloring for the stored value of lambda.
  plot(dpcol, show="RBOW")
  dpcolX <- ICEcolor(dpwdg, lfact=10)
  # Display preference coloring when lambda is increased by a factor of 10.
  plot(dpcolX, show="RBOW")
}
\keyword{methods}
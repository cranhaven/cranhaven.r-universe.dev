\name{ICEepmap}
\alias{ICEepmap}
\alias{ICEomega}
\title{Set Parameter Values defining ICE Economic Preference Maps}
\description{
  ICEepmap() and ICEomega() set numerical values for lambda (the full, fair shadow price of health)
  and for the two so-called power-parameters of a parametric ICE Preference Map.  These functions
  return a value, epm, that is an output list object of class ICEepmap for display using print(epm)
  or plot(epm, xygrid).  The primary purpose of such plots is to allow the user to more easily
  visualize the profound effects that changing numerical values for lambda, beta and either gamma
  or eta = gamma / beta can have on the iso-preference contours (level curves) of an ICE map.
  
  From the statistical prospective championed here, lambda is little more than a nusiance parameter.
  For example, the wedge-shaped ICE confidence regions formed by ICEwedge() are equivariant under
  changes in lambda.  Unfortunately, the resulting economic preferences that can be visualized using
  ICEcolor() can change drastically with changes in lambda.
  
  A stardardized ICE map results when the specified value of lambda is used to assure that the x effe
  difference and the y cost difference are both expressed in the same units (i.e. both in cost units
  or else both in effe units.)  Unfortunately, the only way to assure display of this particular sort
  of rescaling in ICE plane depictions is to use alibi = TRUE in plot.ICEuncrt().  Both plot.ICEwedge()
  and plot.ICEcolor() always default to alias axis scaling. Thus the equivariance property of the
  ICE confidence wedge is depicted as if the rays determining its upper and lower limits are invariant
  under changes in lambda.  
  
  The easy way to visualize a standardized ICE map is to always use the default value of lambda = 1 in
  ICEepmap() and ICEomega().  A standardized ICE map always has the following two characteristics:
  [i] it always assigns a zero overall preference to all (x, y) outcomes everywhere along the x = y
  ICE diagonal, and [ii] its iso-preference contours are always exactly symmetric about the x = -y
  (upper-left to lower-right) ICE diagonal.
}
\usage{
  ICEepmap(lambda = 1, beta = 1, gamma = 3+2*sqrt(2)) 
  ICEomega(lambda = 1, beta = 1, eta = 3+2*sqrt(2))
}
\arguments{
  \item{lambda}{Positive value for the fair, full-retail Shadow Price of Health.}
  \item{beta}{Positive Returns-to-Scale Power parameter for the ICE Preference Map.
   beta = 1 implies linear (constant) Returns-to-Scale.  A beta > 0 and < 1 implies diminishing
   Returns-to-Scale.  A beta > 1 implies increasing Returns-to-Scale.}
  \item{gamma}{Positive Directional Power parameter for ICEepmap().  The smallest
   reasonable value for gamma is usually gamma = beta, which yields a (generalized) linear map.
   The largest reasonable value for gamma is usually gamma = beta*(3+2*sgrt(2)), which yields a
   map that satisfies Cartesian Monotonicity and also yields WTP and WTA values within [0, +Inf).}
  \item{eta}{Positive Power Parameter Ratio for ICEomega().  Generalized linear maps
   result when eta = 1.  The eta for the more realistic Nonlinear maps is greater than one, but
   not greater than the ICE Omega limit of (3+2*sgrt(2)), which is approximately 5.828.  This
   upper limit on eta is required to assure that Cartesian Monotonicity of preferences holds.}
}
\details{
 The ICEepmap() and ICEomega() functions specify numerical values for the Shadow Price of Health
 Parameter, lambda, for the Returns to Scale Power Parameter, beta, and for either the Directional
 Power Parameter, gamma, or else the Power Parameter Ratio, eta = gamma / beta.
}
\value{
  Object of class ICEepmap containing an output list with the following items:
  \item{lambda}{Saved positive value of Shadow Price of Health, lambda, read by the print and
   plot methods for objects of class ICEepmap.}
  \item{beta}{Saved Positive Returns-to-Scale Power parameter, beta, read by the print and
   plot methods for objects of class ICEepmap.}
  \item{gamma}{Saved Positive Directional Power parameter, gamma, read by the print and
   plot methods for objects of class ICEepmap.}
}
\references{
 Cook JR, Heyse JF.  Use of an angular transformation for ratio estimation in cost-effectiveness
 analysis.  \emph{Statistics in Medicine}  2000; \bold{19}: 2989-3003.
 
 Obenchain RL.  Incremental Cost-Effectiveness (ICE) Preference Maps.  2001 \emph{JSM Proceedings
 (Biopharmaceutical Section) on CD-ROM}. (10 pages.)  Alexandria, VA: American Statistical
 Association.  2002.
 
 Obenchain RL.  ICE Preference Maps: Nonlinear Generalizations of Net Benefit and Acceptability.
 \emph{Health Serv Outcomes Res Method}  2008; \bold{8}: 31-56. DOI 10.1007/s10742-007-0027-2.
 Open Access.
}
\author{Bob Obenchain <wizbob@att.net>}
\seealso{\code{\link{plot.ICEepmap}} and \code{\link{print.ICEepmap}}}
\examples{
 pm <- ICEomega(beta=0.8)
 require(lattice)
 plot(pm)
}
\keyword{methods} 
\name{plot.ICEcolor}
\alias{plot.ICEcolor}
\title{Display Economic Preferences for ICE Outcomes within a Bootstrap Confidence Wedge}
\description{Assuming x is an object of class ICEcolor, the default invocation of plot(x)
 first displays a Histogram depicting the Distribution of Economic Preferences within an
 ICE Confidence Wedge then applies a rainbow of colors to these same points on a display
 of the ICE Bootstrap re-sampling Scatter. An invocation of the form plot(x, alibi=TRUE)
 uses alibi (rather than alias) scaling when displaying the Bootstrap Scatter plot. When
 ready, the user should press the ENTER Key to overwrite the Histogram display with the
 Scatter plot.
}
\usage{
  \method{plot}{ICEcolor}(x, alibi = FALSE, show = "Both", \dots)
}
\arguments{
  \item{x}{Required; Output list object of class ICEcolor.}
  \item{alibi}{Optional; Logical value of TRUE or FALSE to control scaling of axes. alibi
   = FALSE produces the default alias graphic in which points in the bootstrap uncertainty
   scatter are held fixed in space, and changes in lambda change the scaling (tick marks)
   along either the horizontal axis of a cost unit display or else along the vertical axis
   of an effe unit display.  alibi = TRUE produces an alibi graphic in which the scaling
   (and range) is the same along both axes, and changes in lambda cause the points in the
   bootstrap uncertainty scatter to move either left or right in a cost unit display or else
   up or down in an effe unit display.}
  \item{show}{Optional; Character string of "Hist", "RBOW" or "Both". show = "Hist" displays 
   a Histogram depicting the distribution of Economic Preferences within the ICE Wedge, while
   show = "RBOW" applies a rainbow of colors (Red--Tan--Yellow--Green) to depict the Economic
   Preference level associated with each re-sampled point within the ICE Wedge. show = "Both"
   is the default; it requires the user to press the ENTER key when ready to overwrite the 
   initial "Hist" display with the corresponding "RBOW" plot.}
  \item{\dots}{Optional; Argument(s) passed on to plot().}
}
\details{
  To illustrate the sensitivity of Economic Preferences to choice of lambda, multiple calls are 
  usually made to ICEcolor() for different values of lambda as well as for different choices of
  the beta and gamma parameters that determine the shape of (and spacing between) Indifference
  Curves on an ICE Economic Preference Map.
  
  The plot() of an object of class ICEcolor displays the Bootstrap Distribution of ICE Uncertainty
  using small, circular, colored dots (pch = 20).  Outcomes outside the Confidence Wedge are
  displayed in black, while outcomes inside the Wedge are displayed in a rainbow of colors (within
  the Red--Tan--Yellow--Green range) that represent Economic Preferences.
  
  Upper and lower ICE Confidence Limits are again displayed as Solid black Rays. The Dashed black
  LINE trough the ICE Origin, (0,0), has slope 1, indicating the ZERO Preference boundary that
  literally bisects the North-East and South-West ICE Quadrants. This Dashed Line may not "appear"
  to have slope 1, but that happens only when the horizontal and vertical RANGES displayed are
  quite different.
}
\value{NULL}
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
\seealso{\code{\link{ICEcolor}}, \code{\link{ICEscale}} and \code{\link{ICEwedge}}.}
\examples{
  data(dpwdg) 
  dpcol <- ICEcolor(dpwdg)
  plot(dpcol, show="RBOW")
  plot(dpcol, alibi=TRUE, show="RBOW")
}
\keyword{methods}

\name{plot.ICEuncrt}
\alias{plot.ICEuncrt}
\title{
 Display Scatter for a possibly Transformed Bootstrap Distribution of ICE Uncertainty
}
\description{
 Assuming x is an output list object of class ICEuncrt, the default invocation of plot(x)
 graphically displays the bootstrap distrib of ICE uncertainty currently stored in x.  An
 invocation of the form x10 <- plot(x, lfact=10) increases the value of x item lambda by a factor
 of 10, displays that transformed bootstrap distribution, and stores it in object x10.  When
 the x item unit is cost, an invocation of the form xs <- plot(x, swu=TRUE) displays the bootstrap
 distribution stored in x using effe units and stores the transformed distribution in object
 xs.  
}
\usage{
 \method{plot}{ICEuncrt}(x, lfact = 1, swu = FALSE, alibi = FALSE, \dots)
}
\arguments{
  \item{x}{Output list object of class ICEuncrt.}
  \item{lfact}{Positive factor multiplying the stored value of x item lambda.}
  \item{swu}{Logical value of TRUE or FALSE to control switching the stored value of
   x item unit between the 2 possibilities, cost and effe.}
  \item{alibi}{Logical value of TRUE or FALSE to control scaling of axes.  alibi = FALSE
   produces the default alias graphic in which points in the bootstrap uncertainty scatter are
   held fixed in space, and changes in lambda merely change the scaling (tick marks) along either
   the horizontal axis of a cost unit display or else along the vertical axis of an effe unit
   display.  alibi = TRUE produces an alibi graphic in which the scaling (and range) is the
   same along both axes, and changes in lambda cause the points in the bootstrap uncertainty
   scatter to literally move either left or right in a cost unit display or else up or dowm in
   an effe unit display.}
  \item{\dots}{Optional argument(s) passed on to plot().}
}
\details{After a single call to ICEuncrt() for an initial value of the Shadow Price of Health,
  lambda, and an initial choice of display unit (cost or effe), multiple calls to
  plot.ICEuncrt() are usually made.  Alternative economic choices for lambda can be suggested
  by making calls to ICEscale() with different values for lambda.  Because the Bootstrap
  Distribution of ICE Uncertainty is equivariant under changes in lambda, it is much faster to
  transform an existing bootstrap distribution than to generate a new one for a different value
  of lambda.  The print.ICEuncrt() and plot.ICEuncrt() functions thus have 2 special parameters,
  lfact and swa, that can change lambda and switch the units of measurement, respectively, without
  actually regenerating the bootstrap distribution via a call to ICEuncrt(). 
}
\value{
  Object of class ICEuncrt containing a possibly TRANSFORMED output list with items:
  \item{df}{Saved value of the name of the data.frame in the original call to ICEuncrt().}
  \item{lambda}{Possibly changed, positive value of lfact * (x item lambda).}
  \item{unit}{Possibly switched value of x item unit, cost or effe.}
  \item{R}{Saved integer value for number of bootstrap replications input to ICEuncrt.}
  \item{trtm}{Saved name of the treatment indicator within the df data.frame.}
  \item{xeffe}{Saved name of the treatment effectiveness variable within the df data.frame.}
  \item{ycost}{Saved name of the treatment cost variable within the df data.frame.}
  \item{effcst}{Saved value of the sorted 3-variable (trtm,effe,cost) data.frame.}
  \item{t1}{Observed value of (DeltaEffe, DeltaCost) when each patient is included exactly once.}
  \item{tb}{R x 2 matrix of values of (DeltaEffe, DeltaCost) computed by transformation.}
  \item{seed}{Saved value of the seed used to start pseudo random number generation.}
}
\references{
 Obenchain RL.  Issues and algorithms in cost-effectiveness inference.  \emph{Biopharmaceutical
 Reports} 1997; \bold{5(2)}: 1-7.  Washington, DC: American Statistical Association.

 Obenchain RL.  Resampling and multiplicity in cost-effectiveness inference.  \emph{Journal of
 Biopharmaceutical Statistics} 1999; \bold{9(4)}: 563--582.
 
 Cook JR, Heyse JF.  Use of an angular transformation for ratio estimation in cost-effectiveness
 analysis.  \emph{Statistics in Medicine}  2000; \bold{19}: 2989-3003.
}
\author{Bob Obenchain <softrx@iquest.net>}
\seealso{\code{\link{ICEuncrt}}, \code{\link{ICEscale}} and \code{\link{ICEwedge}}.}
\examples{
  data(dpunc)
  dpunc
  # Transformation of a bootstrap distribution is fast.
  dpuncs <- plot(dpunc, swu=TRUE)
}
\keyword{methods}
\keyword{hplot}
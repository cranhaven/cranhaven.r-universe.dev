\name{ICEscale}
\alias{ICEscale}
\title{
 ICEscale() functions compute or print ICE Statistical Inference Summary Statistics
 relative to choice for the numerical value of the Shadow Price of Health, lambda
}
\description{
 ICEscale() computes Summary Statistics for 2-sample, 2-variable inference where one variable
 is a measure of effectiveness (higher values are better) and the other variable is a measure
 of cost (lower values are better).  The 2 samples are of patients receiving only 1 of the 2
 possible treatments.  The treatment called new is the one with the higher numerical level
 for the specified treatment indicator variable, while the treatment called std corresponds
 to the lower numerical level.  The pivotal statistic for inference is (DeltaEffe, DeltaCost),
 which are the head-to-head mean differences for new treatment minus std treatment.  Each
 sample is assumed to provide unbiased estimates of the overall expected effectiveness and cost
 for that treatment.
}
\usage{
ICEscale(df, trtm, xeffe, ycost, lambda = 1, ceunit = "cost")
}
\arguments{
  \item{df}{Required; Existing data.frame object containing the trtm, xeffe and ycost variables.}
  \item{trtm}{Required; Name of the treatment indicator variable contained within the df
   data.frame that assumes one of only two different numerical values for each patient.}
  \item{xeffe}{Required; Name of the treatment effectiveness variable within the df data.frame.}
  \item{ycost}{Required; Name of the treatment cost variable within the df data.frame.}
  \item{lambda}{Optional; \code{lambda} strictly positive value for the Shadow Price of Health.}
  \item{ceunit}{Optional; \code{ceunit} character string containing either cost (default) or effe.}
}
\details{
  After an initial call with the default value of lambda = 1, multiple additional calls to
  ICEscale() with different numerical values for lambda are usually made at the very beginning of
  analyses using other functions from the ICEinfer package.  For example, the statistical
  choice for lambda assures that the DeltaEffe and DeltaCost mean treatment differences (new
  minus std) will have approximately equal variability when expressed in either cost or
  effe ceunits.  The power of ten value of lambda that is closest to the statistical value
  for lambda assures use of ceunits that, except for the position of the decimal point, are identical
  to the cost/effectiveness ratio implied by the scales in which data values are stored within
  the input data.frame.
}
\value{
  Object of class ICEscale containing an output list with the following items:
  \item{trtm}{Saved name of the treatment indicator within the input data.frame.}
  \item{xeffe}{Saved name of the treatment effectiveness variable within the input data.frame.}
  \item{ycost}{Saved name of the treatment cost variable within the input data.frame.}
  \item{effcst}{Saved value of the sorted 3-variable (trtm,effe,cost) data.frame.}
  \item{lambda}{Value for the Shadow Price of Health, lambda, input to ICEscals().}
  \item{t1}{Observed values of (DeltaEffe, DeltaCost) when each distinct patient is sampled
   exactly once.}
  \item{s1}{Observed values for the standard deviations of (DeltaEffe, DeltaCost) when each
   distinct patient is sampled exactly once.}
  \item{slam}{Statistical Shadow Price computed as s1[2]/s1[1] and rounded to digits = 3.}
  \item{potlam}{Power-of-Ten Shadow Price computed as 10\^(as.integer(log10(slam))).}
}
\references{
 Obenchain RL.  Issues and algorithms in cost-effectiveness inference.  \emph{Biopharmaceutical
 Reports} 1997; \bold{5(2)}: 1-7.  Washington, DC: American Statistical Association.
 
 Cook JR, Heyse JF.  Use of an angular transformation for ratio estimation in cost-effectiveness
 analysis.  \emph{Statistics in Medicine}  2000; \bold{19}: 2989-3003.
}
\author{Bob Obenchain <wizbob@att.net>}
\seealso{\code{\link{ICEscale}}, \code{\link{plot.ICEuncrt}} and \code{\link{print.ICEuncrt}}}
\examples{
  data(dulxparx)
  ICEscale(dulxparx, dulx, idb, ru)
}
\keyword{methods}
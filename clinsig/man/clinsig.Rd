\name{clinsig}
\alias{clinsig}
\title{Calculate clinical significance criteria.}
\description{Calculates one or more parametric or non-parametric criteria for
 clinical significance.}
\usage{
 clinsig(pre.x,post.x,dys.mct=NA,func.mct=NA,dys.disp=NA,func.disp=NA,
 mct="mean",disp="sd",disp.mult=c(2,2),dys.qnts=NA,
 dir.effect=NA,xlim=range(c(pre.x,post.x),na.rm=TRUE),
 ylim=range(c(pre.x,post.x),na.rm=TRUE),pch=par("pch"),
 do.plot=TRUE,point.id=NA,which.crit="c",coef.alpha=NA,rc.mult=1.96,...)
}
\arguments{
 \item{pre.x}{A vector of pre-intervention assessment scores.}
 \item{post.x}{A vector of post-intervention assessment scores for the same
  measure as \samp{pre.x}.}
 \item{dys.mct}{A Measure of Central Tendency (MCT) for the scores of the
  population of dysfunctional persons on the measure used. If absent, it
  will be estimated from \samp{pre.x}.}
 \item{func.mct}{A Measure of Central Tendency for the normative ("functional")
  scores on the measure.}
 \item{dys.disp}{A measure of DISPersion for the scores of the population of
  dysfunctional persons on the measure. If missing, it will also be estimated.}
 \item{func.disp}{A measure of DISPersion for the normative scores on the
  measure.}
 \item{mct}{The name of a Measure of Central Tendency to be used. Defaults to
  the mean.}
 \item{disp}{The name of a measure of DISPersion to be used. Defaults to the
  standard deviation.}
 \item{disp.mult}{The multiple(s) of the dispersion measure to be used in
  calculations. If different multiples are to be used for the pre/post measures
  and the normative scores, pass a two element vector containing the multipliers
  for the pre/post and then the normative dispersions. Defaults to 2 for both.}
 \item{dys.qnts}{The lower and upper quantiles of the pre-assessment intervention
  scores to be used in calculating the "a" criterion. If a two element vector of
  quantiles such as \samp{c(0.16,0.84)} is passed, this will be used instead of
  \samp{dys.disp} or the dispersion calculated for \samp{pre.x}. See Details.}
 \item{dir.effect}{The direction of effect expected of the intervention.
  This argument should only take the values of -1 or 1. see Details.}
 \item{xlim,ylim}{Allows the user to set the x and y axes to the range desired.
  Defaults to range(pre.x) and range(post.x) respectively.}
 \item{pch}{Symbols to use in the plot.}
 \item{do.plot}{Whether to display a significance plot.}
 \item{point.id}{Optional text to be displayed instead of symbols for each score
  pair.}
 \item{which.crit}{Which criterion should be used if reliable change
  measure is to be calculated.}
 \item{coef.alpha}{sample Cronbach's alpha for the instrument used to 
  measure the dysfunction. If NOT NA, this causes the reliable change
  measure to be calculated.}
 \item{rc.mult}{The multiplier for the standard error in the reliable
  change measure. See Details.}
 \item{...}{Additional arguments passed to \samp{plot}.}
}
\value{
 A list containing:
 \item{pre.x}{the pre-assessment scores}
 \item{post.x}{the post-assessment scores}
 \item{crit}{the three estimated criteria, some of which may be NA}
 \item{sigsums}{the number of scores that met each of the three criteria}
 \item{pre.mct}{the pre-intervention MCT}
 \item{post.mct}{the post-intervention MCT}
 \item{func.mct}{the normative MCT}
 \item{mct}{the name of the function used to calculate the MCTs}
 \item{disp}{the name of the function used to calculate the dispersions}
 \item{post.n}{the number of post-intervention assessments}
 \item{passed}{a vector of three logicals indicating whether the post-intervention
  MCT met the respective criteria}
 \item{relchng}{the reliable change measure calculated. NA if not.}
 \item{dir.effect}{the direction of effect. see Details.}
 \item{which.crit}{which of the three criteria should be used in 
  calculating the reliable change measure.}

 As a side effect, a scatterplot of the pre- and post-scores and MCTs with the
 calculated cutoff scores as lines is displayed if \samp{do.plot} is TRUE.
}
\details{
 The \samp{clinsig} function calculates both parametric and non-parametric versions
 of the Jacobson-Truax estimates of clinical significance. The number of estimates
 produced will depend upon which arguments are supplied. The \samp{"a"} estimate
 is always calculated as it only requires the measure of central tendency (MCT) and
 dispersion of the pre-intervention scores. The \samp{c} estimate requires
 that the normal (non-pathologic) measure of central tendency be specified.
 The \samp{"b"} estimate requires the dispersion of the normal MCT. If the
 dispersion of the dysfunctional scores is not supplied, the function will
 estimate this with the function named in \samp{disp}. The user can calculate a
 measure of dispersion and pass it as \samp{dys.disp} or write a function and
 pass the name of that function as \samp{disp}. Note that the use of non-
 parametric estimates is not included in the work Jacobson and colleagues.
 
 Using the \samp{"a"} cutoff means that post-intervention scores are 
 significant if they are at least a specified distance from the MCT of 
 pre-intervention scores in the direction of the normal MCT (i.e. they 
 are "far enough" from the dysfunctional MCT). The \samp{"b"} cutoff 
 means that post-intervention scores are significant if they are within 
 a specified distance of the normal MCT (i.e. they are "close enough" 
 to the normal MCT). This criterion is typically used with overlapping 
 distributions. The \samp{"c"} cutoff means that post-intervention scores 
 are in the direction of the normal MCT and beyond the weighted mean of 
 the two MCTs (i.e. they are "on the normal side" of the average of the 
 MCTs). The "c" criterion is usually recommended for non-overlapping 
 distributions of pre- and post-scores. Look at the second plot in the 
 examples for an illustration of this in which there is a large effect 
 and a clear separation of pre- and post-scores.

 The direction of effect is calculated as the sign of the functional
 (normal) MCT minus the dysfunctional MCT. If one or both of these are 
 missing, the pre- and post-MCTs are substituted. This assumes that the 
 post-MCT is in the expected direction toward the functional MCT. If this 
 is not the case, the user should set \samp{dir.effect} to 1 (higher 
 scores = improvement) or -1 (lower scores = improvement) or the function 
 may report improvement when the scores are actually indicating 
 deterioration.

 If only the pre-intervention and post-intervention scores are provided, 
 the function calculates a cutoff score for significant change based upon
 \samp{disp.mult} times the measure of dispersion for the pre-intervention
 scores if \samp{dys.disp} is specified. However, if \samp{dys.qnts} is 
 not NA, the \samp{"a"} cutoff score is set to one of the quantiles of 
 the pre-intervention score distribution. If the normal MCT is supplied 
 and it is less than the pre-intervention MCT, or \samp{dir.effect} is -1, 
 the lower quantile is used. In all other cases, the upper cutoff is 
 calculated. Remember that the quantile is not adjusted for 
 \samp{disp.mult}.
 
 The reliable change measure is calculated following 
 Evans, Margison & Barkham (1998). This
 calculation assumes that the sample score distribution is normal. It will
 be calculated with the measures specified in samp{mct} and samp{disp},
 but unless these are the defaults, it is not guaranteed to be correct.
}
\author{Jim Lemon - thanks to Elisa Napoleone for requesting the reliable
 change measure.}
\references{
 Evans, C., Margison, F. & Barkham, M. (1998) The contribution of reliable 
 and clinically significant change methods to evidence-based mental health.
 Evidence Based Mental Health, 1: 70-72.

 Jacobson, N.S. & Truax, P. (1991) Clinical significance: a statistical
 approach to defining meaningful change in psychotherapy research. Journal
 of Consulting and Clinical Psychology, 59(1): 12-19.
}
\examples{
 pre.x<-runif(30,3,6)
 post.x<-runif(30,1,4)
 clinsig(pre.x,post.x,func.mct=1,func.disp=1,xlim=c(1,6),ylim=c(1,6))
 # simulate scores on a typical psychological assessment with a limited
 # range and a large separation between the pre- and post- assessments
 pre.x<-c(3,3,4,5,5,6,6,6,6,7,7,7,8,8,8,8,9,9,10,10)
 post.x<-c(13,12,15,14,12,18,13,17,NA,20,16,22,23,15,19,17,18,21,13,15)
 big.sep<-clinsig(pre.x,post.x,mct="median",disp="mad",func.mct=19,func.disp=2,
  do.plot=FALSE)
 hist(big.sep,main="Widely separated samples")
 legend(20,3.8,c("Pre","Post"),fill=c("red","green"))
 # now squeeze the two samples together to show how the criteria change positions
 post.x<-post.x-7
 little.sep<-clinsig(pre.x,post.x,mct="median",disp="mad",func.mct=15,func.disp=2,
  do.plot=FALSE)
 hist(little.sep,main="Closely spaced samples")
 legend(12.5,3.8,c("Pre","Post"),fill=c("red","green"))
 # example from Evans, Margison & Barkham, 1998 with simulated data
 set.seed(12345)
 # values from EMB 
 pre_mct<-1.81
 pre_disp<-0.53
 post_mct<-0.79
 post_disp<-0.5
 func_mct<-0.72
 func_disp<-0.57
 # accept EMB's normality of distribution
 pre_treat<-rnorm(40,pre_mct,pre_disp)
 post_treat<-rnorm(40,post_mct,post_disp)
 # make sure that no scores go below zero
 post_treat[post_treat<0]<-0
 emb<-clinsig(pre_treat,post_treat,
  dys.mct=pre_mct,func.mct=func_mct,
  dys.disp=pre_disp,func.disp=func_disp,
  coef.alpha=0.89,
  main="Clinical significance plot with reliable change window")
 print(emb)
 hist(emb)
}

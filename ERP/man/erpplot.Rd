\name{erpplot}
\alias{erpplot}
\title{
Plot of ERP curves or effect curves (difference curve for example) with confidence intervals.
}
\description{
The function is designed to plot either raw ERP curves or estimated effect curves. In the case an effect curve is displayed, then 
spline smoothing can be used to obtained more regular curves. Pointwise or simultaneous confidence intervals can be added to the plot.
}
\usage{
erpplot(dta,design=NULL,effect=1,interval=c("none","pointwise","simultaneous"),
alpha=0.05,frames=NULL,ylim=NULL,nbs=NULL, ...)
}
\arguments{
  \item{dta}{Data frame containing the ERP curves: each column corresponds to a time frame and each row to a curve.}
  \item{design}{Design matrix of the nonnull model for the relationship between the ERP and the experimental variables.
          Typically the output of the function model.matrix. Only needed if an effect curve is to be plotted.}
  \item{effect}{Integer value giving the column number of the design matrix corresponding to the effect curve to be plotted}
  \item{interval}{Type of confidence interval to be added to the plot of an effect curve. Default is "none" for no confidence interval. Other possibilities are
  "pointwise" for a pointwise confidence interval or "simultaneous" for both a pointwise and a simultaneous confidence interval.}
  \item{alpha}{The confidence level for the confidence interval is 1-\code{alpha}.}
  \item{frames}{Sequence of time frames. Default is NULL, in which case frames is just the sequence of intergers between one and the total number of frames.}
  \item{ylim}{Limits for the y-axis. Default is NULL (set into the function).}
  \item{nbs}{Number of B-spline basis for an eventual spline smoothing of the effect curve. Default is NULL for no smoothing.}
  \item{\dots}{Graphical parameters (see par) and any further arguments of plot, typically plot.default, may also be supplied as arguments to this function. 
  Hence, the high-level graphics control arguments described under par and the arguments to title may be supplied to this function.}
}
\value{The function generates a plot, but does not return any other numerical outputs.}
\author{David Causeur, IRMAR, UMR 6625 CNRS, Agrocampus Ouest, Rennes, France.}
\seealso{\code{\link{matplot}}}

\examples{
data(impulsivity)

# Comparison of ERP curves in the two conditions, within experimental group 'High', at channel CPZ 

erpdta.highCPZ = impulsivity[(impulsivity$Group=="High")&(impulsivity$Channel=="CPZ"),5:505] 
   # ERP curves for subjects in group 'High'
covariates.highCPZ = impulsivity[(impulsivity$Group=="High")&(impulsivity$Channel=="CPZ"),1:4]
covariates.highCPZ = droplevels(covariates.highCPZ)
   # Experimental covariates for subjects in group 'High'

colors = ifelse(covariates.highCPZ$Condition=="Success","darkgray","orange")
   # Gives a color code for each condition
time_pt = seq(0,1000,2)     # sequence of time points (1 time point every 2ms in [0,1000])
erpplot(erpdta.highCPZ,frames=time_pt,xlab="Time (ms)",lty=1,col=colors,
        ylab=expression(ERPs~(mu~V)),bty="l",
        cex.axis=1.25,cex.lab=1.25)
   # Displays the ERP curves in group 'High', at channel CPZ 
legend("topright",bty="n",lty=1,col=c("darkgray","orange"),legend=c("Success","Failure"))
title("ERP curves",cex.main=1.25)
mtext(paste("12 subjects - Group 'High'",sep=""),cex=1.25)


design = model.matrix(~C(Subject,sum)+Condition,data=covariates.highCPZ)
   # Design matrix to compare ERP curves in the two conditions
design0 = model.matrix(~C(Subject,sum),data=covariates.highCPZ)
   # Design matrix for the null model (no condition effect)

nbs = 20                    # Number of B-splines for the plot of the effect curve
effect=which(colnames(design)=="ConditionSuccess")
erpplot(erpdta.highCPZ,design=design,frames=time_pt,effect=effect,xlab="Time (ms)",
        ylab=expression(Effect~curve~(mu~V)),bty="l",ylim=c(-3,3),nbs=nbs,
        cex.axis=1.25,cex.lab=1.25,interval="simultaneous")
   # with interval="simultaneous", both the pointwise and the simultaneous confidence bands
   # are plotted
title("Success-Failure effect curve with 95 percent C.I.",cex.main=1.25)
mtext(paste("12 subjects - Group 'High' - ",nbs," B-splines",sep=""),cex=1.25)

}

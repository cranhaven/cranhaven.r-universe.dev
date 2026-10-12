\name{erpavetest}
\alias{erpavetest}
\title{Significance testing of averaged ERPs.}
\description{The function first calculates averaged ERP values within a predetermined number of equally-spaced intervals then tests
for significance of the relationship between averaged ERPs and covariates in a linear model framework.}
\usage{
erpavetest(dta, design, design0 = NULL, nintervals = 10, 
method = c("none","BH","holm","hochberg","hommel","bonferroni","BY","fdr"),alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{dta}{Data frame containing the ERP curves: each column corresponds to a time frame and each row to a curve.}
\item{design}{Design matrix of the nonnull model for the relationship between the ERP and the experimental variables.
           Typically the output of the function model.matrix}
\item{design0}{Design matrix of the null model. Typically a submodel of the nonnull model, obtained by removing columns from design.
           Default is NULL, corresponding to the model with no covariates.}
\item{nintervals}{Number of intervals in the partition of the whole interval of observation. Default is 10.}
\item{method}{FDR- or FWER- controlling multiple testing procedures as available in the function p.adjust. Default is "none", for no multiplicity correction.}
\item{alpha}{The FDR or FWER control level. Default is 0.05}
}
\value{
\item{pval}{p-values of the tests.}
\item{correctedpval}{Corrected p-values, for the multiplicity of tests. Depends on the multiple testing method (see function p.adjust).}
\item{significant}{Indices of the time points for which the test is positive.}
\item{segments}{Factor giving the membership of timepoints to each interval in the partition.}
\item{breaks}{Breakpoints of the partition.}
\item{test}{Pointwise F-statistics if p>1, where p is the difference between the numbers of parameters in the nonnull and null models. Otherwise, if p=1, the function
   returns pointwise t-statistics (signed square-roots of F-statistics).}
\item{df1}{Residual degrees of freedom for the nonnull model.}
\item{df0}{Residual degrees of freedom for the null model.}
\item{signal}{Estimated signal: a pxT matrix, where T the number of frames.}
\item{r2}{R-squared values for each of the T linear models.}
}
\author{
David Causeur, IRMAR, UMR 6625 CNRS, Agrocampus Ouest, Rennes, France.
}
\seealso{
\code{\link{erptest}}, \code{\link{erpfatest}}, \code{\link{gbtest}}, \code{\link[stats]{p.adjust}}
}
\examples{

data(impulsivity)

# Paired t-tests for the comparison of the ERP curves in the two conditions, 
# within experimental group High, at channel CPZ

erpdta.high = impulsivity[impulsivity$Group=="High",5:505] 
   # ERP curves for subjects in group 'High'
covariates.high = impulsivity[impulsivity$Group=="High",1:4]
   # Experimental covariates for subjects in group 'High'

design = model.matrix(~C(Subject,sum)+Condition,data=covariates.high)
   # Design matrix to compare ERP curves in the two conditions
design0 = model.matrix(~C(Subject,sum),data=covariates.high)
   # Design matrix for the null model (no condition effect)

tests = erpavetest(erpdta.high,design,design0)
   
time_pt = seq(0,1000,2)     # sequence of time points (1 time point every 2ms in [0,1000])
nbs = 20                    # Number of B-splines for the plot of the effect curve
effect=which(colnames(design)=="ConditionSuccess")
erpplot(erpdta.high,design=design,frames=time_pt,effect=effect,xlab="Time (ms)",
        ylab=expression(Effect~curve~(mu~V)),bty="l",ylim=c(-3,3),nbs=nbs,
        cex.axis=1.25,cex.lab=1.25,interval="simultaneous")
   # with interval="simultaneous", both the pointwise and the simultaneous confidence bands
   # are plotted
abline(v=time_pt[tests$breaks],lty=2,col="darkgray")
   # Add a grid to show breakpoints
points(time_pt[tests$significant],rep(0,length(tests$significant)),pch=16,col="blue")
   # Identifies significant time points by blue dots
title("Success-Failure effect curve with 95 percent C.I.",cex.main=1.25)
mtext(paste("12 subjects - Group 'High' - ",nbs," B-splines",sep=""),cex=1.25)


}


\name{erptest}
\alias{erptest}
\title{FDR- and FWER-controlling Multiple testing of ERP data}
\description{
Classical FDR- and FWER-controlling multiple testing procedures for ERP data in a linear model framework.
}
\usage{
erptest(dta,design,design0=NULL,
method=c("BH","holm","hochberg","hommel","bonferroni","BY","fdr","none"),alpha=0.05,
pi0 = 1, nbs = NULL)
}
\arguments{
  \item{dta}{Data frame containing the ERP curves: each column corresponds to a time frame and each row to a curve.}
  \item{design}{Design matrix of the nonnull model for the relationship between the ERP and the experimental variables. Typically the output of the function
  model.matrix}
  \item{design0}{Design matrix of the null model. Typically a submodel of the nonnull model, obtained by removing columns from design. Default is 
  NULL, corresponding to the model with no covariates.}
  \item{method}{FDR- or FWER- controlling multiple testing procedures as available in the function p.adjust. Default is "BH", 
  for the Benjamini-Hochberg procedure (see Benjamini and Hochberg, 1995).}
  \item{alpha}{The FDR or FWER control level. Default is 0.05}
  \item{pi0}{An estimate of the proportion of true null hypotheses, which can be plugged into an FDR controlling multiple testing procedure to improve
  its efficiency. Default is 1, corresponding to the classical FDR controlling procedures. If NULL, the proportion is estimated using 
  the function pval.estimate.eta0 of package fdrtool, with the method proposed by Storey and Tibshirani (2003).}
  \item{nbs}{Number of B-spline basis for an eventual spline smoothing of the effect curve. Default is NULL for no smoothing.}
}
\value{
\item{pval}{p-values of the tests.}
\item{correctedpval}{Corrected p-values, for the multiplicity of tests. Depends on the multiple testing method (see function p.adjust).}
\item{significant}{Indices of the time points for which the test is positive.}
\item{pi0}{Value for pi0: if the input argument pi0 is NULL, the output is the estimated proportion of true null hypotheses using the method 
by Storey and Tibshirani (2003).}
\item{test}{Pointwise F-statistics if p>1, where p is the difference between the numbers of parameters in the nonnull and null models. Otherwise, if p=1, the function
   returns pointwise t-statistics (signed square-roots of F-statistics).}
\item{df1}{Residual degrees of freedom for the nonnull model.}
\item{df0}{Residual degrees of freedom for the null model.}
\item{signal}{Estimated signal: a p x T matrix, where p is the difference between the numbers of parameters in the nonnull and null models and T the number of time points.}
\item{sd}{T-vector of estimated residual standard deviations where T is the number of time points.}
\item{r2}{R-squared values for each of the T linear models.}
\item{sdsignal}{Standard deviations of the estimated signal: a p x T matrix, where p is the difference between the numbers of parameters in the nonnull and null models and T the number of time points.}
\item{residuals}{n x T matrix of residuals of the fit of the nonnull model.}
\item{coef}{Estimated regression coefficients: a q x T matrix, where q is the number of parameters in the nonnull model and T the number of time points.}
}
\author{David Causeur, IRMAR, UMR 6625 CNRS, Agrocampus Ouest, Rennes, France.}
\seealso{
\code{\link{erpavetest}}, \code{\link{erpfatest}}, \code{\link{gbtest}}, \code{\link[stats]{p.adjust}}
}
\examples{
data(impulsivity)

# Paired t-tests for the comparison of the ERP curves in the two conditions, 
# within experimental group High, at channel CPZ

erpdta.highCPZ = impulsivity[(impulsivity$Group=="High")&(impulsivity$Channel=="CPZ"),5:505] 
   # ERP curves for subjects in group 'High'
covariates.highCPZ = impulsivity[(impulsivity$Group=="High")&(impulsivity$Channel=="CPZ"),1:4]
covariates.highCPZ = droplevels(covariates.highCPZ)
   # Experimental covariates for subjects in group 'High'

design = model.matrix(~C(Subject,sum)+Condition,data=covariates.highCPZ)
   # Design matrix to compare ERP curves in the two conditions
design0 = model.matrix(~C(Subject,sum),data=covariates.highCPZ)
   # Design matrix for the null model (no condition effect)

tests = erptest(erpdta.highCPZ,design,design0)

time_pt = seq(0,1000,2)     # sequence of time points (1 time point every 2ms in [0,1000])
nbs = 20                    # Number of B-splines for the plot of the effect curve
effect=which(colnames(design)=="ConditionSuccess")
erpplot(erpdta.highCPZ,design=design,frames=time_pt,effect=effect,xlab="Time (ms)",
        ylab=expression(Effect~curve~(mu~V)),bty="l",ylim=c(-3,3),nbs=nbs,
        cex.axis=1.25,cex.lab=1.25,interval="simultaneous")
   # with interval="simultaneous", both the pointwise and the simultaneous confidence bands
   # are plotted
points(time_pt[tests$significant],rep(0,length(tests$significant)),pch=16,col="blue")
   # Identifies significant time points by blue dots
title("Success-Failure effect curve with 95 percent C.I.",cex.main=1.25)
mtext(paste("12 subjects - Group 'High' - ",nbs," B-splines",sep=""),cex=1.25)

}

\name{gbtest}
\alias{gbtest}
\title{The Guthrie-Buchwald procedure for significance analysis of ERP data}
\description{Monte-Carlo implementation of the Guthrie-Buchwald procedure (see Guthrie and Buchwald, 1991) which accounts for the
auto-correlation among test statistics to control erroneous detections of short intervals.}
\usage{
gbtest(dta, design, design0 = NULL, graphthresh = 0.05, nsamples = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dta}{Data frame containing the ERP curves: each column corresponds to a time frame and each row to a curve.}
  \item{design}{Design matrix of the nonnull model for the relationship between the ERP and the experimental variables. Typically the output of the function
  model.matrix}
  \item{design0}{Design matrix of the null model. Typically a submodel of the nonnull model, obtained by removing columns from design. Default is 
  NULL, corresponding to the model with no covariates.}
  \item{graphthresh}{Graphical threshold (see Guthrie and Buchwald, 1991). Default is 0.05. As the FDR control level, the smaller is the graphical threshold, the more conservative is the
  procedure.}
  \item{nsamples}{Number of samples in the Monte-Carlo method to estimate the residual covariance. Default is 1000.}
}
\details{
The Guthrie-Buchwald method starts from a preliminary estimation of r, the lag-1 autocorrelation, among test statistics.
Then, the null distribution of the lengths of the intervals I_alpha = t : pvalue_t <= alpha , where alpha is the
so-called graphical threshold parameter of the method, is obtained using simulations of p-values p_t associated to
auto-regressive t-test process of order 1 with mean 0 and auto-correlation r. Such an interval I_alpha is declared
significant if its length exceeds the (1-alpha)-quantile of the null distribution. Note that the former method is
designed to control erroneous detections of short significant intervals but not to control any type-I error rate.
}
\value{
\item{nbsignifintervals}{Number of significant intervals.}
\item{intervals}{List of length nbsignifintervals which components give the indices of each significant intervals.}
\item{significant}{Indices of the time points for which the test is positive.}
\item{signal}{Estimated signal: a pxT matrix, where p is the difference between the numbers of parameters in the nonnull and null models and T the number of frames.}
\item{rho}{Estimated lag-1 auto-correlation.}
\item{r2}{R-squared values for each of the T linear models.}
}
\references{
Guthrie, D. and Buchwald, J.S. (1991). Significance testing of difference potentials. Psychophysiology, 28, 240-244.

Sheu, C.-F., Perthame, E., Lee Y.-S. and Causeur, D. (2016). Accounting for time dependence in large-scale multiple testing of event-related potentials data. Annals of Applied Statistics. 10(1), 219-245.
}
\author{
David Causeur - david.causeur@agrocampus-ouest.fr and Mei-Chen Chu (National Cheng-Kung University, Tainan, Taiwan)
}
\seealso{
\code{\link{erpavetest}}, \code{\link{erpfatest}}, \code{\link{erptest}}
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

tests = gbtest(erpdta.highCPZ,design,design0)

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

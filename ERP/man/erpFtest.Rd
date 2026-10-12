\name{erpFtest}
\alias{erpFtest}
\title{Functional Analysis-of-Variance (Anova) testing of Event-Related Potentials (ERP) data}
\description{Factor-based generalized Likelihood-Ratio Test for functional ANOVA in ERP designs.
The procedure is described in details in Causeur, Sheu, Perthame and Rufini (2018).
}
\usage{
erpFtest(dta,design,design0=NULL,nbf=NULL,pvalue=c("Satterthwaite","MC","none"), 
nsamples=200,min.err=0.01,verbose=FALSE,
nbfmax=min(c(nsamples,nrow(design)))-ncol(design)-1,
wantplot=ifelse(is.null(nbf),TRUE,FALSE),svd.method=c("fast.svd","irlba"))
}
\arguments{
  \item{dta}{Data frame containing the ERP measurements: each column corresponds to a time frame and each row to a curve.}
  \item{design}{Design matrix of the nonnull model for the relationship between the ERP and the experimental variables.
          Typically the output of the function model.matrix}
  \item{design0}{Design matrix of the null model. Typically a submodel of the nonnull model, obtained by removing columns from design.
          Default is NULL, corresponding to the model with no covariates.}
  \item{nbf}{Number of factors in the residual covariance model.
          Default is NULL: the number of factors is determined by minimization of the variance inflation criterion as
          suggested in Friguet et al. (2009).}
  \item{pvalue}{If "Satterthwaite", then the Monte-Carlo p-value is calculated with a Satterthwaite approximation of the null distribution. 
   The other possible value is "none", for no calculation of the p-value or "MC" for the Monte-Carlo p-value.}
  \item{nsamples}{Number of Monte-Carlo samples for the estimation of the null distribution. Default is nsamples=200, recommended when the Satterthwaite approximation is used.}
  \item{min.err}{Control parameter for convergence of the iterative algorithm. Default is 1e-02.}
  \item{verbose}{If TRUE, details are printed along the iterations of the algorithm. Default is FALSE.}
  \item{nbfmax}{Only required if nbf=NULL. The largest possible number of factors.}
  \item{wantplot}{If TRUE, a diagnostic plot is produced to help choosing the number of factors. Only active if nbf=NULL.}
  \item{svd.method}{the EM algorithm starts from an SVD estimation of the factor model parameters. The default option to implement this 
      SVD is \code{\link[corpcor]{fast.svd}}. An alternative option is an approximate but faster SVD by function 
      \code{\link[irlba]{irlba}}.}
}
\details{The method is described in Causeur et al. (2018). It can be viewed as a Hotelling's T-squared multivariate ANOVA test based on a 
factor decomposition of the time-dependence across pointwise F-tests. For 1-way functional ANOVA, it can be compared to function \code{anova.onefactor}
in package \code{fda.usc}.
}
\value{
\item{Fgls}{The generalized LRT statistics comparing the nonnull model with design matrix \code{design} and the null model with design matrix \code{design0}.}
\item{Fols}{The LRT statistics ignoring time-dependence across pointwise F-tests.}
\item{pval}{p-value of the generalized LRT statistics.}
\item{pval.Fols}{p-value of the LRT statistics ignoring time-dependence across pointwise F-tests.}
}
\references{
Causeur, D., Sheu, C.-F., Perthame, E. and Rufini, F. (2018). A functional generalized F-test for signal detection with applications to event-related potentials significance analysis. Submitted.
}
\author{
David Causeur, IRMAR, UMR 6625 CNRS, Agrocampus Ouest, Rennes, France.
Flavia Rufini, Agrocampus Ouest, Rennes, France.
}
\examples{
data(impulsivity)

# Comparison of ERP curves in the two conditions, within experimental group 'High', at channel CPZ 

erpdta.highCPZ = impulsivity[(impulsivity$Group=="High")&(impulsivity$Channel=="CPZ"),5:505] 
   # ERP curves for subjects in group 'High'
covariates.highCPZ = impulsivity[(impulsivity$Group=="High")&(impulsivity$Channel=="CPZ"),1:4]
covariates.highCPZ = droplevels(covariates.highCPZ)
   # Experimental covariates for subjects in group 'High'

design = model.matrix(~C(Subject,sum)+Condition,data=covariates.highCPZ)
   # Design matrix to compare ERP curves in the two conditions
design0 = model.matrix(~C(Subject,sum),data=covariates.highCPZ)
   # Design matrix for the null model (no condition effect)

Ftest = erpFtest(erpdta.highCPZ,design,design0,nbf=NULL,wantplot=TRUE,pvalue="none")
   # with pvalue="none", just to choose a number of factors
Ftest = erpFtest(erpdta.highCPZ,design,design0,nbf=6)
Ftest$pval          # p-value of the Generalized LRT statistic
Ftest$pval.Fols     # p-value of the LRT statistic when time-dependence is ignored

time_pt = seq(0,1000,2)     # sequence of time points (1 time point every 2ms in [0,1000])
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

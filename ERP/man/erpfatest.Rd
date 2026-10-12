\name{erpfatest}
\alias{erpfatest}
\title{Adaptive Factor-Adjustement for multiple testing of ERP data}
\description{Adaptive factor-adjusted FDR- and FWER-controlling multiple testing procedures for ERP data.
The procedure is described in detail in Sheu, Perthame, Lee, and Causeur (2016).
}
\usage{
erpfatest(dta, design, design0 = NULL, 
method = c("BH","holm","hochberg","hommel","bonferroni","BY","fdr","none"),nbf = NULL,
nsamples = 200, significance = c("Satterthwaite", "none"),
nbfmax = min(c(nsamples, nrow(design))) - ncol(design) - 1, alpha = 0.05, pi0 = 1,
wantplot = ifelse(is.null(nbf), TRUE, FALSE), s0 = NULL, min.err = 0.01, maxiter = 5, 
verbose = FALSE, svd.method = c("fast.svd", "irlba"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dta}{Data frame containing the ERP measurements: each column corresponds to a time frame and each row to a curve.}
  \item{design}{Design matrix of the nonnull model for the relationship between the ERP and the experimental variables.
          Typically the output of the function model.matrix}
  \item{design0}{Design matrix of the null model. Typically a submodel of the nonnull model, obtained by removing columns from design.
          Default is NULL, corresponding to the model with no covariates.}
  \item{method}{FDR- or FWER- controlling multiple testing procedures as available in the function p.adjust. Default is "BH",
          for the Benjamini-Hochberg procedure (see Benjamini and Hochberg, 1995).}
  \item{nbf}{Number of factors in the residual covariance model.
          Default is NULL: the number of factors is determined by minimization of the variance inflation criterion as
          suggested in Friguet et al. (2009).}
  \item{nsamples}{Number of Monte-Carlo samples for the estimation of the null distribution. Default is nsamples=200, recommended when the Satterthwaite approximation is used.}
  \item{significance}{If "Satterthwaite", then the Monte-Carlo p-value is calculated with a Satterthwaite approximation of the null distribution. The other possible value is "none", for no calculation of the p-value.}
  \item{nbfmax}{Only required if nbf=NULL. The largest possible number of factors.}
  \item{alpha}{The FDR or FWER control level. Default is 0.05}
  \item{pi0}{An estimate of the proportion of true null hypotheses, which can be plugged into an FDR controlling multiple
          testing procedure to improve its efficiency.
          Default is 1, corresponding to the classical FDR controlling procedures. If NULL, the proportion is estimated
          using the function pval.estimate.eta0 of package fdrtool, with the method proposed by Storey and Tibshirani (2003).}
  \item{wantplot}{If TRUE, a diagnostic plot is produced to help choosing the number of factors. Only active if nbf=NULL.}
  \item{s0}{Prior knowledge of the time frames for which no signal is expected. For example,
          s0=c(1:50, 226:251) specifies that the first 50 time frames and time frames between 226 and 251 are
          known not to contain ERP signal. s0 can also be specified by giving the lower and upper fraction of the
          entire time interval in which the signal is to be searched for. For example: s0=c(0.2, 0.9) means that
          ERP signals are not expected for
          for the first 20 percent and last 10 percent of the time frames measured. Defaul is NULL and it
          initiates a data-driven determination of s0.}
  \item{min.err}{Control parameter for convergence of the iterative algorithm. Default is 1e-03.}
  \item{maxiter}{Maximum number of iterations in algorithms. Default is 5.}
  \item{verbose}{If TRUE, details are printed along the iterations of the algorithm. Default is FALSE.}
  \item{svd.method}{the EM algorithm starts from an SVD estimation of the factor model parameters. The default option to implement this 
      SVD is \code{\link[corpcor]{fast.svd}}. An alternative option is an approximate but faster SVD by function 
      \code{\link[irlba]{irlba}}.}
}
\details{The method is described in Sheu et al. (2016). It combines a decorrelation step based on a regression factor model as
in Leek and Storey (2008), Friguet et al. (2009) or Sun et al. (2012) with an adaptive estimation of the ERP signal.
The multiple testing corrections of the p-values are described in the help documentation of the function \code{\link[stats]{p.adjust}}.
}
\value{
\item{pval}{p-values of the Adaptive Factor-Adjusted tests.}
\item{correctedpval}{Corrected p-values, for the multiplicity of tests. Depends on the multiple testing method (see
 function p.adjust).}
\item{significant}{Indices of the time points for which the test is positive.}
\item{pi0}{Value for pi0: if the input argument pi0 is NULL, the output is the estimated proportion of
               true null hypotheses using the method by Storey and Tibshirani (2003).}
\item{test}{Pointwise factor-adjusted F-statistics if p>1, where p is the difference between the numbers of parameters in the nonnull and null models. Otherwise, if p=1, the function
   returns pointwise factor adjusted t-statistics (signed square-roots of F-statistics).}
\item{df1}{Residual degrees of freedom for the nonnull model.}
\item{df0}{Residual degrees of freedom for the null model.}
\item{nbf}{Number of factors for the residual covariance model.}
\item{signal}{Estimated signal: a pxT matrix, where p is the difference between the numbers of parameters in the nonnull and null models and T the number of frames.}
\item{r2}{R-squared values for each of the T linear models.}
}
\references{
Causeur, D., Chu, M.-C., Hsieh, S., Sheu, C.-F. 2012. A factor-adjusted multiple testing procedure for ERP data analysis. Behavior Research Methods, 44, 635-643.

Friguet, C., Kloareg, M., Causeur, D. 2009. A factor model approach to multiple testing under dependence. Journal of the American Statistical Association, 104, 1406-1415.

Leek, J.T., Storey, J.D. 2008. A general framework for multiple testing dependence. Proceedings of the National Academy of Sciences of the United States of America, 105, 18718-18723.

Sheu, C.-F., Perthame, E., Lee Y.-S. and Causeur, D. 2016. Accounting for time dependence in large-scale multiple testing of event-related potential data. Annals of Applied Statistics. 10(1), 219-245.

Storey, J. D., Tibshirani, R. 2003. Statistical significance for genome-wide experiments. Proceedings of the National Academy of Sciences of the United States of America, 100, 9440-9445.

Sun, Y., Zhang, N.R., Owen, A.B. 2012. Multiple hypothesis testing adjusted for latent variables, with an
 application to the AGEMAP gene expression data. The Annals of Applied Statistics, 6, no. 4, 1664-1688.
}
\author{
David Causeur, IRMAR, UMR 6625 CNRS, Agrocampus Ouest, Rennes, France.
}
\seealso{
\code{\link{erpavetest}}, \code{\link{erptest}}, \code{\link{gbtest}}, \code{\link[stats]{p.adjust}}, \code{\link[fdrtool]{pval.estimate.eta0}}
}
\examples{

\dontrun{

data(impulsivity)

# Paired t-tests for the comparison of the ERP curves in the two conditions, 
# within experimental group High, at channel CPZ

erpdta.highCPZ = impulsivity[(impulsivity$Group=="High")&(impulsivity$Channel=="CPZ"),5:505] 
   # ERP curves for subjects in group 'High', at channel CPZ
covariates.highCPZ = impulsivity[(impulsivity$Group=="High")&(impulsivity$Channel=="CPZ"),1:4]
covariates.highCPZ = droplevels(covariates.highCPZ)
   # Experimental covariates for subjects in group 'High', at channel CPZ

design = model.matrix(~C(Subject,sum)+Condition,data=covariates.highCPZ)
   # Design matrix to compare ERP curves in the two conditions
design0 = model.matrix(~C(Subject,sum),data=covariates.highCPZ)
   # Design matrix for the null model (no condition effect)

tests = erpfatest(erpdta.highCPZ,design,design0,nbf=NULL,wantplot=TRUE,significance="none")
   # with nbf=NULL and significance="none", just to choose a number of factors
Ftest = erpFtest(erpdta.highCPZ,design,design0,nbf=6)
   # with nbf=6 (approximate conservative recommendation based on the variance inflation plot)
   # Signal detection: test for an eventual condition effect.
Ftest$pval

tests = erpfatest(erpdta.highCPZ,design,design0,nbf=6)
   # Signal identification: which are the significant intervals?

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
 
}

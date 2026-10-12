\name{ERP-package}
\alias{ERP-package}
\alias{ERP}
\docType{package}
\title{
\packageTitle{ERP}: Significance Analysis of Event-Related Potentials Data
}
\description{
\packageDescription{ERP}: This package provides testing procedures designed for Event-Related Potentials (ERP) data in a linear model
framework. The functional F-test proposed in Causeur, Sheu, Perthame, Rufini (2018) for analysis of variance issues in
ERP designs is implemented for signal detection (tests for mean difference among groups of curves in One-way ANOVA designs for example). 
Once an experimental effect is declared significant, identification of significant intervals is achieved by the 
multiple testing procedures reviewed and compared in Sheu, Perthame, Lee and Causeur (2016, <DOI: 10.1214/15-AOAS888>).
Some of the methods gathered in the package are the classical FDR- and FWER-controlling procedures, also available
using function p.adjust. The package also implements the Guthrie-Buchwald procedure (Guthrie and Buchwald, 1991, <DOI: 10.1111/j.1469-8986.1991.tb00417.x>),
which accounts for the auto-correlation among t-tests to control erroneous detection of short intervals.
The Adaptive Factor-Adjustment method is an extension of the method described in Causeur, Chu, Hsieh and Sheu (2012, <DOI: 10.3758/s13428-012-0230-0>).
It assumes a factor model for the correlation among tests and combines adaptively the estimation of the signal and
the updating of the dependence modelling (see Sheu et al., 2016, <DOI: 10.1214/15-AOAS888> for further details).
}
\details{
The DESCRIPTION file:
\packageDESCRIPTION{ERP}
\packageIndices{ERP}
This package is designed to help making decision about the position on the brain and the time interval after the onset where an effect is significant.
The function erpplot can be used to display the ERP curves in an exploratory purpose or to plot effect curves (difference curve among experimental groups for example),
with or without smoothing options and confidendence intervals. 
All the other functions implement testing procedures of ERP data in a linear model framework
(F-tests for the comparison of two nested models). The function gbtest implements the Guthrie-Buchwald procedure
(see Guthrie and Buchwald, 1991). The function erpavetest first partitions the
entire interval of ERP observations into a predetermined number of equally-spaced intervals before performing siginificance
testing using the averaged ERPs. The function erptest can be used for the classical FDR- and FWER-controlling multiple
testing of ERP data: especially the Benjamini-Hochberg (see Benjamini and Hochberg, 1995) and Benjamini-Yekutieli
(see Benjamini and Yekutieli, 2001) procedures, with the possible extension proposed by Storey and Tibshirani (2003)
including a non-parametric estimation of the proportion of true nulls. Finally, erpFtest is designed for functional F-tests in ERP
designs using the generalized likelihood-ratio test proposed by Causeur et al. (2018) and erpfatest for the identification of
significant intervals with FDR control using the adaptive factor-adjustment (AFA) multiple testing procedure proposed by Sheu et al. (2016).
}
\author{
\packageAuthor{ERP}
David Causeur, (Agrocampus, Rennes, France), Ching-Fan Sheu (National Cheng Kung University, Tainan, Taiwan),
Mei-Chen Chu (National Cheng Kung University, Tainan, Taiwan), Flavia Rufini (Agrocampus, Rennes, France)
Maintainer: \packageMaintainer{ERP}
Maintainer: David Causeur, http://math.agrocampus-ouest.fr/infoglueDeliverLive/membres/david.causeur, mailto: david.causeur@agrocampus-ouest.fr
}
\references{
Benjamini, Y., and Hochberg, Y. (1995). Controlling the false discovery rate: a practical and powerful approach to multiple testing. Journal of the Royal Statistical Society Series B, 289-300.

Benjamini, Y., and Yekutieli, D. (2001). The control of the false discovery rate in multiple testing under dependency. Annals of Statistics 29, 1165-1188.

Causeur, D., Chu, M.-C., Hsieh, S. and Sheu, C.-F. (2012). A factor-adjusted multiple testing procedure for ERP data analysis. Behavior Research Methods, 44, 635-643.

Causeur, D., Sheu, C.-F., Perthame, E. and Rufini, F. (2018). A functional generalized F-test for signal detection with applications to event-related potentials significance analysis. Submitted.

Friguet, C., Kloareg, M. and Causeur, D. (2009). A factor model approach to multiple testing under dependence. Journal of the American Statistical Association, 104, 1406-1415.

Guthrie, D. and Buchwald, J.S. (1991). Significance testing of difference potentials. Psychophysiology, 28, 240-244.

Holm, S. (1979). A simple sequentially rejective multiple test procedure. Scandinavian Journal of Statistics, 6, 65-70.

Hommel, G. (1988). A stagewise rejective multiple test procedure based on a modified Bonferroni test. Biometrika, 75, 383-386.

Hochberg, Y. (1988). A sharper Bonferroni procedure for multiple tests of significance. Biometrika, 75, 800-803.

Shaffer, J. P. (1995). Multiple hypothesis testing. Annual Review of Psychology, 46, 561-576.

Sarkar, S. (1998). Some probability inequalities for ordered MTP2 random variables: a proof of Simes conjecture. Annals of Statistics, 26, 494-504.

Sarkar, S., and Chang, C. K. (1997). Simes' method for multiple hypothesis testing with positively dependent test statistics. Journal of the American Statistical Association, 92, 1601-1608.

Shen, I. H., Lee, D. S. and Chen, C. L. (2014). The role of trait impulsivity in response inhibition: event-related potentials in a stop-signal task. International Journal
of Psychophysiology 91.

Sheu, C-.F., Perthame, E., Lee, Y-.S., Causeur, D. (2016). Accounting for time dependence in large-scale multiple testing of event-related potential data. Annals of Applied Statistics. 10(1), 219-245.

Wright, S. P. (1992). Adjusted P-values for simultaneous inference. Biometrics, 48, 1005-1013.
}

\examples{

\dontrun{

data(impulsivity)

# Paired t-tests for the comparison of the ERP curves in the two conditions, 
# within experimental group High, at channel CPZ 

erpdta.highCPZ = impulsivity[(impulsivity$Group=="High")&(impulsivity$Channel=="CPZ"),5:505] 
   # ERP curves for subjects in group 'High'
covariates.highCPZ = impulsivity[(impulsivity$Group=="High")&(impulsivity$Channel=="CPZ"),1:4]
covariates.highCPZ = droplevels(covariates.highCPZ)
   # Experimental covariates for subjects in group High

design = model.matrix(~C(Subject,sum)+Condition,data=covariates.highCPZ)
   # Design matrix to compare ERP curves in the two conditions
design0 = model.matrix(~C(Subject,sum),data=covariates.highCPZ)
   # Design matrix for the null model (no condition effect)

tests = erpfatest(erpdta.highCPZ,design,design0,nbf=NULL,wantplot=TRUE,significance="none")
   # with significance="none", just to choose a number of factors
Ftest = erpFtest(erpdta.highCPZ,design,design0,nbf=6)
Ftest$pval
tests = erpfatest(erpdta.highCPZ,design,design0,nbf=6)
   # with nbf=6 (approximate conservative recommendation based on the variance inflation plot)

time_pt = seq(0,1000,2)     # Sequence of time points (1 time point every 2ms in [0,1000])
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
mtext(paste("12 subjects - Group High - ",nbs," B-splines",sep=""),cex=1.25)
}

}

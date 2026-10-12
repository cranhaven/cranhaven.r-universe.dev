\name{twosided.PValues.tobins}
\alias{twosided.PValues.tobins}
\title{
Binning of two sided P-Values and estimation of the probabilities in each bin for the null and non-null states.
}
\description{
For each study, the function discretizes two sided P-values into bins and estimates the probabilities in each bin for the null and non-null states.

The function can plot diagnostic plots (disabled by default) for model fit. These should be monitored for misfit of model to data, before using function output in \code{repfdr}. See description of diagnostic plots below.

}
\usage{
twosided.PValues.tobins(pval.mat, n.bins = 120, type = 0, df = 7,
                                   central.prop = 0.5,
                                   pi0=NULL,plot.diagnostics = FALSE,
                                   trim.z=FALSE,trim.z.upper = 8,
                                   trim.z.lower = -8, force.bin.number = FALSE,
                                   pi.plugin.lambda = 0.05)
}
\arguments{
  \item{pval.mat}{
Matrix of two sided P-Values of the features (in rows) in each study (columns).
}
  \item{n.bins}{
Number of bins in the discretization of the z-score axis (the number of bins is \code{n.bins - 1}). If the number of z-scores per study is small, we set \code{n.bins} to a number lower than the default of 120 (about equals to the square root of the number of z-scores). To override the bin number cap (and create a discretization of the data that is sparse), use the \code{force.bin.number = TRUE} argument.
}
  \item{type}{
Type of fitting used for f; 0 is a natural spline, 1 is a polynomial, in either case with degrees of freedom \code{df} (so total degrees of freedom including the intercept is \code{df+1}).
}
  \item{df}{
Degrees of freedom for fitting the estimated density f(z).
}
  \item{central.prop}{
Central proportion of the z-scores used like the area of zero-assumption to estimate pi0.
}

\item{pi0}{
Sets argument for estimation of proportion of null hypotheses. Default value is NULL (automatic estimation of pi0) for every study. Second option is to supply vector of values between 0 and 1 (with length of the number of studies/ columns of \code{zmat}. These values will be used for pi0.
}


\item{plot.diagnostics}{
  If set to \code{TRUE}, will show disgnostics plots for density estimation for each study. First plot is a histogram of counts for each bin (Displayed as white bars), along with fitted density in green. Pink bars represent the observed number of counts in each bins, minus the expected number of null hypotheses by the model (truncated at zero). Red and Orange dashed lines represent the estimated densities for non null distributions fitted by the spline. A blue dashed line represents the density component of Z scores for null SNPS, N(0,1).

A second plot is the Normal Q-Q plot of Zscores, converted using \code{qnorm} to the normal scale. A valid graph should coincide with a the linear fit displayed. A misfit with the linear plot could indicate either a null distribution which is not standard normal (a problem), or an extreme number of non null P-Values (Signal is not sparse, output is still valid). A black dashed line markes the expected fit for the standard normal distribution (with a single black dot for the (0,0) point). If the linear fit for the Q-Q plot (red line) does not match the dashed black line, the null distribution of the data is not standard normal.

Misfit in these two plots should be investigated by the user, before using output in \code{repfdr}

Default value is \code{False}.
}

\item{trim.z}{ If set to \code{TRUE}, Z scores above \code{trim.z.upper} or below \code{trim.z.lower} will be trimmed at their respective limits. Default value if \code{FALSE}
}

\item{trim.z.upper}{
Upper bound for trimming Z scores. Default value is 8
}

\item{trim.z.lower}{
Lower bound for trimming Z scores. Default value is -8
}

\item{force.bin.number}{Set to \code{T} to be able to create a discretization with \code{n.bins>sqrt(nrow(zmat))}.
}

\item{pi.plugin.lambda}{
  The function makes use of the plugin estimator for the estimation of the proportion of null hypotheses. The plugin  estimator is  \code{(sum(Pvalues > pi.plugin.lambda) + 1)/(m * (1-pi.plugin.lambda))} where \code{m} is the number of P-values. Default value is 0.05. This should be set to the type 1 error used for hypothesis testing.
}
        
}
\details{
This utility function outputs the first two arguments to be input in the main function \code{\link{repfdr}}.
}
\value{
A list with:
  \item{pdf.binned.z}{
A 3-dimensional array which contains for each study (first dimension), the probabilities of a z-score to fall in the bin (second dimension), under each hypothesis status (third dimension). The third dimension can be of size 2 or 3, depending on the number of association states: if the association can be either null or only in one direction, the dimension is 2; if the association can be either null, or positive, or negative, the dimension is 3.  
}
  \item{binned.z.mat}{
A matrix of the bin numbers for each the z-scores (rows) in each study (columns).
}

\item{breaks.matrix}{
A matrix with \code{n.bins + 1} rows and {ncol(zmat)} columns, representing for each study the discretization chosed. Values are the between bin breaks. First and last values are the edges of the outmost bins.
}

\item{df}{
Number of degrees of freedom, used for spline fitting of density.
}

\item{proportions}{
Matrix with \code{n.association.status} rows, and \code{ncol(zmat)} columns, giving the estimated proportion of each component, for each study.
}

\item{PlotWarnings}{
Vector of size \code{ncol{zmat}}, keeping the warnings given for each study (available here, in the plots for each study and printed to console). With no warnings given for study, value is \code{NA}
}

}
\seealso{
\code{\link{repfdr}}
}
\examples{

# we generate a dataset with p=10000 pvalues for two studies,
# p1=300 of which are non null:
set.seed(1)
p = 10000
p1 = 300
z1 = (rnorm(p))
z2 = (rnorm(p))
temp = rnorm(p1, 3.5,0.5)
z1[1:p1] = temp + rnorm(p1,0,0.2)
z2[1:p1] = temp + rnorm(p1,0,0.2)

zmat.example = cbind(z1,z2)
pmat.example = 1-(pnorm(abs(zmat.example)) - pnorm(-1*abs(zmat.example)))

twosided.pval.res = twosided.PValues.tobins(pmat.example,
                                            plot.diagnostics = TRUE)

twosided.pval.res$proportions

}
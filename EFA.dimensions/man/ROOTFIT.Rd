\name{ROOTFIT}
\alias{ROOTFIT}
\title{Factor fit coefficients}
\description{A variety of fit coefficients for the possible N-factor solutions 
	in exploratory factor analysis}
\usage{ROOTFIT(data, corkind='pearson', Ncases=NULL, extraction='PAF', verbose, factormodel)}
\arguments{
  \item{data}{
  \code{}An all-numeric dataframe where the rows are cases & the columns are the 
  variables, or a correlation matrix with ones on the diagonal.The function 
  internally determines whether the data are a correlation matrix.}

  \item{corkind}{
  \code{}The kind of correlation matrix to be used if data is not a correlation 
  matrix. The options are 'pearson', 'kendall', 'spearman', 'gamma', and 'polychoric'. Required 
  only if the entered data is not a correlation matrix.}

  \item{Ncases}{
  \code{}The number of cases upon which a correlation matrix is based. Required only 
  if data is a correlation matrix.}

  \item{extraction}{ 
  \code{}The factor extraction method. The options are:
  	    'PAF' for principal axis / common factor analysis;
  	    'PCA' for principal components analysis.
  	    'ML' for maximum likelihood estimation.}
                  
  \item{verbose}{
  \code{}Should detailed results be displayed in console? TRUE (default) or FALSE}
  
  \item{factormodel}{ 
  \code{}(Deprecated.) Use 'extraction' instead.}
}
\details{
\strong{ Eigenvalue}	
	
An eigenvalue is the variance of the factor. More specifically, an eigenvalue is the
the variance of the linear combination of the variables for a factor. There are 
as many eigenvalues for a correlation or covariance matrix as there are variables 
in the matrix. The sum of the eigenvalues is equal to the number of variables. 
An eigenvalue of one means that a factor explains as much variance as one variable.


\strong{RMSR -- Root Mean Square Residual (absolute fit)}

RMSR (or perhaps more commonly, RMR) is an index of the overall badness-of-fit.
It is the square root of the mean of the squared residuals (the residuals being
the simple differences between original correlations and the correlations
implied by the N-factor model). RMSR is 0 when there is perfect model fit. A
value less than .08 is generally considered a good fit. A standardized version
of the RMSR is often recommended over the RMSR in structural equation modeling
analyses. This is because the values in covariance matrices are scale-dependent.
However, the RMSR coefficient that is provided in this package is based on
correlation coefficients (not covariances) and therefore does not have this
problem.


\strong{GFI (absolute fit)}
 
The GFI (McDonald, 1999) is an index of how closely a correlation matrix is reproduced by 
the factor solution. It is equal to 1.0 - mean-squared residual / mean-squared 
correlation, ignoring the diagonals.


\strong{CAF (common part accounted for)}

Lorenzo-Seva, Timmerman, & Kiers (2011): "We now propose an alternative 
goodness-of-fit index that can be used with any extraction 
method. This index expresses the extent to which the common variance in the data is 
captured in the common factor model. The index is denoted as CAF (common part accounted for)." 

"A measure that expresses the amount of common variance in a matrix is found in the 
KMO (Kaiser, Meyer, Olkin) index (see Kaiser, 1970; Kaiser & Rice, 1974). The KMO 
index is commonly used to assess whether a particular correlation matrix R is suitable 
for common factor analysis (i.e., if there is enough common variance to justify a 
factor analysis)."

"Now, we propose to express the common part accounted for by a common factor model 
with q common factors as 1 minus the KMO index of the estimated residual matrix."

"The values of CAF are in the range [0, 1] and if they are close to zero it means that 
a substantial amount of common variance is still present in the residual matrix after 
the q factors have been extractioned (implying that more factors should be extractioned). 
Values of CAF close to one mean that the residual matrix is free of common variance 
after the q factors have been extractioned (i.e., no more factors should be extractioned)." 


\strong{RMSEA - Root Mean Square Error of Approximation (absolute fit)}

Schermelleh-Engel (2003): "The Root Mean Square Error of Approximation (RMSEA;
Steiger, 1990) is a measure of approximate fit in the population and is
therefore concerned with the discrepancy due to approximation. Steiger (1990)
as well as Browne and Cudeck (1993) define a "close fit" as a RMSEA value <= .05. 
According to Browne and Cudeck (1993), RMSEA values <= .05
can be considered as a good fit, values between .05 and .08 as an adequate fit,
and values between .08 and .10 as a mediocre fit, whereas values > .10 are not
acceptable. Although there is general agreement that the value of RMSEA for a
good model should be less than .05, Hu and Bentler (1999) suggested an RMSEA of
less than .06 as a cutoff criterion."	

Kenny (2020): "The measure is positively biased (i.e., tends to be too large)
and the amount of the bias depends on smallness of sample size and df, primarily
the latter. The RMSEA is currently the most popular measure of model fit. 
MacCallum, Browne and Sugawara (1996) have used 0.01, 0.05, and 0.08 to indicate
excellent, good, and mediocre fit respectively. However, others have suggested
0.10 as the cutoff for poor fitting models. These are definitions for the
population. That is, a given model may have a population value of 0.05 (which
would not be known), but in the sample it might be greater than 0.10. 
There is greater sampling error for small df and low N models, especially for the 
former.  Thus, models with small df and low N can have artificially large values 
of the RMSEA.  For instance, a chi square of 2.098 (a value not statistically 
significant), with a df of 1 and N of 70 yields an RMSEA of 0.126.  For this 
reason, Kenny, Kaniskan, and McCoach (2014) argue to not even compute the 
RMSEA for low df models."	

Hooper (2008): "In recent years it has become regarded as "one of the most
informative fit indices" (Diamantopoulos and Siguaw, 2000: 85) due to its
sensitivity to the number of estimated parameters in the model. In other words,
the RMSEA favours parsimony in that it will choose the model with the lesser
number of parameters."

\strong{TLI -- Tucker Lewis Index (incremental fit)}

The Tucker-Lewis index, TLI, is also sometimes called the non-normed fit index,
NNFI, or the Bentler-Bonett non-normed fit index, or RHO2. The TLI penalizes for
model complexity.	

Schermelleh-Engel (2003): "The (TLI or) NNFI ranges in general from zero to one,
but as this index is not normed, values can sometimes leave this range, with
higher (TLI or) NNFI values indimessageing better fit. A rule of thumb for this
index is that .97 is indimessageive of good fit relative to the independence model,
whereas values greater than .95 may be interpreted as an acceptable fit. An
advantage of the (TLI or) NNFI is that it is one of the fit indices less
affected by sample size (Bentler, 1990; Bollen, 1990; Hu & Bentler, 1995,
1998)."
	
Kenny (2020): "The TLI (and the CFI) depends on the average size of the
correlations in the data. If the average correlation between variables is not
high, then the TLI will not be very high."

\strong{CFI - Comparative Fit Index (incremental fit)}
	
Schermelleh-Engel (2003): "The CFI ranges from zero to one with higher values
indimessageing better fit. A rule of thumb for this index is that .97 is indicative
of good fit relative to the independence model, while values greater than .95
may be interpreted as an acceptable fit. Again a value of .97 seems to be more
reasonable as an indimessageion of a good model fit than the often stated cutoff
value of .95. Comparable to the NNFI, the CFI is one of the fit indices less
affected by sample size."	

Hooper (2008): "A cut-off criterion of CFI >= 0.90 was initially advanced
however, recent studies have shown that a value greater than 0.90 is needed in
order to ensure that misspecified models are not accepted (Hu and Bentler,
1999). From this, a value of CFI >= 0.95 is presently recognised as indicative of
good fit (Hu and Bentler, 1999). Today this index is included in all SEM
programs and is one of the most popularly reported fit indices due to being one
of the measures least effected by sample size (Fan et al, 1999)."
	
Kenny (2020): "Because the TLI and CFI are highly correlated only one of the two
should be reported. The CFI is reported more often than the TLI, but I think the
CFI,s penalty for complexity of just 1 is too low and so I prefer the TLI even
though the CFI is reported much more frequently than the TLI."

\strong{MFI -- (absolute fit)}

An absolute fit index proposed by MacDonald and Marsh (1990) that does not depend on 
a comparison with another model.

\strong{AIC -- Akaike Information Criterion (degree of parsimony index)}
	
Kenny (2020): "The AIC is a comparative measure of fit and so it is meaningful 
only when two different models are estimated. Lower values indicate a better fit 
and so the model with the lowest AIC is the best fitting model.  There are somewhat 
different formulas given for the AIC in the literature, but those differences are 
not really meaningful as it is the difference in AIC that really matters. The 
AIC makes the researcher pay a penalty of two for every parameter that is estimated. 
One advantage of the AIC, BIC, and SABIC measures is that they can be computed for 
models with zero degrees of freedom, i.e., saturated or just-identified models."

\strong{CAIC -- Consistent Akaike Information Criterion (degree of parsimony index)}

A version of AIC that adjusts for sample size. Lower values indicate a better fit.

\strong{BIC -- Bayesian Information Criterion (degree of parsimony index)}

Lower values indicate a better fit.

Kenny (2020): "Whereas the AIC has a penalty of 2 for every parameter estimated, 
the BIC increases the penalty as sample size increases. The BIC places a high value 
on parsimony (perhaps too high)."

\strong{SABIC -- Sample-Size Adjusted BIC (degree of parsimony index)}

Kenny (2020): "Like the BIC, the sample-size adjusted BIC or SABIC places a penalty 
for adding parameters based on sample size, but not as high a penalty as the BIC.  
Several recent simulation studies (Enders & Tofighi, 2008; Tofighi, & Enders, 2007) 
have suggested that the SABIC is a useful tool in comparing models.
}
\value{A list with eigenvalues & fit coefficients.}
\references{
	  {Hooper, D., Coughlan, J., & Mullen, M. (2008). Structural Equation Modelling:
       Guidelines for Determining Model Fit. \emph{Electronic Journal of Business Research
       Methods, 6(1),} 53-60.} 
\cr\cr{Kenny, D. A. (2020). \emph{Measuring model fit.} http://davidaKenny.net/cm/fit.htm}
\cr\cr{McDonald, R. P. (1999). \emph{Test theory: A unified treatment.} Mahwah, NJ: 
	   Lawrence Erlbaum Associates, Publishers.}
\cr\cr{Lorenzo-Seva, U., Timmerman, M. E., & Kiers, H. A. (2011). The Hull method for 
       selecting the number of common factors. \emph{Multivariate Behavioral Research, 46,} 340-364.}
\cr\cr{Schermelleh-Engel, K., & Moosbrugger, H. (2003). Evaluating the fit of
       structural equation models: Tests of significance and descriptive
       goodness-of-fit measures. \emph{Methods of Psychological Research Online, Vol.8(2),}
       pp. 23-74.}
\cr\cr{Tabachnick, B. G., & Fidell, L. S. (2019). \emph{Using multivariate statistics}
	   (pp. 560-564). New York, NY: Pearson.}
}
\author{Brian P. O'Connor }
\examples{
\donttest{
# the Harman (1967) correlation matrix
ROOTFIT(data_Harman, Ncases = 305, extraction='ml')
ROOTFIT(data_Harman, Ncases = 305, extraction='paf')
ROOTFIT(data_Harman, Ncases = 305, extraction='pca')

# RSE data
ROOTFIT(data_RSE, corkind='pearson', extraction='ml')
ROOTFIT(data_RSE, corkind='pearson', extraction='paf')
ROOTFIT(data_RSE, corkind='pearson', extraction='pca')

# NEO-PI-R scales
ROOTFIT(data_NEOPIR, corkind='pearson', extraction='ml')
ROOTFIT(data_NEOPIR, corkind='pearson', extraction='paf')
ROOTFIT(data_NEOPIR, corkind='pearson', extraction='pca')
}
}

\name{ControlResponseBias}
\alias{ControlResponseBias}
\title{
Performs Factor Analysis controlling response bias
}
\description{
Performs Factor Analysis (FA) implementing the method developed by Ferrando, Lorenzo-Seva & Chico (2009). It allows the user to control the effects of two response bias: Social Desirability (SD) and Acquiescence (AC) by extracting the variance due to these factors before extracting the content ones.
}
\usage{
ControlResponseBias(x, content_factors, SD_items, unbalanced_items, contSD = FALSE,
    contAC = FALSE, corr = "Pearson", rotat = "promin", target, factor_scores = FALSE,
    PA = FALSE, path = FALSE, display = TRUE)

}
\arguments{
  \item{x}{
Raw sample scores or a covariance/correlation matrix directly. The SD items (if applicable) have to be included in the same dataset as the content ones.
}
  \item{content_factors}{
The number of content factors to be retained. At least, each factor has to be defined by 3 items, so the maximum number of content factors will be the number of items / 3.
}
  \item{SD_items}{
When controlling Social Desirability, a vector containing the Social Desirability markers has to be provided. At least 4 SD markers are necessary to perform the SD control procedure. Providing a valid input argument turns automatically contSD to TRUE.
}
  \item{unbalanced_items}{
When controlling AC, if the content items are not fully balanced between positive worded items and negative worded items, the user has to determine which items will not be used in the calculation of the centroid. For more information, check the details section. Providing a valid input argument turns automatically contAC to TRUE
}
  \item{contSD}{
Logical variable determining if the method for controlling Social Desirability will be used. If SD_items are provided, it automatically switch to TRUE.
}
  \item{contAC}{
Logical variable determining if the method for controlling Acquiescence will be used. If unbalanced_items are provided, it automatically switch to TRUE. However, it is possible to control Acquiescence without providing unbalanced_items when the scale is fully balanced.
}
  \item{corr}{
Determines the type of matrices to be used in the factor analysis. "Pearson": Computes Pearson correlation matrices (linear model); "Polychoric": Computes Polychoric/Tetrachoric correlation matrices (graded model). If the matrix is not positive definite, the smoothing procedure proposed by Bentler and Yuan (2011) is used.
}
 \item{rotat}{
Determines if the factor loading matrix will be rotated, and if it dues, which method will be used. By default, "promin" rotation wil lbe used. The rotation methods available include all the available options for the rotation of the loading matrix included in the following packages: \code{stats} (R Core Team, 2019), \code{GPArotation} (Bernaards, Jennrich, 2005) and \code{PCovR} (Vervloet, Kiers, Van den Noortgate & Ceulemans, 2015). If the user is not interested in rotating the factors, provide the "none" option. The argument has to be provided with the same syntactic structure as in the original package.
}
  \item{target}{
The target for the procrustes factor analysis rotation (optional).
}
  \item{factor_scores}{
Logical variable determining if the factor scores will be computed. If TRUE, Fully-Informative Prior EAP scores (Ferrando & Lorenzo-Seva, 2016) will be computed for continuous and graded models. When using Polychoric matrix, the precision of the scores is computed, and stored in $precision_matrix in a three dimensional matrix.
}
  \item{PA}{
Logical variable determining if the Optimal Implementation of Parallel Analysis (Timmerman & Lorenzo-Seva, 2011) will be computed for estimating the number of factors to be retained.
}
  \item{path}{
Logical variable determining if path diagram will be plotted (FALSE by default). It uses the function \code{\link[semPlot]{semPaths}} internally, and for preventing displaying issues, it is limited to 40 total items and 5 content factors. Since it was designed just for providing a visual representation of the factor loadings, the residuals and intercepts have been omited, and the loadings lower than .3 in absolute value are not being plotted (in AC the threshold is .2).

}
  \item{display}{
Determines if the output will be displayed in the console, TRUE by default. If it is TRUE, the output is printed in console and if it is FALSE, the output is returned silently to the output variable, without printing anything in console. Alternativelly, a character array can be provided for printing specific sections of the output, the options being the following: "Summary", "Descriptives", "Adequacy", "PA" (Parallel Analysis),"GOF" (Goodness of Fit indices) and "Loadings".
}
}
\details{
The function is based on the method proposed by Ferrando, Lorenzo-Seva & Chico (2009), and it is adapted from the MATLAB code of Psychological Test Toolbox program (Navarro-Gonzalez, Vigil-Colet, Ferrando & Lorenzo-Seva, 2019).

The FA procedure is structurated in 3 steps:

1. Extract the Social Desirability factor using the SD markers provided by the user. This items should be pure measures of SD and are assumed to be uncorrelated with the content.

2. Extract the Acquiescence factor using a balanced set of content items, containing the same number of positively keyed and negatively keyed items. If the scale is only partially balanced, some items of the predominant type (positively keyed or negatibely keyed) have to be excluded from the balanced set. The recommended exclusion criteria is the following: exclude the items with lower loadings on AC, using the "unbalanced_itemd" argument. If there is no a priori reasoning, we recommend selecting some items at random to be used as unbalanced in a first exploratory analisis and then, look at the AC loadings and select the ones with lower loadings for a definitive second analisis.

3. Extract the content factors using the residual matrix obtained after substracting the variance due to Social Desirability and Acquiescence.

It is not mandatory to control both of the response biases. The functions allows the user to select which bias should be controlled between SD, AC or both.

The function implements all the available options for the rotation of the loading matrix included in the following packages:

- \code{\link[stats]{stats-package}} (R Core Team, 2019).

- \code{\link[GPArotation]{GPArotation-package}} (Bernaards, Jennrich, 2005).

- \code{\link[PCovR]{PCovR-package}} (Vervloet, Kiers, Van den Noortgate & Ceulemans, 2015).

The selection of rotation methods include some of the most knowns ones, like:

-\code{\link[PCovR]{promin}} (Lorenzo-Seva, 1999), available in the PCovR package.

-\code{\link[stats]{varimax}} (Kaiser, 1958), available in the stats package.

- \code{\link[GPArotation:rotations]{Oblimin}} (Jackson, 2005), available in the GPArotation package.

If the user is not interested in any of the rotation methods available or prefer rotating the loading matrix externally, please consider that the response bias factors (SD and/or AC) should not be rotated.

The robust goodness fit of indices are computed using \code{\link{lavaan}} (Rosseel, 2012).
}
\value{
\item{loadings}{Obtained loading matrix.}
\item{Phi}{Correlation between factors, only available when an oblique rotation has been selected.}
\item{Factor_scores}{Obtained factor scores for each participant in the bias and content factors. This variable is returned only if the argument factor_scores has set to TRUE.}
\item{precision_matrix}{Precision values for each factor score estimate, including the 90 \% CI, the Posterior SD (PSD) and the reliability of the estimation. Only available for graded model.}
\item{comunalities}{Obtained communalities for each item.}
\item{ECV}{Explained common variance for each factor.}
\item{reduced}{Reduced matrix obtained once extracting all the factors.}
\item{produced}{Produced final matrix.}
\item{RMSEA}{Root Mean Square Error of Approximation.}
\item{Chi}{Robust Mean-Scaled Chi Square of the model.}
\item{TLI}{Non-Normed Fit Index (NNFI; Tucker & Lewis).}
\item{CFI}{Comparative Fit Index.}
\item{GFI}{Goodness of Fit Index.}
\item{RMSR}{Root Mean Square Residuals.}
\item{kelley}{Kelley's criterion.}
\item{PA_Real_Data}{A vector containing the percentage of explained variance by the real data for each factor (If Parallel Analysis is required by using the argument PA = TRUE).}
\item{PA_Mean_Random}{A vector containing the percentage of explained variance by the mean of random data for each factor (If Parallel Analysis is required by using the argument PA = TRUE).}
\item{PA_Percentile_Random}{A vector containing the percentage of explained variance by the percentile of distribution of random data for each factor (If Parallel Analysis is required by using the argument PA = TRUE).}
\item{N_factors_mean}{The number of factors to be retained suggested comparing the real data with the mean of the random data (If Parallel Analysis is required by using the argument PA = TRUE).}
\item{N_factors_percentiles}{The number of factors to be retained suggested comparing the real data with the percentile of distribution of the random data (If Parallel Analysis is required by using the argument PA = TRUE).}

}
\references{

Bentler, P. M., & Yuan, K. H. (2011). Positive definiteness via off-diagonal scaling of a symmetric indefinite matrix. Psychometrika, 76(1), 119-123. \doi{10.1007/s11336-010-9191-3}

Bernaards, C. A. and Jennrich, R. I. (2005) Gradient Projection Algorithms and Software for Arbitrary Rotation Criteria in Factor Analysis. Educational and Psychological Measurement, 65, 676-696. \doi{10.1177/0013164404272507}

Ferrando, P. J., & Lorenzo-Seva, U. (2016). A note on improving EAP trait estimation in oblique factor-analytic and item response theory models. Psicologica, 37, 235-247.

Ferrando, P. J., Lorenzo-Seva, U., & Chico, E. (2009). A General factor-Analytic procedure for assessing response bias in questionnaire measures. Structural Equation Modeling, 16, 364-381. \doi{10.1080/10705510902751374}

Jackson, J. E. (2005). Oblimin Rotation. Encyclopedia of Biostatistics. 6. \doi{10.1002/0470011815.b2a13060}

Kaiser, H.F. (1958). The varimax criterion for analytic rotation in factor analysis. Psychometrika, 23, 187-200. \doi{10.1007/BF02289233}

Lorenzo-Seva, U. (1999). Promin: A Method for Oblique Factor Rotation. Multivariate Behavioral Research, 34(3), 347-365. \doi{10.1207/S15327906MBR3403_3}

Navarro-Gonzalez, D., Vigil-Colet, A., Ferrando, P.J., & Lorenzo-Seva, U. (2019). Psychological Test Toolbox: A New Tool to Compute Factor Analysis Controlling Response Bias. Journal of Statistical Software, 91(6). \doi{10.18637/jss.v091.i06}

R Core Team (2019). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. \url{https://www.R-project.org}

Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2).\doi{10.18637/jss.v048.i02}

Timmerman, M. E., & Lorenzo-Seva, U. (2011). Dimensionality Assessment of Ordered Polytomous Items with Parallel Analysis. Psychological Methods, 16, 209-220. \doi{10.1037/a0023353}

Vervloet, M., Kiers, H.A.L., Van den Noortgate, W., & Ceulemans, E. (2013). PCovR: An R Package for Principal Covariates Regression. Journal of Statistical Software, 65(8). \doi{10.18637/jss.v065.i08}

}
\author{
David Navarro-Gonzalez

Andreu Vigil-Colet

Pere Joan Ferrando

Urbano Lorenzo-Seva

Jorge Tendeiro
}

\examples{
## This is an example using the vampyr_example dataset, which contains 4 SD markers (the first
## 4 items, defined using the "SD_items" argument) and 6 Physical aggression items extracted
## from the IDAQ questionnaire. Since the 6 content items are balanced (3 positively worded and
## 3 negatively worded), there is no need to use the "unbalaced_items" argument.

ControlResponseBias(vampyr_example, content_factors = 1, SD_items = c(1,2,3,4),
    corr = "Pearson", contAC = TRUE, rotat = "none")

## For speeding purposes, some options have been disabled. Pearson correlation matrix is
## being used, but Polychoric matrix may be advisable. Parallel Analysis ('PA' argument)
## is disabled, as well as the factor scores and the path diagram.
}

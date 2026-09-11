\docType{package}
\encoding{UTF-8}
\name{HRTnomaly-package}
\alias{HRTnomaly-package}
\alias{HRTnomaly}
\title{HRTnomaly}

\description{
    \if{html}{\figure{logo.svg}{options: style='float:right;width:166px;height:192px;padding:2px;'}}
    Enhanced Anomaly Detection for Historical, Relational, and Tail Cellwise Outlier
}

\details{
\tabular{ll}{
Package: \tab HRTnomaly\cr
Type: \tab Package\cr
License: \tab AGPL-3\cr
}

The presence of outliers in a dataset can substantially bias the results of statistical analyses. 
To correct for outliers, micro edits are manually performed on all records. A set of constraints 
and decision rules is typically used to aid the editing process. However, straightforward 
decision rules might overlook anomalies arising from disruption of linear relationships. 
This package provides a computationally efficient method to identify historical,
tail, and relational anomalies at the data-entry level. A score statistic 
is developed for each anomaly type, using a distribution-free approach motivated 
by the Bienaymé-Chebyshev's inequality, and fuzzy logic is used to detect 
cellwise outliers resulting from different types of anomalies. Each data entry 
is individually scored and individual scores are combined into a final score to 
determine anomalous entries. The HRTnomaly package has proven to be a powerful tool for identifying
outliers that are not easily detectable using other traditional methods. 

For a complete list of exported functions, use \code{library(help = "HRTnomaly")}.

\emph{This research was supported by the U.S. Department of Agriculture,
National Agriculture Statistics Service. The findings and conclusions in this
publication are those of the authors and should not be construed to represent
any official USDA or U.S. Government determination or policy.}
}

\examples{
# Load the package
library(HRTnomaly)
set.seed(2025L)
# Load the 'toy' data
data(toy)
# Detect cellwise outliers using robust regression
res_c <- cellwise(toy[sample.int(33), ], epochs = 10L)
# Detect cellwise outliers using Bayesian testing
res_g <- bayesHRT(toy[sample.int(33), ], prior = 0.5)

# Detect record level outliers using Deep Isolation Forests
res_t <- dif(iris)
}

\references{
Agostinelli C, Leung A, Yohai VJ, Zamar RH (2015). Robust estimation of multivariate location and scatter in the presence of cellwise and casewise contamination. \emph{Test}, \bold{24}(3): 441--461.

Alqallaf F, Van Aelst S, Yohai VJ, Zamar RH (2009). Propagation of outliers in multivariate data. \emph{The Annals of Statistics}, 311--331.

Bienaymé IJ (1867). Considérations à l'appui de la découverte de Laplace sur la loi de probabilité dans la méthode des moindres carrés. \emph{Journal de Mathématiques Pures et Appliquées}, \bold{2}(12): 158--176.

Chepulis MA, Shevlyakov G (2020). On outlier detection with the Chebyshev type inequalities. \emph{Journal of the Belarusian State University. Mathematics and Informatics}, \bold{3}: 28--35.

Filzmoser P, Gregorich M (2020). Multivariate outlier detection in applied data analysis: Global, local, compositional and cellwise outliers. \emph{Mathematical Geosciences}, \bold{52}(8): 1049--1066.

Gupta MM, Qi J (1991). Theory of t-norms and fuzzy inference methods. \emph{Fuzzy Sets and Systems}, \bold{40}(3): 431--450.

Huber PJ, Ronchetti EM (1981). \emph{Robust statistics}. John Wiley & Sons, New York.

O'Gorman TJ (1994). The effect of cosmic rays on the soft error rate of a DRAM at ground level. \emph{IEEE Transactions on Electron Devices}, \bold{41}(4): 553--557.

Rousseeuw PJ, Van den Bossche W (2018). Detecting deviating data cells. \emph{Technometrics}, \bold{60}(2): 135--145.

Sandqvist AP (2016). Identifizierung von Ausreissern in eindimensionalen gewichteten Umfragedaten. \emph{KOF Analysen}, \bold{2016}(2): 45--56.

Sartore L, Chen L, Bejleri V (2024). Empirical Inferences Under Bayesian Framework to Identify Cellwise Outliers. \emph{Stats}, \bold{7}: 1244--1258.

Sartore L, Chen L, van Wart J, Dau A, Bejleri V (2024). Identifying Anomalous Data Entries in Repeated Surveys. \emph{Journal of Data Science}, \bold{22}(3): 436--455.

Tchebichef P (1867). Des valeurs moyennes. \emph{Journal de Mathématiques Pures et Appliquées}, \bold{2}(12): 177--184.
}

\author{
Luca Sartore, Lu Chen, Justin van Wart, Andrew Dau, and Valbona Bejleri

Maintainer: Luca Sartore \email{drwolf85@gmail.com}
}

\keyword{outliers}
\keyword{distribution}
\keyword{probability}

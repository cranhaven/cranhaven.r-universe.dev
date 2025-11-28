\name{lcorrel}
\alias{lcorrel}
\title{Local Pearson and GW Pearson Correlation}
\description{This function computes Local Pearson and Geographically Weighted Pearson Correlation Coefficients and tests for their statistical significance. Because the local significant tests are not independent, under the multiple hypotheses testing theory, a Bonferroni correction of the local coefficients takes place. The function results in tables with results for all possible pairs of the input variables.}
\usage{
lcorrel(DFrame, bw, Coords)
}
\arguments{
  \item{DFrame}{A numeric Data Frame of at least two variables}
  \item{bw}{A positive value between 0 and 1 to define the proportion of the total observations for the local sample for which each time the local coefficients are calculated for. This can be also the result of bandwidth selection algorithms of local regression techniques such as the Geographically Weighted Regression (GWR)}
  \item{Coords}{a numeric matrix or vector or data frame of two columns giving the X,Y coordinates of the observations (data points or geometric centroids)}
}
\details{
The degrees of freedom for the local t-student test is Round(bw * Number of Observations) - 2.
}
\value{
lcorrel returns a list of 7 Data Frames
  \item{LPCC}{A numeric data frame with the Local Pearson Correlation Coefficients (LPCCs) for each possible pair of the input variables in DFrame}
  \item{LPCC_t}{A numeric data frame with the t-student test statistics for all LPCCs}
  \item{LPCC_sig}{A numeric data frame with level of significance (p-value) for all LPCCs}
  \item{LPCC_sig_BF}{A numeric data frame with level of significance (p-value) for all LPCCs adjusted using the conservative Bonferroni correction to account for false positives under the multiple hypothesis testing theory}
  \item{GWPCC}{A numeric data frame with the Geographically Weighted Pearson Correlation Coefficients (GWPCCs) for each possible pair of the input variables in DFrame}
  \item{GWPCC_sig}{A numeric data frame with level of significance (p-value) for all GWPCCs}
  \item{GWPCC_sig_BF}{A numeric data frame with level of significance (p-value) for all GWPCCs adjusted using the conservative Bonferroni correction to account for false positives under the multiple hypothesis testing theory}
}

\references{
Kalogirou, S. (2012) Testing local versions of correlation coefficients, Review of Regional Research - Jahrbuch fur Regionalwissenschaft, 32(1), pp. 45-61, doi: 10.1007/s10037-011-0061-y. \url{https://link.springer.com/article/10.1007/s10037-011-0061-y}

Kalogirou, S. (2013) Testing geographically weighted multicollinearity diagnostics, GISRUK 2013, Department of Geography and Planning, School of Environmental Sciences, University of Liverpool, Liverpool, UK, 3-5 April 2013. \url{https://theses.ncl.ac.uk/jspui/handle/10443/204}

Kalogirou, S. (2015) A spatially varying relationship between the proportion of foreign citizens and income at local authorities in Greece, 10th International Congress of the Hellenic Geographical Society,  Aristotle University of Thessaloniki, Thessaloniki 22-24 October 2014. 
}
\author{Stamatis Kalogirou <stamatis.science@gmail.com>}

\examples{
data(VotesGR)
local.cor<-lcorrel(VotesGR[5:6],0.1,cbind(VotesGR$X, VotesGR$Y))
plot(local.cor$LPCC[,2],local.cor$GWPCC[,2])
}
\keyword{LPCC}
\keyword{GWPCC}
\keyword{local multi-collinearity}

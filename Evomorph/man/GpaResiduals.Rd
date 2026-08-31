\encoding{utf-8}
\name{GpaResiduals}
\alias{GpaResiduals}
\title{Generalized Procrustes residuals calculation}
\description{Calculates Procrustes residuals of landmark configurations}
\usage{GpaResiduals(lands,gpa_coords)}
\arguments{
	\item{lands}{Landmark configuration}
	\item{gpa_coords}{GPA coordinates matrix}}
\details{
Partial least squares analysis is a common tool to examine complex patterns of covariations between multiple sets of variables (Rohlf and Corti 2000; Bookstein et al. 2003). When one set of these variables consists of shape coordinates it is called singular warps analysis. Singular warp analysis is based on a singular decomposition of the cross-covariance matrix of two sets of variables measured from the same sample objects. Bookstein et al. (2013) gave this equation (1):
\deqn{ \sum = \left ( 1/N \right )X^{t}Y\ }

Where N is the sample size and X and Y are matrices for two sets of variables with N rows, corresponding to the same sample specimens, and columns according to the number of variables of each set. If each variable in both matrices is first mean-centered (by variable), then Equation (1) yields the cross-covariance matrix between variables in X and those in Y.

Most of the landmark-based morphometric studies use aligned shape coordinates (GPA) as variable for analyzing complex patterns of covariations between sets of landmarks coordinates data. However, whereas GPA mean-centers all specimen configurations – it superimposes the centroids of each specimen - GPA does not mean center each coordinate within the specimens, and therefore does not in itself provide suitable variables for use in Equation (1). 

To avoid this problem \code{GpaResiduals} mean center the aligned coordinates to get Procrustes residuals which are more appropriate for calculating covariance (McNulty 2009).

If generalized procrustes coordinates are not provided, the function will calculate them using \pkg{geomorph} package (Adams & Otárola Castillo 2013)}

\value{
	It returns a list containing:
	
	\item{consens}{Consensus shape}
	\item{cvectorized}{Consensus shape (1 dim vector)}
	\item{resid}{GPA residuals matrix}
}
\references{
Adams, D. C., & Otárola-Castillo, E. (2013). geomorph: an R package for the collection and analysis of geometric morphometric shape data. Methods in Ecology and Evolution, 4(4), 393-399.

Bookstein, F. L., Gunz, P., Mitterœcker, P., Prossinger, H., Schæfer, K., & Seidler, H. (2003). Cranial integration in Homo: singular warps analysis of the midsagittal plane in ontogeny and evolution. Journal of Human Evolution, 44(2), 167-187.

McNulty, K. P. (2009). Computing singular warps from Procrustes aligned coordinates. Journal of human evolution, 57(2), 191-194.

Rohlf, F. J., & Corti, M. (2000). Use of two-block partial least-squares to study covariation in shape. Systematic Biology, 49(4), 740-753.
}
\author{Cabrera Juan Manuel} 
\examples{

data("aegla_landmarks")
result<-GpaResiduals(lands = aegla_landmarks)

#GPA consensus (matrix)
result$consens

#GPA consensus (vector)
result$cvectorized

#GPA residual matrix
result$resid
}
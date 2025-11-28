\name{gw.zi.bw}
\alias{gw.zi.bw}
\title{Optimal bandwidth estimation for Geographically Weighted Zero Inflated Poisson Regression (GWZIPR)}
\description{This function helps choosing the optimal bandwidth for the Geographically Weighted Zero Inflated Poisson Regression (GWZIPR).}
\usage{
gw.zi.bw(formula, family, dframe, coords, kernel, algorithm="exhaustive", 
       optim.method="Nelder-Mead", b.min=NULL, b.max=NULL, step=NULL)}
\arguments{
  \item{formula}{the local model to be fitted using the same syntax used in the zeroinfl function of the R package \code{pscl}. This is a sting (a symbolic description of the model) that is passed to the sub-models' \code{\link{zeroinfl}} function. For more details look at the details of the \code{\link{zeroinfl}} function.}
  \item{family}{a specification of the count model family to be used in the local model as in the \code{\link{zeroinfl}} function. Currently the only option tested is "poisson".}
  \item{dframe}{a numeric data frame of at least two suitable variables (one dependent and one independent)}
  \item{coords}{a numeric matrix or data frame of two columns giving the X,Y coordinates of the observations}
  \item{kernel}{the kernel to be used in the regression. Options are "adaptive" or "fixed". The weighting scheme used here is defined by the bi-square function \code{(weight = (1-(ndist/H)^2)^2} for distances less than or equal to \code{H}, \code{0} otherwise)}
  \item{algorithm}{a character argument that specifies whether the function will use an \code{exhaustive} or a  \code{heuristic} algorithm. In the first case all possible bandwidths within a range are being tested. In the second case the  \code{\link{optim}} function is being used allowing for the choice of various optimisation methods (such as \code{Brent} or \code{BFGS}) that may find a global or local optimum. The default algorithm is \code{"exhaustive"}}
  \item{optim.method}{the optimisation method to be used. A detailed discussion is available at the 'Details' section of the function \code{\link{optim} (stats)}. Example methods are \code{"Nelder-Mead"}, \code{"Brent"}, \code{"BFGS"}, \code{"CG"} and \code{"L-BFGS-B"}. The default method is \code{"Nelder-Mead"}.}
  \item{b.min}{the minimum bandwidth. This is important for both algorithms. In the case of the \code{exhaustive} algorithm it sets the lower boundary for the range in which the function will compute the CV score for each possible bandwidth. In the case of the \code{heuristic} algorithm it provides the initial value for the bandwidth to be optimised which is very important. In the latter case b.min and b.max should be provided if the optimisation method \code{"L-BFGS-B"} or \code{"Brent"} has been selected.}
  \item{b.max}{the maximum bandwidth. This is important for both algorithms. In the case of the \code{exhaustive} algorithm it sets the upper boundary for the range in which the function will compute the CV score for each possible bandwidth. In the case of the \code{heuristic} algorithm b.max and b.min should be provided if the optimisation method \code{"L-BFGS-B"} or \code{"Brent"} has been selected.}
  \item{step}{this numeric argument is used only in the case of a \code{fixed} kernel indicating the increment of the sequence of bandwidths in between the \code{b.min} and the \code{b.max}. In the case of the \code{adaptive} kernel the increment is 1 neighbour.}
}
\details{
Please carefully read the function \code{\link{optim}(stats)} when using a \code{heuristic} algorithm.
}
\value{
\item{bw}{The optimal bandwidth (fixed or adaptive)}
\item{CV}{The corresponding Cross Validation score for the optimal bandwidth}
\item{CVs}{Available only in the case of the \code{exhaustive} algorithm. This is a numeric matrix in which the first column refers to the bandwidth in test and the second to the corresponding CV score.}
}
\references{
Kalogirou, S. (2016) Destination Choice of Athenians: an application of geographically weighted versions of standard and zero inflated Poisson spatial interaction models, Geographical Analysis, 48(2),pp. 191-230. DOI: 10.1111/gean.12092 \url{https://onlinelibrary.wiley.com/doi/abs/10.1111/gean.12092}
}

\author{Stamatis Kalogirou <stamatis.science@gmail.com>}

\note{Please select the optimisation algorithm carefully. This function needs further testing. Please report any bugs!}

\section{Warning}{Large datasets increase the processing time.}

\seealso{
\code{\link{gwr}}
}

\examples{
RDF <- random.test.data(9,9,3,"zip")
gw.zip.bw <- gw.zi.bw(dep ~ X1 + X2, "poisson", RDF, cbind(RDF$X,RDF$Y), 
                      kernel = 'adaptive', b.min = 54, b.max=55)
}

\keyword{GWR}
\keyword{optimal bandwidth}

\name{Fit}
\alias{Fit}
\title{The Fitting Function of \code{GPM} Package}
\usage{
Fit(X, Y, CorrType = 'G', Eps = 10^(seq(-1, -12)), AnaGr = NULL, Nopt = 5,TraceIt = 0,
MaxIter = 100, Seed = 1, LowerBound = NULL, UpperBound = NULL, 
StopFlag = 1, Progress = 0, DoParallel = 0, Ncores = NULL)
}
\arguments{
\item{X}{Matrix containing the training (aka design or input) data points. The rows and columns of \code{X} denote individual observation settings and input dimension, respectively.}

\item{Y}{Matrix containing the output (aka response) data points. The rows and columns of \code{Y} denote individual observation responses and output dimension, respectively.}

\item{CorrType}{The type of the correlation function of the GP model. Choices include \code{'G'} (default), \code{'PE'}, \code{'LBG'}, and \code{'LB'}. See the \code{references} for the details. For smooth (or analytic) functions, choose either \code{'G'} or \code{'LBG'}. Fitting is faster if \code{'G'} is chosen.}

\item{Eps}{A vector containing the smallest eigen value(s) that the correlation matrix is allowed to have. The elements of Eps must be in [0, 1] and sorted in a descending order.}

\item{AnaGr}{Flag indicating whether the gradient of the log-likelihood should be taken analytically (\code{!= 0}) or numerically (\code{== 0}). For now, only available when \code{CorrType == 'G'} or \code{CorrType == 'PE'}. If \code{AnaGr != 0}, the fitted model will generally be more accurate.}

\item{Nopt}{The number of times the log-likelihood function is optimized when \code{Eps[1]} is used to constraint the smallest eigen value that the correlation matrix is allowed to have. Higher \code{Nopt} will increase fitting time as well as the chances of finding the global optimum. If \code{nrow(X)} is large (i.e., large training datasets), \code{Nopt} can be small.Analyzing the optimization results for \code{Eps[1]} and when \code{Progress != 0} will determine if \code{Nopt} has been large enough.}

\item{TraceIt}{Non-negative integer. If positive, tracing information on the progress of the optimization is \strong{printed}. There are six levels of tracing (see \code{\link{optim}}) and higher values will produce more tracing information.}

\item{MaxIter}{Maximum number of iterations allowed for each optimization (see \code{\link{optim}}).}

\item{Seed}{An integer for the random number generator. Use this to make the results reproducible.}

\item{LowerBound, UpperBound}{To estimate the scale (aka roughness) parameters of the correlation function, the feasible range should be defined. \code{LowerBound} and \code{UpperBound} are vectors determining, resepectively, the lower and upper bounds. Their length depends on the parametric form of the correlation function (see \code{reference 1} for the details).}

\item{StopFlag}{Flag indicating whether the optimization must be stopped if the negative log-likelihood increases with decreasing \code{Eps[i]}.}

\item{Progress}{Flag indicating if the fitting process should be summarized. Set it to \code{!=0} to turn it on.}
\item{DoParallel}{If \code{!= 0}, optimizations will be done in parallel.}
\item{Ncores}{Number of cores to use if \code{DoParallel != 0}. The default is the maximum number of physical cores.}
}
\value{
Model A list containing the following components:
\itemize{
\item{\code{CovFunc}} {A list containing the type and estimated parameters of the correlation function.}
\item{\code{Data}} {A list storing the original (but scaled) data.}
\item{\code{Details}} {A list of some parameters (used in prediction) as well as some values reporting the total run-time (\code{cost}) and the added nugget (\code{Nug_opt}) for satisfying the constraint on the smallest eigen value of the correlation matrix.}
\item{\code{OptimHist}} {The optimization history.}
\item{\code{Setting}} {The default/provided settings for running the code.}
}
}
\description{
Fits a Gaussian process (GP) to a set of simulation data as described in \code{reference 1}. Both the inputs and outputs can be multi-dimensional. The outputs can be noisy in which case it is assumed that the noise is stationary (i.e., its variance is not a function of x).
}
\examples{
# 1D example: Fit a model (with default settings) and evaluate the performance
# by computing the root mean squared error (RMSE) in prediction.
library(lhs)
X <- 5*maximinLHS(15, 1)
Y <- 2*sin(2*X) + log(X+1)
M <- Fit(X, Y)
XF <- matrix(seq(0, 5, length.out = 100), 100, 1)
YF <- Predict(XF, M)
RMSE <- sqrt(mean((YF$YF - (2*sin(2*XF) + log(XF+1)))^2))

\dontrun{
# 1D example: Fit a model, evaluate the performance, and plot the response
# along with 95\% prediction interval
X <- 10*maximinLHS(10, 1) - 5
Y <- X*cos(X)
M <- Fit(X, Y)
XF <- matrix(seq(-5, 5, length.out = 500), 500, 1)
YF <- Predict(XF, M)
RMSE <- sqrt(mean((YF$YF - (XF*cos(XF)))^2))
Draw(M, 1, res = 20)

# 2D example: Fit a model, evaluate the performance, and plot the response
# surface along with 95\% prediction interval
X <- 2*maximinLHS(10, 2) - 1
Y <- X[, 1]^2 + X[, 2]^2
M <- Fit(X, Y, CorrType = "PE")
XF <- 2*maximinLHS(100, 2) - 1
YF <- Predict(XF, M)
RMSE <- sqrt(mean((YF$YF - (XF[, 1]^2 + XF[, 2]^2))^2))
library(lattice)
Draw(M, c(1, 1), res = 15, PI95=1)

# 2D example: Plot the previous model wrt X1 in the [-2, 2]
# interval with X2=1
Draw(M, c(1, 0), LB = -2, UB = 2, res = 15, PI95=1)

# 3D example: Compare the performance of Gaussian ("G") and lifted Browninan
# with Gamma=1 ("LBG")
X <- 2*maximinLHS(50, 3) - 1
Y <- cos(X[, 1]^2) + 2*sin(X[, 2]^2) + X[, 3]^2
M_G <- Fit(X, Y)
M_LBG <- Fit(X, Y, CorrType = "LBG")
XF <- 2*maximinLHS(500, 3) - 1
YF_G <- Predict(XF, M_G)
YF_LBG <- Predict(XF, M_LBG)
RMSE_G <- sqrt(mean((YF_G$YF - (cos(XF[, 1]^2) + 2*sin(XF[, 2]^2) + XF[, 3]^2))^2))
RMSE_LBG <- sqrt(mean((YF_LBG$YF - (cos(XF[, 1]^2) + 2*sin(XF[, 2]^2) + XF[, 3]^2))^2))

# 3D example: Draw the response in 2D using the M_G model when X3=0
Draw(M_G, c(1, 1, 0), PI95 = 0, Values = 0, X1Label = 'Input 1', X2Label = 'Input 2')

# 3D example: 2D response
X <- 2*maximinLHS(50, 3) - 1
Y <- cbind(cos(X[, 1]^2) + 2*sin(X[, 2]^2) + X[, 3]^2, rowSums(X))
M <- Fit(X, Y)
Draw(M, c(0, 1, 1), Response_ID = 2, Values = 0.5)

# 2D example with noise
X <- 2*maximinLHS(100, 2) - 1
Y <- X[, 1]^2 + X[, 2]^2 + matrix(rnorm(nrow(X), 0, .5), nrow(X), 1)
M <- Fit(X, Y)
# Estimating the noise variance (should be close to 0.5^2)
M$Details$Nug_opt*M$CovFunc$Parameters$Sigma2*M$Data$Yrange^2
}
}
\references{
\enumerate{
\item Bostanabad, R., Kearney, T., Tao, S., Apley, D. W. & Chen, W. (2018) Leveraging the nugget parameter for efficient Gaussian process modeling. Int J Numer Meth Eng, 114, 501-516.
\item Plumlee, M. & Apley, D. W. (2017) Lifted Brownian kriging models. Technometrics, 59, 165-177.
}
}
\seealso{
\code{\link[stats]{optim}} for the details on \code{L-BFGS-B} algorithm used in optimization.\cr
\code{\link[GPM]{Predict}} to use the fitted GP model for prediction.\cr
\code{\link[GPM]{Draw}} to plot the response via the fitted model.
}

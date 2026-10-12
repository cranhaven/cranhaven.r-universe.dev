\name{influence}
\alias{influence}
\title{Assessment of local influence in SYMARMA models}

\description{This function discusses local influence analysis in SYMARMA models with Student-\emph{t} and Gaussian
distributions through Billor and Loyne's slope, Cook's curvature and Lesaffre and Verbeke's curvature
using the methodology of benchmarks proposed by Zhang and King. Although this function is concerned primarily
with local influence, some discussion of assessing global influence is presented.
}

\usage{
influence(model, diag="slope", scheme="additive",iter=2000,
 alpha=0.95, theta=0.05, plot="TRUE")
}

\arguments{
  \item{model}{a result of a call to \code{\link{elliptical.ts}}.}
  \item{diag}{a description of the diagnostic method: \dQuote{slope} for Billor and Loyne's, \dQuote{cook} for Cook's and \dQuote{lv} for Lesaffre and Verbeke's. The default is to \emph{slope}.}
  \item{scheme}{a description of the perturbation scheme: \dQuote{additive} for data additive perturbation and \dQuote{dispersion} for dispersion parameter perturbation. The default is to \emph{additive}.} 
  \item{iter}{integer giving the number of iterations for construction of benckmarks. Default is 2,000 iterations.}
  \item{alpha}{percentile for benchmarks in assessing global influence (\emph{BS_0} and \emph{BC_0}) and first assessing local influence (\emph{BS_1} and \emph{BC_1}), e.g., 0.95.}
  \item{theta}{percentile for benchmarks in assessing global influence second assessing local influence (\emph{BS_2} and \emph{BC_2}), e.g., 0.05.}
  \item{plot}{a logical indicating if plot should be produced.}
}

\value{
\item{Indiv1}{individual benchmark type I.}
\item{Indiv2}{individual benchmark type II.}
\item{VectorInd}{slope or curvature vector.}
}

\references{
Cook, R.D. (1986). Assessment of local influence (with discussion). \emph{Journal of the Royal Statistical Society}, \strong{B 48}, 133-169.

Billor, N. and Loynes, R.M. (1993). Local influence: A new approach. \emph{Communications in Statistics Theory and Methods}, \strong{22}, 1595-1611. doi: \href{https://doi.org/10.1080/03610929308831105}{10.1080/03610929308831105}.

Lesaffre, F. and Verbeke, G. (1998). Local influence in linear mixed models. \emph{Biometrics}, \strong{38}, 963-974. doi: \href{https://doi.org/10.2307/3109764}{10.2307/3109764}.

Zhang, X. and King, M.L. (2005). Influence diagnostics in generalized autoregressive conditional heteroscedasticity processes. \emph{J. Business Econ. Statist.}, \strong{23}, 118-129. 
doi: \href{https://doi.org/10.1198/073500104000000217}{10.1198/073500104000 000217}.
}

\author{
Vinicius Quintas Souto Maior and Francisco Jose A. Cysneiros

Maintainer: Vinicius Quintas Souto Maior <vinicius@de.ufpe.br>
}

\examples{
data(assets)
attach(assets)

# Return in the prices on Microsoft and SP500 index

N = length(msf)
.sp500 = ((sp500[2:N]-sp500[1:(N-1)])/sp500[1:(N-1)])*100
.msf = ((msf[2:N]-msf[1:(N-1)])/msf[1:(N-1)])*100

# The T-bill rates were divided by 253 to convert to a daily rate

.tbill = tbill/253

# Excess return in the d prices on Microsoft and SP500 index

Y = .msf - .tbill[1:(N-1)]
X = .sp500 - .tbill[1:(N-1)]

# Period from April 4, 2002 to October 4, 2002

serie = Y[2122:2240]
aux = cbind(X[2122:2240])

# Fit SYMARMA models

fit.1 = elliptical.ts(serie,order=c(0,0,1),xreg=aux,include.mean=FALSE,
 family="Normal")
fit.2 = elliptical.ts(serie,order=c(0,0,1),xreg=aux,include.mean=FALSE,
 family="Student", index1=4)

# Assessment of local influence

influence(fit.1,diag="slope",scheme="additive",iter=20,plot="FALSE") 
influence(fit.2,diag="lv",scheme="additive",iter=20,plot="FALSE") 
}



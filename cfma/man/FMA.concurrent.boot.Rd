\name{FMA.concurrent.boot}
\alias{FMA.concurrent.boot}
\title{ Functional mediation analysis under concurrent regression model with point-wise bootstrap confidence interval
}
\description{ This function performs functional mediation regression under the concurrent model with given tuning parameter. Point-wise confidence bands are obtained from bootstrap.
}
\usage{
FMA.concurrent.boot(Z, M, Y, intercept = TRUE, basis = NULL, Ld2.basis = NULL, 
    basis.type = c("fourier"), nbasis = 3, timeinv = c(0, 1), timegrids = NULL, 
    lambda.m = 0.01, lambda.y = 0.01, sims = 1000, boot = TRUE, 
    boot.ci.type = c("bca", "perc"), conf.level = 0.95, verbose = TRUE)
}
\arguments{
  \item{Z}{ a data matrix. \code{Z} is the treatment trajectory in the mediation analysis. The number of rows is the number of subjects, and the number of columns is the number of measured time points.
}
  \item{M}{ a data matrix. \code{M} is the mediator trajectory in the mediation analysis. The number of rows is the number of subjects, and the number of columns is the number of measured time points.
}
  \item{Y}{ a data matrix. \code{Y} is the outcome trajectory in the mediation analysis. The number of rows is the number of subjects, and the number of columns is the number of measured time points.
}
  \item{intercept}{ a logic variable. Default is \code{TRUE}, an intercept term is included in the regression model.
}
  \item{basis}{ a data matrix. Basis function used in the functional data analysis. The number of columns is the number of basis function considered. If \code{basis = NULL}, Fourier basis functions will be generated.
}
  \item{Ld2.basis}{ a data matrix. The second derivative of the basis function. The number of columns is the number of basis function considered. If \code{Ld2.basis = NULL}, the second derivative of Fourier basis functions will be generated.
}
  \item{basis.type}{ a character of basis function type. Default is Fourier basis (\code{basis.type = "fourier"}).
}
  \item{nbasis}{ an integer, the number of basis function included. If \code{basis} is provided, this argument will be ignored.
}
  \item{timeinv}{ a numeric vector of length two, the time interval considered in the analysis. Default is (0,1).
}
  \item{timegrids}{ a numeric vector of time grids of measurement. If \code{timegrids = NULL}, it is assumed the between measurement time interval is constant.
}
  \item{lambda.m}{ a numeric value of the tuning parameter in the mediator model.
}
  \item{lambda.y}{ a numeric value of the tuning parameter in the outcome model.
}
  \item{sims}{ an integer indicating the number of simulations for inference.
}
  \item{boot}{ a logical value, indicating whether or not bootstrap should be used. Default is \code{TRUE}.
}
  \item{boot.ci.type}{ a character of confidence interval method. \code{boot.ci.type = "bca"} bias corrected confidence interval; \code{boot.ci.type = "perc"} percentile confidence interval.
}
  \item{conf.level}{ a number of significance level. Default is 0.95.
}
  \item{verbose}{ a logical value, indicating whether print out bootstrap replications.
}
}
\details{ The concurrent mediation model is
    \deqn{M(t)=Z(t)\alpha(t)+\epsilon_{1}(t),}
    \deqn{Y(t)=Z(t)\gamma(t)+M(t)\beta(t)+\epsilon_{2}(t),}
    where \eqn{\alpha(t)}, \eqn{\beta(t)}, \eqn{\gamma(t)} are coefficient curves. The model coefficient curves are estimated by minimizing the penalized \eqn{L_{2}}-loss.
}
\value{
    \item{alpha}{a list of output for \eqn{\alpha} estimate

        \code{coefficients}{: the result of the coefficient estimates corresponding to the basis function}

        \code{curve}{: the point-wise estimate of the coefficient curve}
    }
    \item{gamma}{: a list of output for \eqn{\gamma} estimate

        \code{coefficients}{: the result of the coefficient estimates corresponding to the basis function}

        \code{curve}{: the point-wise estimate of the coefficient curve}
    }
    \item{beta}{a list of output for \eqn{\beta} estimate

        \code{coefficients}{: the result of the coefficient estimates corresponding to the basis function}

        \code{curve}{: the point-wise estimate of the coefficient curve}
    }
    \item{IE}{a list of output for indirect effect estimate

        \code{coefficients}{: the result of the coefficient estimates corresponding to the basis function}

        \code{curve}{: the point-wise estimate of the coefficient curve}
    }
    \item{DE}{a list of output for direct effect estimate

        \code{coefficients}{: the result of the coefficient estimates corresponding to the basis function}

        \code{curve}{: the point-wise estimate of the coefficient curve}
    }
}
\references{Zhao et al. (2017). \emph{Functional Mediation Analysis with an Application to Functional Magnetic Resonance Imaging Data.} arXiv preprint arXiv:1805.06923.
}
\author{
Yi Zhao, Johns Hopkins University, \email{zhaoyi1026@gmail.com};

Xi Luo, Brown University \email{xi.rossi.luo@gmail.com}; 

Martin Lindquist, Johns Hopkins University, \email{mal2053@gmail.com}; 

Brian Caffo, Johns Hopkins University, \email{bcaffo@gmail.com}
}
\examples{

##################################################
# Concurrent functional mediation model
data(env.concurrent)
Z<-get("Z",env.concurrent)
M<-get("M",env.concurrent)
Y<-get("Y",env.concurrent)

\donttest{
# consider Fourier basis
fit.boot<-FMA.concurrent.boot(Z,M,Y,intercept=FALSE,timeinv=c(0,300))
}
##################################################
}
\keyword{ models }

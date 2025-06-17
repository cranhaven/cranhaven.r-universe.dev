\name{FMA.concurrent}
\alias{FMA.concurrent}
\title{ Functional mediation analysis under concurrent regression model
}
\description{ This function performs functional mediation regression under the concurrent model with given tuning parameter.
}
\usage{
FMA.concurrent(Z, M, Y, intercept = TRUE, basis = NULL, Ld2.basis = NULL, 
    basis.type = c("fourier"), nbasis = 3, timeinv = c(0, 1), timegrids = NULL, 
    lambda.m = 0.01, lambda.y = 0.01)
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
}
\details{ The concurrent mediation model is
    \deqn{M(t)=Z(t)\alpha(t)+\epsilon_{1}(t),}
    \deqn{Y(t)=Z(t)\gamma(t)+M(t)\beta(t)+\epsilon_{2}(t),}
    where \eqn{\alpha(t)}, \eqn{\beta(t)}, \eqn{\gamma(t)} are coefficient curves. The model coefficient curves are estimated by minimizing the penalized \eqn{L_{2}}-loss.
}
\value{
    \item{basis}{the basis functions used in the analysis.}
    \item{M}{a list of output for the mediator model

        \code{coefficient}{the estimated coefficient with respect to the basis function}

        \code{curve}{: the estimated coefficient curve}

        \code{fitted}{: the fitted value of \code{M}}

        \code{lambda}{: \eqn{\lambda} value}
    }
    \item{Y}{a list of output for the outcome model

        \code{coefficient}{: the estimated coefficient with respect to the basis function}

        \code{curve}{: the estimated coefficient curve}

        \code{fitted}{: the fitted value of \code{Y}}

        \code{lambda}{: \eqn{\lambda} value}
    }
    \item{IE}{a list of output for the indirect effect comparing \eqn{Z_{1}(t)=1} versus \eqn{Z_{0}(t)=0}

        \code{coefficients}{: the coefficient with respect to the basis function}

        \code{curve}{: the estimated causal curve}
    }
    \item{DE}{a list of output for the direct effect comparing \eqn{Z_{1}(t)=1} versus \eqn{Z_{0}(t)=0}

        \code{coefficients}{: the coefficient with respect to the basis function}
        
        \code{curve}{: the estimated causal curve}
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

# consider Fourier basis
fit<-FMA.concurrent(Z,M,Y,intercept=FALSE,timeinv=c(0,300))

# estimate of alpha
plot(fit$M$curve[1,],type="l",lwd=5)
lines(get("alpha",env.concurrent),lty=2,lwd=2,col=2)

# estimate of gamma
plot(fit$Y$curve[1,],type="l",lwd=5)
lines(get("gamma",env.concurrent),lty=2,lwd=2,col=2)

# estimate of beta
plot(fit$Y$curve[2,],type="l",lwd=5)
lines(get("beta",env.concurrent),lty=2,lwd=2,col=2)

# estimate of causal curves
plot(fit$IE$curve,type="l",lwd=5)
plot(fit$DE$curve,type="l",lwd=5)
##################################################
}
\keyword{ models }

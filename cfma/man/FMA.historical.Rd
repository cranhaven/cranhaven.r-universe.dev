\name{FMA.historical}
\alias{FMA.historical}
\title{ Functional mediation analysis under historical influence model
}
\description{ This function performs functional mediation regression under the historical influence model with given tuning parameter.
}
\usage{
FMA.historical(Z, M, Y, delta.grid1 = 1, delta.grid2 = 1, delta.grid3 = 1, 
    intercept = TRUE, basis1 = NULL, Ld2.basis1 = NULL, basis2 = NULL, Ld2.basis2 = NULL, 
    basis.type = c("fourier"), nbasis1 = 3, nbasis2 = 3, 
    timeinv = c(0, 1), timegrids = NULL, 
    lambda1.m = 0.01, lambda2.m = 0.01, lambda1.y = 0.01, lambda2.y = 0.01)
}
\arguments{
  \item{Z}{ a data matrix. \code{Z} is the treatment trajectory in the mediation analysis. The number of rows is the number of subjects, and the number of columns is the number of measured time points.
}
  \item{M}{ a data matrix. \code{M} is the mediator trajectory in the mediation analysis. The number of rows is the number of subjects, and the number of columns is the number of measured time points.
}
  \item{Y}{ a data matrix. \code{Y} is the outcome trajectory in the mediation analysis. The number of rows is the number of subjects, and the number of columns is the number of measured time points.
}
  \item{delta.grid1}{ a number indicates the width of treatment-mediator time interval in the mediator model.
}
  \item{delta.grid2}{ a number indicates the width of treatment-outcome time interval in the outcome model.
}
  \item{delta.grid3}{ a number indicates the width of mediator-outcome time interval in the outcome model.
}
  \item{intercept}{ a logic variable. Default is \code{TRUE}, an intercept term is included in the regression model.
}
  \item{basis1}{ a data matrix. Basis function on the \eqn{s} domain used in the functional data analysis. The number of columns is the number of basis function considered. If \code{basis = NULL}, Fourier basis functions will be generated.
}
  \item{Ld2.basis1}{ a data matrix. The second derivative of the basis function on the \eqn{s} domain. The number of columns is the number of basis function considered. If \code{Ld2.basis = NULL}, the second derivative of Fourier basis functions will be generated.
}
  \item{basis2}{ a data matrix. Basis function on the \eqn{t} domain used in the functional data analysis. The number of columns is the number of basis function considered. If \code{basis = NULL}, Fourier basis functions will be generated.
}
  \item{Ld2.basis2}{ a data matrix. The second derivative of the basis function on the \eqn{t} domain. The number of columns is the number of basis function considered. If \code{Ld2.basis = NULL}, the second derivative of Fourier basis functions will be generated.
}
  \item{basis.type}{ a character of basis function type. Default is Fourier basis (\code{basis.type = "fourier"}).
}
  \item{nbasis1}{ an integer, the number of basis function on the \eqn{s} domain included. If \code{basis1} is provided, this argument will be ignored.
}
  \item{nbasis2}{ an integer, the number of basis function on the \eqn{t} domain included. If \code{basis2} is provided, this argument will be ignored.
}
  \item{timeinv}{ a numeric vector of length two, the time interval considered in the analysis. Default is (0,1).
}
  \item{timegrids}{ a numeric vector of time grids of measurement. If \code{timegrids = NULL}, it is assumed the between measurement time interval is constant.
}
  \item{lambda1.m}{ a numeric vector of tuning parameter values on the \eqn{s} domain in the mediator model. 
}
  \item{lambda2.m}{ a numeric vector of tuning parameter values on the \eqn{t} domain in the mediator model. 
}
  \item{lambda1.y}{ a numeric vector of tuning parameter values on the \eqn{s} domain in the outcome model. 
}
  \item{lambda2.y}{ a numeric vector of tuning parameter values on the \eqn{t} domain in the outcome model. 
}
}
\details{ The historical influence mediation model is
    \deqn{M(t)=\int_{\Omega_{t}^{1}}Z(s)\alpha(s,t)ds+\epsilon_{1}(t),}
    \deqn{Y(t)=\int_{\Omega_{t}^{2}}Z(s)\gamma(s,t)ds+\int_{\Omega_{t}^{3}}M(s)\beta(s,t)ds+\epsilon_{2}(t),}
    where \eqn{\alpha(s,t)}, \eqn{\beta(s,t)}, \eqn{\gamma(s,t)} are coefficient curves; \eqn{\Omega_{t}^{j}=[(t-\delta_{j})\vee 0,t]} for \eqn{j=1,2,3}. The model coefficient curves are estimated by minimizing the penalized \eqn{L_{2}}-loss.
}
\value{
    \item{basis1}{the basis functions on the \eqn{s} domain used in the analysis.}
    \item{basis2}{the basis functions on the \eqn{t} domain used in the analysis.}
    \item{M}{a list of output for the mediator model

        \code{coefficient}{: the estimated coefficient with respect to the basis function}

        \code{curve}{: the estimated coefficient curve}

        \code{fitted}{: the fitted value of \code{M}}

        \code{lambda1}{: the \eqn{\lambda} value on the \eqn{s} domain}

        \code{lambda2}{: the \eqn{\lambda} value on the \eqn{t} domain}
    }
    \item{Y}{a list of output for the outcome model

        \code{coefficient}{: the estimated coefficient with respect to the basis function}

        \code{curve}{: the estimated coefficient curve}

        \code{fitted}{: the fitted value of \code{Y}}

        \code{lambda1}{: the \eqn{\lambda} value on the \eqn{s} domain}

        \code{lambda2}{: the \eqn{\lambda} value on the \eqn{t} domain}
    }
    \item{IE}{a list of output for the indirect effect comparing \eqn{Z_{1}(t)=1} versus \eqn{Z_{0}(t)=0}

        \code{curve}{: the estimated causal curve}
    }
    \item{DE}{a list of output for the direct effect comparing \eqn{Z_{1}(t)=1} versus \eqn{Z_{0}(t)=0}

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
# Historical influence functional mediation model
data(env.historical)
Z<-get("Z",env.historical)
M<-get("M",env.historical)
Y<-get("Y",env.historical)

# consider Fourier basis
fit<-FMA.historical(Z,M,Y,delta.grid1=3,delta.grid2=3,delta.grid3=3,
    intercept=FALSE,timeinv=c(0,300))

# estimate of causal curves
plot(fit$IE$curve,type="l",lwd=5)
plot(fit$DE$curve,type="l",lwd=5)
##################################################
}
\keyword{ models }

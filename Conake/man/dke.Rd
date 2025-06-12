\encoding{UTF-8}
\name{dke}
\alias{dke}

\title{
Function for probability density estimation
}
\description{
The function estimates the density in a single value or in a
grid using discrete associated kernels. Four different associated kernels are available: extended beta, gamma, lognormal and reciprocal inverse Gaussian.
}
\usage{
dke(vec_data, ker, bw, x = NULL,a=0,b=1)
}

\arguments{
  \item{vec_data}{
The data sample.
}
  \item{ker}{
The associated kernel: "BE" extended beta, "GA" gamma, "LN" lognormal and "RIG" reciprocal inverse Gaussian.
}
  \item{bw}{
The bandwidth or smoothing parameter.
}
  \item{x}{
The single value or grid where estimation is computed
}
 \item{a}{
The left bound of the support used for extended beta kernel. Default value is 0 for beta kernel.
}
 \item{b}{
The right bound of the support used for extended beta kernel. Default value is 1 for beta kernel.
}

}
\details{
The  kernel estimator \eqn{\widehat{f}_n}{\widehat{f}_n} of  \eqn{f}{f} is defined in the above sections.
We recall that in general, the sum of the estimated values on the support is not equal to 1. In practice, we calculate the  normalizing constant \eqn{C_n} before computing the estimated density \eqn{\tilde{f}_n}{\tilde{f}_n}; see Libengué (2013) .

The bandwidth parameter in the function is obtained using the  cross-validation technique for the four kernels. 

}
\value{
Returns a list containing:
  \item{C_n }{The normalizing constant.}
  \item{f_n}{The values of the  estimated function}
}
\references{

Libengué, F.G. (2013). \emph{Méthode Non-Paramétrique par Noyaux Associés Mixtes et Applications}, Ph.D. Thesis Manuscript (in French) to Université  de Franche-Comté, Besançon, France and Université de  Ouagadougou, Burkina Faso, June 2013, \bold{LMB no. 14334}, Besançon.

}
\author{
W. E. Wansouwé, F.G. Libengué and C. C. Kokonendji   
}
\examples{
## A sample data with n=100.
V<-rgamma(100,1.5,2.6)


##The bandwidth can be the one obtained by cross validation.
h<-0.052
## We choose Gamma kernel.

est<-dke(V,"GA",h)
est$f_n




}


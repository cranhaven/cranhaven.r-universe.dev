\encoding{UTF-8}
\name{Conake-package}
\alias{Conake-package}
\alias{Conake}
\docType{package}
\title{
Continuous Associated Kernel Estimation
}
\description{
 
 Continuous smoothing of probability density function defined on a compact \eqn{T=[a,b]} or  semi-infinite support \eqn{T=[0,\infty)} is performed using four continuous associated kernels: extended beta, gamma, lognormal and reciprocal inverse Gaussian. The cross-validation technique is also  implemented to select the smoothing parameter.
}
\details{
\describe{
\item{The estimated density:}{ The  kernel estimator \eqn{\widehat{f}_n}{\widehat{f}_n} of  \eqn{f}{f} is defined as

\deqn{\widehat{f}_n(x) = \frac{1}{n}\sum_{i=1}^{n}{K_{x,h}(X_i)},} where \eqn{K_{x,h}} is one of the kernels defined below.
 In practice, we first calculate the  normalizing constant

\deqn{{C}_n =   \int_{x\in T}{\widehat{f}_n(x)dx},} where T is the support of the density function. This normalizing constant is not generally equal to 1.  The estimated density is then \eqn{\tilde{f}_n=\widehat{f}_n/C_n}{\tilde{f}_n=\widehat{f}_n/C_n}. 


   }

}

  	Given a data sample, the \pkg{Conake} package allows to compute the density \code{\link{dke}} using one of the four kernel functions: extended beta, gamma,  lognormal and reciprocal inverse Gaussian. The bandwidth parameter is calculated using the cross-validation technique \code{\link{cvbw}}.The kernel functions \code{\link{kef}} are defined below.


	\describe{

		    
	        \item{Extended beta kernel :}{ The extended beta kernel is defined on \eqn{{S}_{x,h,a,b}=[a,b]=T} with \eqn{a<b<\infty}, \eqn{x \in T}{x \in T} and \eqn{h>0}{h>0}:

			 
\deqn{BE_{x,h,a,b}(y) = \frac {(y-a)^{(x-a)/\{(b-a)h\}}(b-y)^{(b-x)/\{(b-a)h\}}} {(b-a)^{1+h^{-1}}B\left(1+(x-a)/(b-a)h,1+(b-x)/(b-a)h\right)}1_{S_{x,h,a,b}}(y),}



where \eqn{B(r,s)=\int_0^1 t^{r-1}(1-t)^{s-1}dt} is the usual beta function with \eqn{r>0}, \eqn{s>0} and  \eqn{1_A}{1[A]} denotes the indicator function of A.  For \eqn{a=0} and \eqn{b=1}, the extended beta kernel corresponds to the beta kernel which is the probability density function of the beta distribution with shape parameters \eqn{1+x/h} and \eqn{(1-x)/h}; see  Libengué (2013).
    }

  \item{Gamma kernel:}{ The gamma kernel is defined on \eqn{{S}_{x,h}=[0,+\infty)=T} with \eqn{x \in T}{x \in T} and \eqn{h>0}{h>0}:
 
   \deqn{GA_{x,h}(y) = \frac {y^{x/h}} {\Gamma(1+x/h)h^{1+x/h}}exp\left(-\frac{y}{h} \right)1_{S_{x,h}}(y),}
    
 where  \eqn{\Gamma(.) }  is the classical gamma function. It is the  probability density function of the gamma distribution with scale parameter \eqn{1+x/h} and shape parameter \eqn{h}; see Chen (2000) and also Libengué (2013).  
   }



 \item{Lognormal kernel :}{The lognormal kernel is defined on \eqn{{S}_{x,h}=[0,\infty)=T} with \eqn{x \in T}{x \in T} and \eqn{h>0}{h>0}:
    

     \deqn{LN_{x,h}(y) = \frac {1} {yh\sqrt{2\pi}}exp\left\{-\frac{1}{2}\left(\frac{1}{h}log(\frac{y}{x})-h \right)^{2}\right\}1_{S_{x,h}}(y).}
    
 It is the probability densiy function  of the classical lognormal distribution with mean \eqn{log(x)+h^{2}} and standard deviation \eqn{h}; see Igarashi and Kakizawa (2015) and also Libengué (2013).
   }




\item{Reciprocal inverse Gaussian kernel:}{ The reciprocal inverse Gaussian kernel  is defined on  \eqn{{S}_{x,h}=]0,\infty)=T} with \eqn{x \in T}{x \in T} and \eqn{h>0}{h>0}:
 
\deqn{RIG_{x,h}(y) = \frac {1}{\sqrt{2\pi hy}} exp\left\{-\frac{\zeta(x,h)}{2h}\left(\frac{y}{\zeta(x,h)}-2+\frac{\zeta(x,h)}{y}\right)\right\}1_{S_{x,h}}(y),}
 

 where \eqn{\zeta(x,h)=(x^2+xh)^{1/2}}.
It is the probability densiy function  of the classical reciprocal inverse Gaussian distribution with mean \eqn{1/\sqrt{x^2+xh}} and standard deviation \eqn{1/h}; see Igarashi and Kakizawa (2015) and also Libengué (2013).

    }


\item{The bandwidth selection:}{  The cross-validation technique  \code{\link{cvbw}} is used for the bandwidth selection. The  optimal parameter is the one which minimizes the cross-validation function defined by:

\deqn{CV(h)=\int_{x\in T}{\{\widehat{f}_n(x)\}^{2}dx}-\frac{2}{n}\sum_{i=1}^{n}{\widehat{f}_{n,-i}(X_i)},}

where \eqn{\widehat{f}_{n,-i}(X_i)=(n-1)^{-1}\sum_{j \ne i}^{n}K_{X_i,h}(X_j)} is the density estimator computed without the observation \eqn{X_{i}}.

   }   

  }


 }\author{
W. E. Wansouwé,  F.G. Libengué and  C. C. Kokonendji 

Maintainer: W. E. Wansouwé <ericwansouwe@gmail.com>
}
\references{
Chen, S. X. (1999). Beta kernels estimators for density functions, \emph{Computational Statistics and Data Analysis}  \bold{31},  131 - 145.

Chen, S. X. (2000). Gamma kernels estimators for density functions, \emph{Annals of the Institute of Statistical Mathematics}  \bold{52},  471 - 480.

Libengué, F.G. (2013). \emph{Méthode Non-Paramétrique par Noyaux Associés Mixtes et Applications}, Ph.D. Thesis Manuscript (in French) to Université  de Franche-Comté, Besançon, France and Université de  Ouagadougou, Burkina Faso, June 2013, \bold{LMB no. 14334}, Besançon.

Igarashi, G. and Kakizawa, Y. (2015). Bias correction for some asymmetric kernel estimators, \emph{Journal of Statistical Planning and Inference}  \bold{159},  37 - 63.

}

\keyword{ package }


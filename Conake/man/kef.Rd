\encoding{UTF-8}
\name{kef}
\alias{kef}

\title{
Continuous associated kernel function
}
\description{
This function computes the discrete associated kernel function; see Chen (1999) and also Chen (2000).
}
\usage{
kef(x, t, h, ker, a = 0, b = 1)
}

\arguments{
   \item{x}{
The target.
}
  \item{t}{
A single value or the grid  where the continuous associated kernel function is computed.
}
  \item{h}{
The bandwidth or smoothing parameter.
}
  \item{ker}{
The associated kernel: "BE" extended beta, "GA" gamma, "LN" lognormal and "RIG" reciprocal inverse Gaussian.
}
 \item{a}{
The left bound of the support used for extended beta kernel. Default value is 0 for beta kernel.
}
 \item{b}{
The right bound of the support used for extended beta kernel. Default value is 1 for beta kernel.
}
}
\details{
The associated kernel is one of the four which have been defined in the sections above : extended beta, gamma, lognormal and reciprocal inverse Gaussian; see Igarashi and Kakizawa (2015) and also Libengué (2013).
}
\value{
Returns the value of the discrete associated kernel function at t according to the target and the bandwidth.
}
\references{

Chen, S. X. (1999). Beta kernels estimators for density functions, \emph{Computational Statistics and Data Analysis}  \bold{31},  131 - 145.

Chen, S. X. (2000). Gamma kernels estimators for density functions, \emph{Annals of the Institute of Statistical Mathematics}  \bold{52},  471 - 480.

Libengué, F.G. (2013).\emph{Méthode Non-Paramétrique par Noyaux Associés Mixtes et Applications}, Ph.D. Thesis Manuscript (in French) to Université  de Franche-Comté, Besançon, France and Université de  Ouagadougou, Burkina Faso, June 2013, \bold{LMB no. 14334}, Besançon.

Igarashi, G. and Kakizawa, Y. (2015). Bias correction for some asymmetric kernel estimators, \emph{Journal of Statistical Planning and Inference}  \bold{159},  37 - 63.

}
\author{
W. E. Wansouwé, F.G. Libengué and  C. C. Kokonendji 
}

\examples{
x<-4
h<-0.1
t<-0:10
kef(x,t,h,"GA")

}


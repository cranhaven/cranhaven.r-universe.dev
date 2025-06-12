\encoding{UTF-8}
\name{cvbw}
\alias{cvbw}

\title{
Cross-validation function for bandwidth selection
}
\description{
The function allows to calculate the optimal bandwidth using the cross-validation method. Four kernels are available: extended beta, gamma, lognormal and reciprocal inverse Gaussian kernels.

}
\usage{
cvbw(Vec, bw = NULL, ker,a=0,b=1)
}

\arguments{
  \item{Vec}{
The sample data.
}
  \item{bw}{
The sequence of bandwidths where the cross-validation is computed. If NULL, the procedure defines a sequence of bandwidths.
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
The selection of the bandwidth parameter is crucial. If the bandwidth is small, we will obtain an
undersmoothed estimator, with high variability. On the contrary, if the value  is large, the
resulting estimator will be very smoothed and farther from the function that we are trying to estimate. See Libengué (2013).
}
\value{
Returns a list containing:
  \item{hcv }{The optimal bandwidth obtained by cross-validation.}
  \item{CV }{The values of the cross-validation function in the sequence of bandwidths.}
  \item{bw}{The sequence of bandwidths used.}
}
\references{
Libengué, F.G. (2013). \emph{Méthode Non-Paramétrique par Noyaux Associés Mixtes et Applications}, Ph.D. Thesis Manuscript (in French) to Université  de Franche-Comté, Besançon, France and Université de  Ouagadougou, Burkina Faso, June 2013, \bold{LMB no. 14334}, Besançon.

}
\author{
W. E. Wansouwé, F.G. Libengué and C. C. Kokonendji  
}
\examples{
## Data can be simulated data or real data
## We use simulate data 
## and then compute the cross validation. 
Vec<-rgamma(100,1.5,2.6)
\dontrun{
CV<-cvbw(Vec,ker="GA")
CV$hcv
}

}


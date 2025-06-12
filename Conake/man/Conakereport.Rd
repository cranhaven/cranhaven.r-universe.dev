\name{Conakereport}
\alias{Conakereport}
\title{
A brief summary of the results
}
\description{
For a sample, the function gives automatically the result of computations of  the normalizing constant and the smoothing parameter. One can then plot the histogram.
}

\usage{
Conakereport(Vec, ker, h = NULL, a = 0, b = 1)
}

\arguments{
  \item{Vec}{
The sample of data.
}
  \item{ker}{
The kernel function:
}
  \item{h}{
The bandwidth or smoothing parameter. 
}
 \item{a}{
The left bound of the support used for extended beta kernel. Default value is 0 for beta kernel.
}
 \item{b}{
The right bound of the support used for extended beta kernel. Default value is 1 for beta kernel.
}
}

\value{

Returns a list containing:
\item{h_n }{The bandwith parameter used to compute f_n}
\item{C_n }{The normalizing constant}
 
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
Vec<-rgamma(100,1.5,2.6)
\dontrun{
Conakereport(V,ker="GA")
}

}

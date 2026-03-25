\name{Generic methods for spei objects}

\alias{alphaBetaMV}
\alias{calcSPI}
\alias{distEmpiric}
\alias{fitnessGamma}
\alias{matrizToSerie}
\alias{plotSerieStandart}
\alias{sumaMoviles}
\alias{sumaMovilesSerie}
\alias{testNS}

\title{Generic methods for \code{SPIGA} objects.}

\description{Generic methods for extracting information and plotting \code{SPIGA} objects.}

\usage{
%sumaMoviles(Ps, scale, rows, cols=12)

%sumaMovilesSerie(Ps, scale)

%alphaBetaMV(Pnn,nn)

calcSPI(Pt,alpha, beta,m, nd)

%fitnessGamma(x1,x2,P)

%distEmpiric(x,m=1)

%testNS(o,c)

%matrizToSerie(m)

%plotSerieStandart(P, ylabel='ANOM')
}

\arguments{
\item{Pt}{monthly precipitation series.}
  \item{alpha}{parameter to Gamma function.}
  \item{beta}{parameter to Gamma function.}
  \item{m}{Number of zeros in the column.}
  \item{nd}{Number total data.}
}

%\details{}

\references{
McKee, Thomas B. and Doesken, Nolan J. and Kleist, John. 1993. The relationship of Drought Frequency and Duration to Time Scales. \emph{Eighth Conference on Applied Climatology}

A. Belauneh and J. Adamowski. Standard Precipitation Index Drought Forecasting Using Neural Networks, Wavelet Neural Networks, and Support Vector Regression. \emph{Applied Computational Intelligence and Soft Computing}, http://dx.doi.org/10.1155/2012/794061
%\url{http://}
}

\author{
Iván Arturo Ayala Bizarro <ivan.ayala@unh.edu.pe>

Jessica Zúñiga Mendoza <zumeje@gmail.com>
}


%\note{}

%\seealso{}

%\examples{}

%\keyword{}
%\keyword{}

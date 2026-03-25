\name{SPIDataset}

\alias{Pm_Pisco}
\alias{alphaGA_SPI3}
\alias{betaGA_SPI3}

\title{The data set for illustratrating the functions of the SPIGA package}

\description{The set used, data are monthly rainfall (1981-2015) and the dimensionless parameters to calculate the SPI drought.}

\usage{
data(Pm_Pisco)
data(alphaGA_SPI3)
data(betaGA_SPI3)
}

\format{
\code{Pm_Pisco} dataframe with:
\describe{
\item{YEAR}{ monthly precipitation totals, in mm.}
\item{MONTH}{ monthly precipitation totals, in mm.}
\item{P1}{ monthly precipitation totals st-1, in mm.}
\item{P2}{ monthly precipitation totals st-2, in mm.}
\item{...}{monthly precipitation totals st-n, in mm.}
}

\code{alphaGA_SPI3} dataset: monthly alpha parameter.
\describe{
\item{mon}{ month analysis}
\item{st-1}{ monthly alpha parameter station 1}
\item{st-2}{ monthly alpha parameter station 2}
\item{st-n}{ monthly alpha parameter station n}
}

\code{betaGA_SPI3} dataset: monthly beta parameter.
\describe{
\item{mon}{ month analysis}
\item{st-1}{ monthly beta parameter station 1}
\item{st-2}{ monthly beta parameter station 2}
\item{st-n}{ monthly beta parameter station n}
}
}

\references{
McKee, Thomas B. and Doesken, Nolan J. and Kleist, John. 1993. The relationship of Drought Frequency and Duration to Time Scales. \emph{Eighth Conference on Applied Climatology}

A. Belauneh and J. Adamowski. Standard Precipitation Index Drought Forecasting Using Neural Networks, Wavelet Neural Networks, and Support Vector Regression. \emph{Applied Computational Intelligence and Soft Computing}, http://dx.doi.org/10.1155/2012/794061
%\url{http://}
}

\source{The \code{Pm_Pisco} data were obtained from the Peruvian Interpolation data of the SENAMHI's Climatological and Hidrological Observations, SENAMHI-PERU. \url{http://peruclima.pe/}.}

\author{
Iván Arturo Ayala Bizarro <ivan.ayala@unh.edu.pe>

Jessica Zúñiga Mendoza <zumeje@gmail.com>
}

%\note{}

%\seealso{}

\examples{
data(Pm_Pisco)
names(Pm_Pisco)
summary(Pm_Pisco)

data(alphaGA_SPI3)
names(alphaGA_SPI3)
summary(alphaGA_SPI3)

data(betaGA_SPI3)
names(betaGA_SPI3)
summary(betaGA_SPI3)
}

%\keyword{}
%\keyword{}

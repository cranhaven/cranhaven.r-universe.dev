\name{fscaret-package}
\alias{fscaret-package}
\docType{package}
\title{
Automated feature selection caret (fscaret)
}
\description{
This package provide fast and automated feature selection based on caret package modeling methods.
The main advantage of this extension is that it requires minimum user involvement.
Also the variety of used methods in combination with the scaling according to RMSE or MSE obtained from models profit the user.
The idea is based on the assumption that the variety of models will balance the roughness of calculations (default model settings are applied).
On Windows OS the time limiting function is off, multicore functionalaity is enabled via parLapply() function of package 'parallel'.
Acknowledgments:\cr
This work was funded by Poland-Singapore bilateral cooperation project no 2/3/POL-SIN/2012\cr
}
\details{
\tabular{ll}{
Package: \tab fscaret\cr
Type: \tab Package\cr
Version: \tab 0.9.4.2\cr
Date: \tab 2017-12-07\cr
License: \tab GPL-2 | GPL-3\cr
}
}
\author{
Jakub Szlek <j.szlek@uj.edu.pl>
Contributions from Aleksander Mendyk, also stackoverflow and r-help@r-project.org mailing list community.\cr
Maintainer: Jakub Szlek <j.szlek@uj.edu.pl>.\cr
}
\references{
Kuhn M. (2008) Building Predictive Models in R Using the caret Package \emph{Journal of Statistical Software} \bold{28(5)} \url{http://www.jstatsoft.org/}.\cr
Szlek J, Paclawski A, Lau R, Jachowicz R, Mendyk A. Heuristic modeling of macromolecule release from PLGA microspheres. \emph{International Journal of Nanomedicine.}
\bold{2013:8(1); 4601 - 4611}. \url{http://www.dovepress.com/international-journal-of-nanomedicine-journal}.
}
\keyword{ package }
\seealso{
\code{\link[caret]{train}}, \code{\link[caret]{trainControl}}, \code{\link[caret]{rfeControl}} 
by Max Kuhn <Max.Kuhn at pfizer.com> and \code{\link{predict}} base utilities
}

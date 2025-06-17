\encoding{UTF-8}
\name{TP.idm-package}
\alias{TP.idm-package}
\alias{TP.idm}
\docType{package}
\title{
\packageTitle{TP.idm}
}
\description{
Estimation of transition probabilities for the illness-death model. Both the Aalen-Johansen estimator for a Markov model and a novel non-Markovian estimator by de Uña-Álvarez and Meira-Machado (2015) <doi:10.1111/biom.12288>, see also Balboa and de Uña-Álvarez (2018) 
<doi:10.18637/jss.v083.i10>, are included.
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{TP.idm}
\packageIndices{TP.idm}

This package incorporates the function \code{TPidm} which can be used to compute a nonparametric transition probability matrix for the illness-death model. For a complete list of functions use \code{library(help=TP.idm)}.
}
\author{
\packageAuthor{TP.idm}

Maintainer: \packageMaintainer{TP.idm}
}
\references{
Andersen P. K., Borgan O., Gill R. D. and Keiding N. (1993). Statistical models based on counting processes. Springer Series in Statistics. New York, NY: Springer.

Datta S. and Satten G. A. (2001). Validity of the Aalen-Johansen estimators of stage occupation probabilities and Nelson-Aalen estimators of integrated transition hazards for non-Markov models. \emph{Statistics and Probability Letters} \bold{55(4)}, 403--411.

de Uña-Álvarez J. and Meira-Machado L. (2015). Nonparametric estimation of transition probabilities in a non-Markov illness-death model: a comparative study. \emph{Biometrics} \bold{71}, 364--375.

Meira-Machado L. F., de Uña-Álvarez J. and Cadarso-Suárez C. (2006). Nonparametric estimation of transition probabilities in a non-Markov illness-death model. \emph{Lifetime Data Annal} \bold{12(3)}, 325--344.

Balboa-Barreiro V., de Uña-Álvarez J. (2018). Estimation of Transition Probabilities for the Illness-Death Model: Package \code{TP.idm}. \emph{Journal of Statistical Software} \bold{83(10)}, 1--19.
}

\keyword{ package }
%%\seealso{
%%~~ Optional links to other man pages, e.g. ~~
%%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%%}
\examples{
data(colonTP)

# create a TPidm object with s = 0, t = "last" (default),
# CI = TRUE (default) and method = "AJ":
aj0ci <- TPidm(colonTP, s = 0, method = "AJ")
summary(aj0ci) # summarizing the results
plot(aj0ci) # plotting all occupation probabilities
}

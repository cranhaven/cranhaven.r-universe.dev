\encoding{UTF-8}
\name{TPidm}
\alias{TPidm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Transition probabilities for the illness-death model
}
\description{
  This function computes the transition probability matrix for the illness-death model, by using the Aalen-Johansen technique (suitable for Markov models) or alternatively a non-Markovian estimator. It creates a \sQuote{TPidm} object.
}
\usage{
TPidm(data, s, t = "last", cov = NULL, CI = TRUE, level = 0.95, 
ci.transformation = "linear", method = "NM")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
     A data.frame including at least four columns named \code{time1}, \code{event1}, \code{Stime} and \code{event}, which correspond to disease free survival time, disease free survival indicator, time to death or censoring, and death indicator, respectively.
}
  \item{s}{
     The current time for the transition probabilities to be computed; \code{s=0} reports the occupation probabilities.
}
  \item{t}{
     The future time for the transition probabilities to be computed. Default is \dQuote{last} which means the largest time among the uncensored entry times for the intermediate state and the final absorbing state.
}
  \item{cov}{
     A categorical variable for the optional by-group analysis; this variable must be a \code{factor}.
}
  \item{CI}{
     If \code{TRUE} (default), confidence intervals are computed.
}
  \item{level}{
     Level of confidence intervals. Default is \code{0.95} (corresponding to 95 \%).
}
  \item{ci.transformation}{
     Transformation applied to compute confidence intervals. Possible choices are \dQuote{linear}, \dQuote{log}, \dQuote{log-log} and \dQuote{cloglog}. Default is \dQuote{linear}.
}
  \item{method}{
     The method used to compute the transition probabilities. Possible options are \dQuote{AJ} (Aalen-Johansen) or \dQuote{NM} (non-Markovian). Default is \dQuote{NM}.
}
}
\details{
  If s = 0 this function TPidm reports the state occupation probabilities at time t. For s > 0, the transition probabilities are provided. The default method \dQuote{NM} computes the estimator proposed by de Uña-Álvarez and Meira-Machado (2015), which is consistent regardless the Markov assumption. To fit a Markovian transition probability matrix, use the \dQuote{AJ} method. Note that the Aalen-Johansen occupation probabilities are consistent even when the process is non-Markov (Datta and Satten, 2001), but this is not true for the Aalen-Johansen transition probabilities (Meira-Machado et al., 2006). If \code{CI} is \code{TRUE} (default), TPidm calculates the estimated variance and the confidence intervals by using the plug-in variance described in Balboa and de Uña-Álvarez (2018) (method = \dQuote{NM}), or the standard formulas for Markovian processes (method = \dQuote{AJ}, see Andersen et al., 1993).
  }
\value{
  An object of class \sQuote{TPidm}. It is a list containing the following objects:
    
  \item{s }{The user-supplied current time for the transition probabilities.}
  \item{t }{The user-supplied future time for the transition probabilities.}
  \item{method }{The method used to compute the transition probabilities.}
  \item{times }{The uncensored entry times for the intermediate state and the final absorbing state which fall between s and t.}
  \item{probs }{A matrix with transition probability estimates \eqn{P_{ij}}{P_{ij}}(s,t) for the user-supplied (s, t) and each possible transition from state i (rows) to state j (columns). If \code{CI} is \code{TRUE} (default), it includes confidence limits and variances.}
  \item{all.probs }{An array with transition probability estimates \eqn{P_{ij}}{P_{ij}}(s, \eqn{t_{k}}{t_{k}}) for each possible transition, evaluated at all the event times \eqn{t_{k}}{t_{k}} falling between s and t. If \code{CI} is \code{TRUE} (default), it includes confidence limits and variances.}
  \item{p.trans }{Possible transitions among the states. If s = 0, possible transitions are \dQuote{1 1}, \dQuote{1 2} and \dQuote{1 3}; if s > 0 possible transitions are \dQuote{1 1}, \dQuote{1 2}, \dQuote{1 3}, \dQuote{2 2} and \dQuote{2 3}.}
  \item{CI }{A logical value chosen by the user.}
  \item{ci.transformation }{Transformation chosen by the user to compute the confidence intervals.}
}
\note{
If \code{time1=Stime} and \code{event1=event=1}, the function \code{TPidm} assumes that a direct transition from the initial state to the final absorbing state has occurred. That is, zero sojourn times are not allowed for the intermediate state (replacing the zeros by a small positive amount is a way to introduce such cases).

If \code{cov} is specified, the main function provides estimations of transition probabilities for each level of the covariate. 
}
\references{
 Andersen P. K., Borgan O., Gill R. D. and Keiding N. (1993). Statistical models based on counting processes. Springer Series in Statistics. New York, NY: Springer.
 
 Datta S. and Satten G. A. (2001). Validity of the Aalen-Johansen estimators of the stage occupation probabilities and Nelson-Aalen estimators of integrated transition hazards for non-Markov models. \emph{Statistics and Probability Letters} \bold{55(4)}, 403--411.
 
 de Uña-Álvarez J. and Meira-Machado L. (2015). Nonparametric estimation of transition probabilities in a non-Markov illness-death model: a comparative study. \emph{Biometrics} \bold{71}, 364--375.
 
 Meira-Machado L. F., de Uña-Álvarez J. and Cadarso-Suárez C. (2006). Nonparametric estimation of transition probabilities in a non-Markov illness-death model. \emph{Lifetime Data Anal} \bold{12(3)}, 325--344.
 
 Balboa-Barreiro V., de Uña-Álvarez J. (2018). Estimation of Transition Probabilities for the Illness-Death Model: Package \code{TP.idm}. \emph{Journal of Statistical Software} \bold{83(10)}, 1--19.
}
%%\author{
%%  ~~who you are~~
%%}
%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
data(colonTP)

# create a TPidm object with s = 0, t = "last" (default),
# CI = TRUE (default) and method = "AJ":
aj0ci <- TPidm(colonTP, s = 0, method = "AJ")
plot(aj0ci) # plotting all occupation probabilities

#or
# create a TPidm object with s = 365, t = "last" (default),
# CI = FALSE and method = "NM" (default):
nm365 <- TPidm(colonTP, s = 365, CI=FALSE)
summary(nm365) # summarizing the results

# or
# create a TPidm object with cov = "rx":
nm365cov <- TPidm(colonTP, s = 365, t = 1095, cov = "rx", CI=FALSE)
summary(nm365cov) # summarizing the results
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ package }
\keyword{ TPidm }

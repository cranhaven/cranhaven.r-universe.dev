\name{dls}
\alias{dls}
\title{
Calculating Derivatives of Partial Likelihood for Cox Proportional Hazard Additive Models}
\description{
Calculate the log partial likelihood and derivatives with respect to the subject log hazard ratio (compared to the baseline) for Cox proportional hazard additive model described in \code{\link{gamcox}}. Results are used to update estimates in \code{\link{gamcox}} function.
}
\usage{
dls(Y,X,which,eta,span=0.5,adjust=TRUE)
}
\arguments{
  \item{Y}{
a list including two elements: \code{time} for survival times and \code{event} for censoring statu.
}
  \item{X}{
a data frame containing the variables in the model. The data must be structured so that the X and Y coordinates for two-dimensional predictor (e.g., geolocation) are in the 1st and 2nd columns, respectively. 
}
  \item{which}{
matrix index for smooth term.
}
  \item{eta}{
current estimated subject log hazard ratio compared to the baseline. 
}
  \item{span}{
smoothing parameter that been used to smoothing the second derivative of the log partial likelihood.
}
  \item{adjust}{
\code{adjust=TRUE} means there are confounders included in the model.
}
}

\details{
For data that having tied failure times, Efron's approximation method is used to calculate the log partial likelihood and correspongding derivatives. Let \eqn{\eta}{\eta} denote the log hazard ratio, and \emph{l} denote the partial likelihood. When fitting a Cox proportional hazard additive model, \eqn{\eta}{\eta} is updated by
\deqn{
\eta^{new} = \eta^{old} - \frac{dl/d{\eta}}{smooth(d^2l/d\eta^2)}
}{\eta.new = \eta.old - (dl/d\eta)/(smooth(dl^2/d\eta^2))
}
}

\value{

\item{deltaeta}{difference between the input \code{eta} and the new updated \code{eta}.}

\item{w}{inverse of smoothed second derivatives.}

\item{l}{
partial likelihood baed on input \code{eta}.
}
}

\references{
Hastie TJ, Tibshirani RJ.  Generalized Additive Models.  (Chapman & Hall/CRC Monographs on Statistics & Applied Probability, Boca Raton, Florida, 1990). 

Bristow RE, Chang J, Ziogas A, Gillen DL, Bai L, Vieira VM. Spatial Analysis of Advanced-stage Ovarian Cancer Mortality in California. American Journal of Obstetrics and Gynecology 2015, 213(1), e1-43).
}

\author{
Lu Bai and Scott Bartell

Send bug reports to \email{sbartell@uci.edu}.
}
 

\seealso{
\code{\link{gamcox}}, 
\code{\link{predict.gamcox}}.
}
\examples{
\donttest{
data(CAdata)
Y = CAdata[,c("time","event")]
X = CAdata[,c(3:5)]
eta = coxph(Surv(time,event)~AGE,data=CAdata)$linear.predictors
result = dls(Y,X,1:2,eta,span=0.2)
plot(eta,result$deltaeta)
}
}

\keyword{misc}
\keyword{smooth}

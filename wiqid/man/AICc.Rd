\name{AICc}
\alias{AICc}
\title{
Akaike's Information Criterion with small-sample correction - AICc
}
\description{
Akaike's Information Criterion with small-sample correction - AICc
}
\usage{
AICc(object, ..., nobs, df)
}

\arguments{
  \item{object}{
a fitted model object for which there exists a logLik method to extract the corresponding log-likelihood, number of parameters, and number of observations.
}
  \item{\dots}{
optionally more fitted model objects.
}
  \item{nobs}{
scalar; the value to use for the effective sample size; overrides the value contained in the model object(s).
}
  \item{df}{
the value to use for the number of parameters; usually a vector of length = number of models; non-NA elements override the value contained in the corresponding model object.
}
}
\details{
AICc is Akaike's information Criterion (AIC) with a small sample correction. It is \deqn{AICc = AIC + 2K(K + 1) / (n - K - 1)} where \emph{K} is the number of parameters and \emph{n} is the number of observations.

This is an S3 generic, with a default method which calls \code{\link{logLik}}, and should work with any class that has a \code{logLik} method.
}
\value{
If just one object is provided, the corresponding AICc.

If multiple objects are provided, a data frame with rows corresponding to the objects and columns representing the number of parameters in the model (df) and the AICc.

The result will be \code{Inf} for over-parameterised models, ie. when \code{df >= nobs - 1}.
}
\references{
Burnham, K P; D R Anderson 2002. \emph{Model selection and multimodel inference: a practical information-theoretic approach}. Springer-Verlag.
}
\author{
Essentially the same as \code{\link{AIC}} in package \code{stats}. Modified to return AICc by Mike Meredith.
}
\note{
For some data types, including occupancy data, there is debate on the appropriate effective sample size to use.
}

\seealso{
\code{\link{AIC}}.
}
\examples{
# Occupancy models
data(salamanders)
mt <- occSStime(salamanders, p ~ .time, plot=FALSE)
mT <- occSStime(salamanders, p ~ .Time, plot=FALSE)
AIC(mt, mT)
AICc(mt, mT)
# The default sample size = the number of sites
nobs(mt) == nrow(salamanders)
# It is sometimes taken to be the total number of surveys...
AICc(mt, mT, nobs=length(salamanders))
# ... or the minimum of ...
n <- min(sum(rowSums(salamanders) > 0), # sites where species was detected
         sum(rowSums(salamanders) == 0)) # sites where species was not detected
AICc(mt, mT, nobs=n)

# Survival models
data(dippers)
DH <- dippers[1:7]  # Extract the detection histories
null <- survCJS(DH)  # the phi(.) p(.) model
phit <- survCJS(DH, phi ~ .time)  # the phi(t) p(.) model
full <- survCJS(DH, list(phi ~ .time, p ~ .time))  # the phi(t) p(t) model
AICc(null, phit, full)
# for the full model, all 12 parameters cannot be estimated;
#   we can manually set df=11 for this model:
AICc(null, phit, full, df=c(NA, NA, 11))
}
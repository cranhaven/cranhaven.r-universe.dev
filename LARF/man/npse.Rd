\name{npse}
\alias{npse}

\title{Nonparametric Power Series Estimation}
\description{
Use the optimal order of power series of covariates to predict outcome. The optimal order of power series is determined by cross-validation.}
\usage{
npse(formula, order = 3, m = 10, seed = NULL)
}

\arguments{
  \item{formula}{specification of the outcome model in the form like either \code{z ~ x1 + x2} or \code{z ~ X} where \code{X} is the covariate matrix.}
  \item{order}{the maximal order of power series to be used.}
  \item{m}{the number of folds to be used in cross-validation.}
  \item{seed}{random starting number used to replicate cross-validation.}
}

\details{
This function predicts the outcome based on the optimal order of covariates power series. The optimal order of the power series is determined by cross-validation. For example, it can be used to predict the probabilty of receiving treatment inducment based on covariates.
}

\value{
\item{fitted}{Predicted outcomes based on the estimated model. They are probabilities when the outcome is binary.}
\item{Lambda}{The optimal order of power series determined by cross-validation.}
\item{Data.opt}{The data including \code{z} and the optimal covariates power series.}
\item{CV.Res}{The residual sum of squares of the cross-validations.}
\item{seed}{The random seed.}
}

\references{
Abadie, Alberto. 2003. "Semiparametric Instrumental Variable Estimation of Treatment Response Models." \emph{Journal of Econometrics} 113: 231-263.
}
\author{
Weihua An, Departments of Sociology and Statistics, Indiana University Bloomington, \email{weihuaan@indiana.edu}. \cr
Xuefu Wang, Department of Statistics, Indiana University Bloomington, \email{wangxuef@umail.iu.edu}.
}

\seealso{
\code{\link{larf}}, \code{\link{larf.fit}}
}

\examples{
data(c401k)
attach(c401k)

\dontrun{
# binary outcome
Z <- c401k$e401k

# covariates
X <- as.matrix(c401k[,c("inc", "male", "fsize"  )])

# get nonparametric power series estimation of the regression of Z on X
zp <- npse(Z~X, order = 5, m = 10, seed = 681)

# sum of residual squares of the cross-validations
zp$CV.Res

# the opitimal order of the power series
zp$Lambda

# summary of the predictions based on the optimal power series
summary(zp$fitted)
}
}

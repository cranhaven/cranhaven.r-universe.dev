\name{print}
\alias{print}
\alias{print.ilse}
\alias{print.fiml}
\title{Print the Information of FIML or ILSE methods}
\description{
  print method for class "ilse" or class "fiml".
}
\usage{
  print(object)
  \method{print}{ilse}(object)

  \method{print}{fiml}(object)

}
\arguments{
  \item{object}{an object of class "ilse" or "fiml".}
}

\value{
  For "ilse", print the basic information of ilse estimation and algorithm and return a list including

\item{beta}{a named vector of coefficients}
\item{Bmat}{a named matrix that summary the estimated beta in every iteration.}
\item{residuals}{the residuals, that is response minus fitted values.}
\item{fitted.values}{the fitted mean values.}
\item{d.fn}{a nonnegative value, vlaue of relative variation rate of objective function value}
\item{d.par}{a nonnegative value, relative variation rate of estimated parametric vector when algorithm stopped.}
\item{K}{a positive integer, iterative times in total.}

For "fiml", print the basic information of fiml estimation and return a list including

  \item{beta}{A named vector of coefficients}
  \item{iterations}{A positive integer, iterative times in total.}
  \item{stop.code}{The stop code returned by nlm.}
}
\seealso{
print.lm
}
\examples{

data(nhanes)
NAlm1 <- ilse(age~., data=nhanes)
a <- print(NAlm1)
a

fimllm <- fimlreg(age~., data=nhanes, iterlim= 40)
b <- print(fimllm)
b
}

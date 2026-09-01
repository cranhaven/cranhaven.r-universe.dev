\name{autoCode}
\alias{autoCode}
\title{Automated model code}
\description{Given a set of variable names, a model code including all the possible edges is built.}
\usage{autoCode(var.names, lag.type = "ecq")}
\arguments{
  \item{var.names}{A vector containing the names of the variables, which must be at least of length 2.}
  \item{lag.type}{The type of lag shape, which will be applied to all variables and must be one among 'ecq', 'qd', 'ld', 'gam' and 'none'. Default is 'ecq'.}
}
\value{A list of formulas to be passed as argument \code{model.code} in \code{dlsem()}.}
%\note{}
%\references{}
\seealso{\link{dlsem}.}
\examples{
autoCode(c("Job","Consum","Pollution"),lag.type="ecq")
}

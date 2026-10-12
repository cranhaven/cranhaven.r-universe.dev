\name{rsp}
\alias{rsp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Regional Supply Percentage Updating}
\description{
\code{rsp} uses the RSP technique to update the matrix of technical input coefficients \code{A} from an \code{InputOutput} object created from \code{\link{as.inputoutput}}. The function calls upon \code{\link{import.total}} and \code{\link{export.total}} to calculate the imports and exports.
}
\usage{
rsp(io)
}

\arguments{
 \item{io}{An \code{InputOutput} class object from \code{\link{as.inputoutput}}}
}
\details{
The new matrix of technical coefficients is calculated as follows:
\deqn{A_{new} = \hat{p} A}
where \eqn{\hat{p}} is a diagonal matrix with each diagonal componenet calculated as
\deqn{p_i = \frac{X_i - E_i}{X_i - E_i + M_i}}
}
\value{
\item{Anew}{The updated matrix of technical input coefficients}
}
\references{
Nazara, Suahasil & Guo, Dong & Hewings, Geoffrey J.D., & Dridi, Chokri, 2003. "PyIO. Input-Output Analysis with Python". REAL Discussion Paper 03-t-23. University of Illinois at Urbana-Champaign. (\url{http://www.real.illinois.edu/d-paper/03/03-t-23.pdf})
}
\author{
John J. P. Wade
}


\seealso{
\code{\link{import.total}}, \code{\link{export.total}} 
}
\examples{
data(toy.IO)
class(toy.IO)

Anew <- rsp(toy.IO)
}


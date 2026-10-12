\name{output.decomposition}
\alias{output.decomposition}

\title{Decomposition of Output Changes
}
\description{
Performs decomposition of output changes given two periods of data. You can decompose by origin over internal, external, or total and you can additionally decompose by changes due to final demand, technical change, or total. This follows the technique of Sonis et al (1996).
}
\usage{
output.decomposition(io1, io2, origin = "all", cause = "all")
}

\arguments{
  \item{io1}{The first period \code{InputOutput} class object from \code{\link{as.inputoutput}}}
  \item{io2}{An \code{InputOutput} class object from \code{\link{as.inputoutput}}}
  \item{origin}{Character. Choosing to decompose changes to the sectors due to \code{internal} changes, \code{external} changes, and/or \code{total}}
  \item{cause}{Character. Choosing to decompose changes to the sectors due to changes in \code{fianldemand} (\code{f}), technical changes \code{leontief} (\code{L}), or \code{total} changes}
}
\details{
A superscript of \eqn{f} indicates changes due to final demand, \eqn{l} indicates changes due to the Leontief inverse, and no superscript indicates total. A subscript of \eqn{s} indicates changes in output originating internally of the sectors, \eqn{n} indicates externally, and no subscript indicates total. \eqn{L} is the Leontief inverse and \eqn{f} is aggregated final demand. Analysis is over changes from period 1 to period 2. The values are calculated as follows:

Originating: Total
\deqn{\Delta X^f = L_1\Delta f}
\deqn{\Delta X^l = \Delta L f_1}
\deqn{\Delta X   = \Delta L \Delta f}
Originating: Internal
\deqn{\Delta X_s^f = diag(L_1)\Delta f}
\deqn{\Delta X_s^l = diag(\Delta L) f_1}
\deqn{\Delta X_s   = diag(\Delta L) \Delta f}
Originating: External
\deqn{\Delta X_n^f = \Delta X^f - \Delta X_s^f}
\deqn{\Delta X_n^l = \Delta X^l - \Delta X_s^l}
\deqn{\Delta X_n   = \Delta X   - \Delta x_s}


}
\value{
The function always outputs a named row of some variant of \code{delta.X}. A prefix indicates the changes origin where total is blank. A suffix indicates the cause of the change where total is also blank.
\item{int}{A prefix for internal}
\item{ext}{A prefix for external}
\item{f}{A suffix for final demand}
\item{L}{A suffix for technical or Leontief}
}
\references{
Nazara, Suahasil & Guo, Dong & Hewings, Geoffrey J.D., & Dridi, Chokri, 2003. \emph{PyIO. Input-Output Analysis with Python}. REAL Discussion Paper 03-t-23. University of Illinois at Urbana-Champaign. (\url{http://www.real.illinois.edu/d-paper/03/03-t-23.pdf})

Sonis, Michael & Geoffrey JD Hewings, & Jiemin Guo. \emph{Sources of structural change in input-output systems: a field of influence approach}. Economic Systems Research 8, no. 1 (1996): 15-32.
}
\author{John J. P. Wade, Ignacio Sarmiento-Barbieri}


\seealso{\code{\link{as.inputoutput}}}
\examples{
data(toy.IO)
data(toy.IO2)
class(toy.IO)
class(toy.IO) == class(toy.IO2)

OD1 <- output.decomposition(toy.IO, toy.IO2)
OD1$Hogwarts

OD2 <- output.decomposition(toy.IO, toy.IO2, origin = "external", 
                            cause = c("finaldemand","leontief"))
OD2
}

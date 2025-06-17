\name{GameTheoryAllocation-package}
\alias{GameTheoryAllocation-package}
\alias{GameTheoryAllocation}
\docType{package}
\title{
\packageTitle{GameTheoryAllocation}
}
\description{
\packageDescription{GameTheoryAllocation}
}

\author{
A. Saavedra-Nieves

Maintainer: A. Saavedra-Nieves (alejandro.saavedra.nieves@gmail.com)
}
\references{
Frisk, M., Gothe-Lundgren, M.,Jornsten, K., Ronnqvist, M. (2010). Cost allocation in collaborative
forest transportation. European Journal of Operational Research, Vol. 205,
pp. 448-458.

Gillies, D.B.  (1953). Some theorems on n-person games. PhD thesis, Princeton
University.

Owen, G. (1977). Values of games with a priori unions. Mathematical Economics and Game Theory: Essays in Honor
of Oskar Morgenstern (Eds.: O. Moeschlin
R. Hein). Springer, New York.

Shapley, L.S. (1953). A value por n-person games. In H. Kuhn y A. Tucker
(eds), Contributions to the theory of games II, Vol. 28, Annals of Mathematics
Studies. Princeton University Press.

Schmeidler, D. (1969). The nucleolus of a characteristic function game, SIAM Journal of Applied Mathematics, vol. 17, pp. 1163-1170.

}

\keyword{GameTheory}

\examples{
# Example 1

characteristic_function<-c(0,0.538, 0.761, 1.742, 0.554, 0.137, 0.293, 0.343)
isinthecore(characteristic_function,allocation=c(0.1,0.2,0.043),game="cost")
#[1] "The allocation is not in the core"
#NULL

isinthecore(characteristic_function,allocation=c(0.05,0.206,0.087),game="cost")
#[1] "The allocation is in the core"
#NULL

nucleolus(characteristic_function,game="cost")
#[1] "Nucleolus"
#      1     2 3
#  0.137 0.206 0
# Example 2

characteristic_function<-c(1,1,2,1,2,2,2)
Owen_value(characteristic_function,union=list(c(1,2),c(3)),game="cost")
#[1] "Owen Value"
#     1    2   3
#  0.25 0.25 1.5}

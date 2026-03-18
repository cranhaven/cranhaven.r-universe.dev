\name{findoptimal}
\alias{findoptimal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Chose the most likely solution of the three potential onces.
}
\description{
Starting with a 3x3 matrix, three potential haplotypes 2x2 matrices will be calculted and evaluated. The most likly one is choosen. A dicrete solution values are not enforced. This reduces increases the right prediction on real data. 
}
\usage{
findoptimal(A, B, C, D, mmorg, exact = 1e-05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
Firts entry of the 2x2 matrix.
}
  \item{B}{
Second numeric entry of teh 2x2 matrix.
}
  \item{C}{
Third numeric entry of the 2x2 matrix.
}
  \item{D}{
Last numeric entry of the 2x2 matrix.
}
  \item{mmorg}{
3x3 matrix of the pair of original snip pairs.
}
  \item{exact}{
\code{exact} is a parameter when data are assumed to be equal. Acutually 1e-5 is taken. Should not be larger than the invers of four times the number of people.
}
}
\details{
It chose the 2x2 model of haplotypes with the smallest prediction error. 
}
\value{
\item{AA}{Coefficient of x^3}
\item{BB}{Coefficient of x^2}
\item{CC}{Coefficient of x}
\item{DD}{Coefficient of the intercept}
}

\author{
Jan wolfertz
}
\examples{
dd2=matrix(c(4,0,0,0,30,0,0,0,23),ncol=3,byrow=TRUE)
A=c(38+0i,2+12.1655i,2-12.1655i)
B=c(0+0i,36-12.1655i,36+12.1655i)
C=c(0+0i,36-12.1655i,36+12.1655i)
D=c(76+0i,40+12.1655i,40-12.1655i)
}
\keyword{haplotyping}

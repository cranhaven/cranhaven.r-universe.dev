\name{ECconversion3}
\alias{ECconversion3}
\title{
A function for developing own harmonization model
}
\description{
This function enables development of own function for harmonizing soil indicators to standard values
}
\usage{
ECconversion3(x, A, B, method)
}
\arguments{
  \item{x}{
input predictor value
}
  \item{A}{
location parameter representing the value of target variable when the predictors are minimal (or the y-intercept)
}
  \item{B}{
Rate parameter representing the rate of change of the target variable with the predictor (or the slope)
}
 \item{method}{
model relationship between target and predictor variables
}
}
\details{
model for the relationship between target and predictor variables can be "linear", "power", "exponential", "log". Default is "linear"
}
\value{
model object containing predictive parameters of the conversion model
}
\references{
van Looy k,  Bouma  J, Herbst M,  Koestel J, Minasny B,  Mishra U, Montzka C,  Nemes A,  Pachepsky AY,  Padarian J,  Schaap MG,  Tóth B,  Verhoef A, Jan Vanderborght,  van der Ploeg MJ,  Weihermüller L,   Zacharias S,  Zhang Y,  Vereecken H. 2017. Pedotransfer functions in Earth System Science: Challenges and Perspectives. Reviews of Geophysics 55(4): 1199-1256.

Sudduth KA, Kitchen RN, Wiebold WJ, Batchelor W. 2005. Relating apparent electrical conductivity to soil properties across the North-Central USA. Computers and Electronics in Agriculture, 46(1-3):263-283
}
\author{
Christian Thine Omuto
}
\seealso{
\code{\link{ECconversion1}}, \code{\link{ECconversion2}}, \code{\link{ECconversion4}}
}
\examples{
x=as.vector(c(0.800,2.580,0.980,0.532,1.870, 18.500,0.430,0.302,0.345,2.700))
y=as.vector(c(17.88,  6.43,  3.83,  7.18,  6.64, 14.83,  4.19,  7.31,  3.21, 18.41))
xy=as.data.frame(cbind(x,y))
names(xy)=c("ECa", "EC")
EC3.ml=nls(EC~ECconversion3(ECa,A,B), start=c(A=0.1, B=0.8), data=xy)
cor.test(fitted(EC3.ml),xy$EC)
plot(fitted(EC3.ml)~xy$EC)
abline(0,1)
}

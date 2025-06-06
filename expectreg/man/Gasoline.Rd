\name{Gasoline}
\alias{Gasoline}
\docType{data}
\title{
Gasoline Consumption
}
\description{
A panel of 18 observations from 1960 to 1978 in OECD countries.
}
\usage{data("Gasoline")}
\format{
  A data frame with 342 observations on the following 6 variables.
  \describe{
    \item{\code{country}}{a factor with 18 levels \code{AUSTRIA} \code{BELGIUM} \code{CANADA} \code{DENMARK} \code{FRANCE} \code{GERMANY} \code{GREECE} \code{IRELAND} \code{ITALY} \code{JAPAN} \code{NETHERLA} \code{NORWAY} \code{SPAIN} \code{SWEDEN} \code{SWITZERL} \code{TURKEY} \code{U.K.} \code{U.S.A.}}
    \item{\code{year}}{the year}
    \item{\code{lgaspcar}}{logarithm of motor gasoline consumption per car}
    \item{\code{lincomep}}{logarithm of real per-capita income}
    \item{\code{lrpmg}}{logarithm of real motor gasoline price}
    \item{\code{lcarpcap}}{logarithm of the stock of cars per capita}
  }
}


\source{
Online complements to Baltagi (2001).

\url{https://www.wiley.com/legacy/wileychi/baltagi/}
}
\references{
Baltagi, Badi H. (2001) "Econometric Analysis of Panel Data", 2nd ed., John Wiley and Sons.

Gibraltar, B.H. and J.M. Griffin (1983) ???Gasoline demand in the OECD: An application of pooling and testing procedures???, European Economic Review, 22(2), 117???137.
}
\examples{
data(Gasoline)

expreg<-expectreg.ls(lrpmg~rb(lcarpcap),smooth="fixed",data=Gasoline,
lambda=20,estimate="restricted",expectiles=c(0.01,0.05,0.2,0.8,0.95,0.99))

plot(expreg)
}
\keyword{datasets}

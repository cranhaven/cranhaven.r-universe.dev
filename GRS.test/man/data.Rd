\name{data}
\alias{data}
\docType{data}
\title{
Fama-French Data: 25 size-B/M portfolio and risk factors, obtained from French's library
}
\description{
http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html
}
\usage{data("data")}
\format{
  A data frame with 630 observations on the following 32 variables.
  \describe{
    \item{\code{date}}{date}
    \item{\code{RM_RF}}{Market Excess Return}
    \item{\code{SMB}}{SMB}
    \item{\code{HML}}{HML}
    \item{\code{RMW}}{RMW}
    \item{\code{CMA}}{CMA}
    \item{\code{MOM}}{MOM}
    \item{\code{P11}}{Portfolio Returns}
    \item{\code{P12}}{Portfolio Returns}
    \item{\code{P13}}{Portfolio Returns}
    \item{\code{P14}}{Portfolio Returns}
    \item{\code{P15}}{Portfolio Returns}
    \item{\code{P21}}{Portfolio Returns}
    \item{\code{P22}}{Portfolio Returns}
    \item{\code{P23}}{Portfolio Returns}
    \item{\code{P24}}{Portfolio Returns}
    \item{\code{P25}}{Portfolio Returns}
    \item{\code{P31}}{Portfolio Returns}
    \item{\code{P32}}{Portfolio Returns}
    \item{\code{P33}}{Portfolio Returns}
    \item{\code{P34}}{Portfolio Returns}
    \item{\code{P35}}{Portfolio Returns}
    \item{\code{P41}}{Portfolio Returns}
    \item{\code{P42}}{Portfolio Returns}
    \item{\code{P43}}{Portfolio Returns}
    \item{\code{P44}}{Portfolio Returns}
    \item{\code{P45}}{Portfolio Returns}
    \item{\code{P51}}{Portfolio Returns}
    \item{\code{P52}}{Portfolio Returns}
    \item{\code{P53}}{Portfolio Returns}
    \item{\code{P54}}{Portfolio Returns}
    \item{\code{P55}}{Portfolio Returns}
  }
}
\details{
http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html
}
\source{
http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html

Monthly from 1963 to 2015
}
\references{
Fama and French, 1993, Common risk factors in the returns on stocks and bonds, Journal of Financial Economics, 33, 3-56.
<DOI:10.1016/0304-405X(93)90023-5>

Fama and French, 2015, A five-factor asset-pricing model, Journal of Financial Economics, 116-1-22.
<DOI:http://dx.doi.org/10.1016/j.jfineco.2014.10.010>

}
\examples{
data(data)
y=ts(data[,2],frequency=12,start=c(1950,1))
plot.ts(y)
}
\keyword{datasets}

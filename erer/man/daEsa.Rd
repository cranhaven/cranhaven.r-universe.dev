\name{daEsa}
\alias{daEsa}
\docType{data}
\title{Daily returns of 14 public firms from 1990 to 2004}
\description{This data set contains daily returns of 14 public firms, three-month treasury bill, and SP 500 Index from 1990 to 2004.

\tabular{ll}{
\code{date} \tab Eight-digit numbers for date of 1990-2004; the format is YYMMDD, e.g., 19900102 for Jan 2, 1990 \cr
\code{tb3m} \tab Daily returns for three-month treasury bills \cr
\code{sp500} \tab Daily returns for SP 500 Index \cr
\code{bcc} \tab Daily returns for Boise Cascade \cr
\code{bow} \tab Daily returns for Bowater \cr
\code{csk} \tab Daily returns for Chesapeake Corp VA \cr
\code{gp} \tab Daily returns for Georgia-Pacific \cr
\code{ip} \tab Daily returns for International Paper \cr
\code{kmb} \tab Daily returns for Kimberly Clark \cr
\code{lpx} \tab Daily returns for Louisiana Pacific \cr
\code{mwv} \tab Daily returns for MeadWestvaco \cr
\code{pch} \tab Daily returns for Potlatch \cr
\code{pcl} \tab Daily returns for Plum Creek \cr
\code{pop} \tab Daily returns for Pope and Talbot \cr
\code{tin} \tab Daily returns for Temple Inland \cr
\code{wpp} \tab Daily returns for Wausau Mosinee Paper \cr
\code{wy} \tab Daily returns for  Weyerhaeuser \cr
}
}
\usage{data(daEsa)}

\format{A data frame object with daily returns for firms or indexes from 1994 to 2004. There are 17 columns and 3747 rows. The date is not regular because there is no trading on weekends and holidays. Therefore, the date is represented by a number, not a date.}

\details{This is the transformated data set used in the study of Sun and Liao (2011).}

\source{The daily returns for SP 500 and individual firms are from the database of the Center for Research in Security Prices (CRSP). The risk-free rate of return is the secondary market rate for the 3-month US Treasury bills from the Federal Reserve Bank.}

\references{Sun, C., and X. Liao. 2011. Effects of litigation under the Endangered Species Act on forest firm values. Journal of Forest Economics 17(4):388-398.}

\seealso{\code{\link{evReturn}}; \code{\link{evRisk}}.}

\examples{
data(daEsa) 
dim(daEsa); colnames(daEsa)
head(daEsa); tail(daEsa)
str(daEsa)

# if dates are stored as a date object in R, then it can be converted into
# numbers as the following example shows.

raw <- as.Date(c('1990-01-02', '1991-11-12')); raw; str(raw)
raw2 <- as.numeric(strftime(raw, format = "\%Y\%m\%d")); raw2; str(raw2)

}
\keyword{datasets}
\encoding{UTF-8}
\name{marketsUS}
\alias{marketsUS}
\title{NASDAQ and NYSE Indices}
\description{
  Daily quotations at close of the NASDAQ and NYSE stock market indices
  from February 1971 till November 2021.
}
\usage{marketsUS}
\format{
  The \code{marketsUS} data frame has 12562 rows and 3 columns: \code{date}
  and quotation at close of the \code{nasdaq} and \code{nyse} indices.
}
\references{
  de Carvalho, M., Kumukova, A. and dos Reis, G. (2022) Regression-type analysis
  for multivariate extreme values. Submitted.    
}
\examples{
\dontrun{
## de Carvalho et al (2022; Fig 5.1)
data(marketsUS)
packages <- c("scales", "ggplot2")
sapply(packages, require, character.only = TRUE)
ggplot(data = marketsUS, aes(x = date, y = value, color = Indices)) + 
  geom_line(aes(y = nasdaq, col = "NASDAQ"), alpha = 0.5,
                position = position_dodge(0.8), size = 1.1) +
  geom_line(aes(y = nyse, col = "NYSE"), alpha = 0.5,
            position = position_dodge(0.8), size = 1.1) + 
  scale_y_continuous(breaks = seq(2000, 14000, by = 2000)) + 
  scale_x_date(labels = date_format("\%Y"), 
               breaks = as.Date(c("1971-01-01", "1978-01-01",
                                  "1985-01-01", "1992-01-01",
                                  "1999-01-01", "2006-01-01",
                                  "2013-01-01", "2020-01-01"))) + 
  scale_color_manual(values = c("red", "blue")) +
  labs(y = "Value (in USD)", x = "Time (in Years)")
}
}
\keyword{Financial}

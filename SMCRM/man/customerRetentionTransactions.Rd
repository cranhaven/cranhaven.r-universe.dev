\docType{data}
\name{customerRetentionTransactions}
\alias{customerRetentionTransactions}
\title{Transactions Data for Customer Retention (Chapter 4)}
\format{Data frame with the following 7 variables
\describe{
  \item{\code{customer}}{customer number (from 1 to 500)}
  \item{\code{quarter}}{quarter (from 1 to 12) where the transactions occurred}
  \item{\code{purchase}}{1 when the customer purchased in the given quarter and 0 if no purchase occurred in that quarter}
  \item{\code{order_quantity}}{dollar value of the purchases in the given quarter}
  \item{\code{crossby}}{number of different categories purchased in a given quarter}
  \item{\code{ret_expense}}{dollars spent on marketing efforts to try and retain that customer in the given quarter}
  \item{\code{ret_expense_sq}}{square of dollars spent on marketing efforts to try and retain that customer in the given quarter}
}}
\usage{
  customerRetentionTransactions
}
\description{
  Transactions Data for Customer Retention (Chapter 4)
}
\examples{
data(customerRetentionTransactions)
str(customerRetentionTransactions)
}
\keyword{datasets}


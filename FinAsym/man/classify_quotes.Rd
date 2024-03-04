\name{classify_quotes}
\alias{classify_quotes}

\title{
Classification of implicit trade origination from market quotes
}
\description{
The information on trade origination (i.e.g buy- or sell-initiated) is available for a variety of markets and trading platforms. However, data on effective trades for over-the-counter - OTC - markets are hard to find and, when available, they may be unreliable. Instead, for OTC markets, quotes are typically easily accessible. This R function is used to classify implicit trading activity from market quotes. The classification method is standard in the literature on market microstructure and relies on Lee and Ready (1991).
}
\usage{
classify_quotes(returns, ind_bid, ind_ask, trading_day)
}

\arguments{
  \item{returns}{
Numerical matrix of intra-daily market quotes organised by continuous and consecutive time stamps either for a given trading day, or for round-the-clock trading activities. In the second case, no indication of the trading day is necessary. This matrix has a row dimension equal to the number of intra-daily quotes and a column dimension equal to 2 - one for the best bid prices and the other for the best ask prices available for each time stamp.
}
  \item{ind_bid}{
Column number for bid prices in the matrix `returns'. 
}
  \item{ind_ask}{
Column number for ask prices in the matrix `returns'. 
}
  \item{trading_day}{
String indicating the trading day to which the intra-daily quotes refer.
}
}
\details{
The code considers five possible cases for trade classification. An increase in both ask and bid prices indicates that dealers are willing to sell the contract at a price higher than in the previous transaction. This reflects a buy order. A fall in both ask and bid prices, instead, suggest that a sell order takes place. An increase in the ask and a decrease in the bid is classified according to the relative size of the change. If the positive increase in the ask is larger (lower) than the decrease of the bid in absolute value, then the trade is classified as buy (sell) initiated. If the positive increase in the bid is larger (smaller) than the fall in the ask price, then the trade is classified as buy (sell) initiated. Finally, symmetric changes or no changes at all in bid and ask prices indicate no trading.

Two additional caveats apply. This implementation of the Lee-Ready algorithm admits no missing values for intra-daily quotes. Therefore the user should clean up the dataset and remove any missing values. Moreover, the quotes should follow a chronological order.
}
\value{
\item{no_trades }{Number of implicit trades with no sign classification.}
\item{sell_trades }{Number of sell-initiated trades.}
\item{buy_trades }{Number of buy-initiated trades.}
}
\references{
C. M. C. Lee and M. J. Ready. Inferring Trade Direction from Intraday Data. Journal of Finance, 46(2), 733-746, 1991.
}
\author{
Paolo Zagaglia <paolo.zagaglia@gmail.com>
}


\examples{
#Sample data
mydata<-matrix(c(4.56, 4.7, 4.57, 4.64, 4.53, 
4.65, 4.59, 4.66, 4.55, 4.65, 4.59, 4.66, 4.59, 
4.66, 4.55, 4.65, 4.55, 4.65, 4.55, 4.65, 4.59, 
4.66, 4.55, 4.65, 4.59, 4.66, 4.59, 4.66), 
nrow=14, byrow=TRUE)

#Trading day
gname <- 'June 29 2006'
	
ind_bid <- c(1)
ind_ask <- c(2)
	
quotes_types <- classify_quotes(mydata, ind_bid, 
ind_ask, gname)
}

\keyword{ classify_quotes }

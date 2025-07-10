## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = TRUE, fig.height = 4, fig.width = 7, fig.align = "center"---------
utils::data(nhtemp)
graphics::plot(nhtemp)

## ----echo = TRUE--------------------------------------------------------------
strucchange::sctest(strucchange::efp(nhtemp ~ 1, type = "OLS-CUSUM"))

## ----echo = TRUE, fig.height = 4, fig.width = 7, fig.align = "center"---------
T            <- 1000
series_short <- fracdiff::fracdiff.sim(n=T,d=0)$series
series_long  <- fracdiff::fracdiff.sim(n=T,d=0.45)$series

graphics::par(mfrow=c(1,2))
stats::acf(series_short,main="short memory")
stats::acf(series_long,main="long memory")

## ----echo = TRUE--------------------------------------------------------------
set.seed(410)
T                  <- 500
N                  <- 500
results_short      <- vector("numeric",N)
results_long       <- vector("numeric",N)
for(i in 1:N)
{
  series_short     <- fracdiff::fracdiff.sim(n=T,d=0)$series
  series_long      <- fracdiff::fracdiff.sim(n=T,d=0.45)$series
  results_short[i] <- strucchange::sctest(strucchange::efp(series_short ~ 1, type = "OLS-CUSUM"))$p.value<0.05
  results_long[i]  <- strucchange::sctest(strucchange::efp(series_long ~ 1, type = "OLS-CUSUM"))$p.value<0.05
}
mean(results_short)
mean(results_long)

## ----echo = TRUE--------------------------------------------------------------
nasdaq=data.table::fread("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=NASDAQCOM&scale=left&cosd=2006-01-01&coed=2009-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Daily&fam=avg&fgst=lin&fgsnd=2009-06-01&line_index=1&transformation=lin&vintage_date=2019-11-04&revision_date=2019-11-04&nd=1971-02-05")

## ----echo = TRUE, fig.height = 4, fig.width = 7, fig.align = "center"---------
nasdaq$NASDAQCOM    <- ifelse(nasdaq$NASDAQCOM == ".", NA, nasdaq$NASDAQCOM)
nasdaq              <- as.data.frame(nasdaq)
nasdaq              <- stats::na.omit(nasdaq)
nasdaq$NASDAQCOM    <- as.numeric(nasdaq$NASDAQCOM)
nasdaq$observation_date=zoo::as.Date(nasdaq$observation_date)
nasdaq_xts=xts::xts(nasdaq[,-1],order.by = nasdaq$observation_date)
nasdaq_xts          <- log(diff(nasdaq_xts)^2)[-1]
graphics::par(mfrow=c(1,1))
zoo::plot.zoo(nasdaq_xts, xlab="", ylab="Log squared returns", main="Log squared returns of the NASDAQ")

## ----echo = TRUE--------------------------------------------------------------
T           <- length(nasdaq_xts)
x           <- as.numeric(nasdaq_xts)
d_est       <- LongMemoryTS::local.W(x, m=floor(1+T^0.65))$d
round(d_est,3)

## ----echo = TRUE--------------------------------------------------------------
library(memochange)
CUSUMfixed(x,d=d_est,procedure="CUSUMfixedm_typeA",bandw=10)

## ----echo = TRUE--------------------------------------------------------------
BP       <- strucchange::breakpoints(x~1)$breakpoints
BP_index <- zoo::index(nasdaq_xts[BP])
BP_index

## ----echo = TRUE, fig.height = 4, fig.width = 7, fig.align = "center"---------
T_index  <- zoo::index(nasdaq_xts[T])
m1       <- mean(nasdaq_xts[1:BP])
m2       <- mean(nasdaq_xts[(BP+1):T])
zoo::plot.zoo(nasdaq_xts, xlab="", ylab="Log squared returns", main="Log squared returns of the NASDAQ")
graphics::segments(0,m1,BP_index,m1,col=2,lwd=2)
graphics::segments((BP_index+1),m2,T_index,m2,col=2,lwd=2)

## ----echo = TRUE, eval=FALSE--------------------------------------------------
# 
# test_func<-function(T,d)
# {
#   # Simulate a fractionally integrated (long-memory) time series of
#   # length T with memory d that is not subject to a shift.
#   tseries     <- fracdiff::fracdiff.sim(n=T,d=d)$series
# 
#   # Simulate a fractionally integrated (long-memory) time series of
#   # length T with memory d that is subject to a shift in the middle of
#   # the sample of magnitude 2.
#   changep     <- c(rep(0,T/2),rep(2,T/2))
#   tseries2    <- tseries+changep
# 
#   # Estimate the long-memory parameter of both series using the suggested bandwidth.
#   d_est       <- LongMemoryTS::local.W(tseries, m=floor(1+T^0.65))$d
#   d_est2      <- LongMemoryTS::local.W(tseries2, m=floor(1+T^0.65))$d
# 
#   # Apply both functions on both time series. Arguments are chosen according to
#   # Wenger, Leschinski (2019) who propose these tests.
#   typeAsize   <- CUSUMfixed(tseries,d=d_est,procedure="CUSUMfixedm_typeA",bandw=10)
#   typeBsize   <- CUSUMfixed(tseries,d=d_est,procedure="CUSUMfixedm_typeB",bandw=10)
#   typeApower  <- CUSUMfixed(tseries2,d=d_est2,procedure="CUSUMfixedm_typeA",bandw=10)
#   typeBpower  <- CUSUMfixed(tseries2,d=d_est2,procedure="CUSUMfixedm_typeB",bandw=10)
# 
#   # Save if the tests reject at the 5% significance level.
#   decAsize    <- typeAsize["Teststatistic"] > typeAsize["95%"]
#   decBsize    <- typeBsize["Teststatistic"] > typeBsize["95%"]
#   decApower   <- typeApower["Teststatistic"] > typeApower["95%"]
#   decBpower   <- typeBpower["Teststatistic"] > typeBpower["95%"]
# 
#   return(c(decAsize,decBsize,decApower,decBpower))
# }

## ----echo = TRUE, eval=FALSE--------------------------------------------------
# set.seed(410)
# # Parameter setting considered
# T_grid              <- c(50,100)
# d_grid              <- c(0.1,0.2)
# N                   <- 500
# 
# # Generate array to save the results
# resultmat           <- array(NA, dim=c(length(T_grid),length(d_grid),4))
# dimnames(resultmat) <- list(paste("T=",T_grid,sep=""),paste("d=",d_grid,sep=""),
#                             paste(rep(c("type-A","type-B"),2),c("size","size","power","power"),sep=" "))
# 
# # Monte Carlo simulation
# for(TTT in 1:length(T_grid))
# {
#   T <- T_grid[TTT]
#   for(ddd in 1:length(d_grid))
#   {
#     d                 <- d_grid[ddd]
#     result_vec        <- 0
#     for(i in 1:N)
#     {
#     result_vec        <- result_vec+test_func(T,d)
#     }
#   resultmat[TTT,ddd,] <- result_vec/N
#   }
# }
# # Results
# resultmat
# #> , , type-A size
# #>
# #>       d=0.1 d=0.2
# #> T=50  0.020 0.032
# #> T=100 0.026 0.046
# #>
# #> , , type-B size
# #>
# #>       d=0.1 d=0.2
# #> T=50  0.016 0.028
# #> T=100 0.036 0.046
# #>
# #> , , type-A power
# #>
# #>       d=0.1 d=0.2
# #> T=50   0.86 0.824
# #> T=100  1.00 0.960
# #>
# #> , , type-B power
# #>
# #>       d=0.1 d=0.2
# #> T=50  0.830 0.770
# #> T=100 0.998 0.938


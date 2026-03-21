# RTTWebClient
This package works with WebAPi of TickTrader Server. 

# Prerequisites
This package use httr, jsonlite, data.table r libraries. Please install them before using.

# How to install RTTWebClient?
```
if(!require(devtools)) {install.packages("devtools"); library(devtools)}
if(require(RTTWebClient)) {detach("package:RTTWebClient", unload=TRUE); remove.packages("RTTWebClient")}
install_github("SoftFx/TTWebClient-R")	 

```

# Examples
 You can use function ttInitialize to set server name and port:
1) cryptottlivewebapi.fxopen.net - TT Exchange FXOPEN WebAPI
2) ttlivewebapi.fxopen.com - TT Margin Live WebAPI

Port is 8443 as default

To get Quotes, Bars, etc. info you should call function from R list object which was created by InitRTTWebApiHost with the appropriate parameters. 
(if you don't set HMAC parameters the public connect will be used).
If you don't set any parameters connection to ttlivewebapi.fxopen.com will be requested.

```
library(RTTWebClient)
options(scipen = 999)
publicClient <- InitRTTWebApiHost(server = "ttlivewebapi.fxopen.com")  #for public interface
#or use (just for example) for private interface 
#privateClient <- InitRTTWebApiHost(server = "ttlivewebapi.fxopen.com", id = rfdfddfsdfs, key = fdfsdfs, secret = fdfsdfsdf) 
symbols <- publicClient$GetSymbolsInfo()
pipsValue <- publicClient$GetPipsValue("USD", c("EURUSD", "USDCAD"))
bars <- publicClient$GetBarsHistory("EURUSD", "Bid", "M1", Sys.time(), Sys.time(), -1)
ticks <- publicClient$GetTickHistory("EURUSD", Sys.time(), Sys.time(), -1)
currentQuotes <- publicClient$GetCurrentQuotes()
dividends <- publicClient$GetDividends()
currency <- publicClient$GetCurrencyInfo()
```

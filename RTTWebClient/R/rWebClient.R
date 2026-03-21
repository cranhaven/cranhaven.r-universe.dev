GetBars <- function(GetBarMethod, symbol, barsType = "Bid", periodicity = "M1", startTime, endTime, count) {
  tempStartTime <- as.double(startTime)*1000
  history <- data.table("Volume"= numeric(),
                        "Close" = numeric(), "Low"= numeric(),"High"= numeric(), "Open"= numeric(),
                        "Timestamp"= numeric())
  maxCount <- 1000
  if(count == 0) {
    repeat{
      bars <- Throttling(GetBarMethod, symbol, barsType, periodicity, tempStartTime, maxCount)
      lastHistoryNoteTimestamp <- history[.N, Timestamp]
      excludeIndex <-ifelse(length(lastHistoryNoteTimestamp) <= 0, numeric(0), bars[Timestamp==lastHistoryNoteTimestamp,which=T])
      if(!is.na(excludeIndex))
        bars <-bars[-excludeIndex]
      if(nrow(bars) == 0) {
        break;
      }else{
        endTimeInMs <- as.double(endTime) * 1000
        if(bars[.N, Timestamp] >= endTimeInMs){
          history = rbind(history, bars[Timestamp <= endTimeInMs])
          break;
        }
      }
      history <- rbind(history, bars)
      tempStartTime <- history[.N, Timestamp]
    }
  }else{
    if(abs(count) < maxCount){
      history <- Throttling(GetBarMethod, symbol, barsType, periodicity, tempStartTime, count)
    }else{
      history <- Throttling(GetBarMethod, symbol, barsType, periodicity, tempStartTime, maxCount * sign(count))
    }
  }
  history[, Timestamp := as.POSIXct(Timestamp / 1000, origin = "1970-01-01", tz = "GMT")]
  setkey(history, Timestamp)
  setcolorder(history, c("Timestamp", "Open", "Low", "High", "Close", "Volume"))
  return(history)
}

GetTicks <- function(GetTickMethod, symbol, startTime, endTime, count) {
  tempStartTime <- as.double(startTime)*1000
  history <- data.table("Timestamp" = numeric(),
                        "BidPrice" = numeric() , "BidVolume" = numeric(),
                        "BidType" = character(), "AskPrice" = numeric(),
                        "AskVolume" = numeric(),  "AskType" = character())
  maxCount <- 1000
  if(count == 0) {
    repeat{
      ticks <- Throttling(GetTickMethod, symbol, tempStartTime, maxCount)
      lastHistoryNoteTimestamp <- history[.N, Timestamp]
      excludeIndex <-ifelse(length(lastHistoryNoteTimestamp) <= 0, numeric(0), ticks[Timestamp==lastHistoryNoteTimestamp, which=TRUE])
      if(!is.na(excludeIndex))
        ticks <-ticks[-excludeIndex]
      if(nrow(ticks) == 0) {
        break;
      }else{
        endTimeInMs <- as.double(endTime) * 1000
        if(ticks[.N, Timestamp] >= endTimeInMs){
          history = rbind(history, ticks[Timestamp <= endTimeInMs])
          break;
        }
      }
      history <- rbind(history, ticks)
      tempStartTime <- history[.N, Timestamp]
    }
  }else{
    if(abs(count) < maxCount){
      history <- Throttling(GetTickMethod, symbol, tempStartTime, count)
    }else{
      history <- Throttling(GetTickMethod, symbol, tempStartTime, maxCount * sign(count))
    }
  }
  history[, Timestamp := as.POSIXct(Timestamp / 1000, origin = "1970-01-01", tz = "GMT")]
  setkey(history, Timestamp)
  return(history)
}

Throttling <- function(func, ..., N = 10, sleepTimeSec = 1, errorPattern = "429"){
  res <- NULL
  func <- match.fun(func)
  for(i in 1:N){
    res <- tryCatch({
      return(func(...))
      # return(stop(errorPattern))
    }, error = function(e){
      print(paste( i, "-", e$message))
      if(!grepl(errorPattern, e$message, fixed = TRUE))
        stop(e$message)
      print(paste("Sleeping", sleepTimeSec, "sec"))
      Sys.sleep(sleepTimeSec)
    })
  }
  if(length(res) == 0)
    stop(paste("More than", N, "attempts with errors matchs the pattern", errorPattern))
  return(res)
}

# GetBidAskBar <- function(GetBarMethod, symbol, periodicity = "M1", startTime, endTime, count) {
#   bidBars <- GetBars(GetBarMethod, symbol, barsType = "Bid", periodicity, startTime, endTime, count)
#   askBars <- GetBars(GetBarMethod, symbol, barsType = "Ask", periodicity, startTime, endTime, count)
#   bidAskBars <- merge(bidBars, askBars)
#   colnames(bidAskBars) <- c("Timestamp", "BidVolume", "BidClose", "BidLow", "BidHigh", "BidOpen",
#                             "AskVolume", "AskClose", "AskLow", "AskHigh", "AskOpen")
#   return(bidAskBars)
# }

# Get current time in ms
getTimestamp = function() {
  return(round(as.double(Sys.time()) * 1000))
}

# Generate HMAC Headers from
getHMACHeaders = function(url, id, key, secret, method = "GET", body = "") {
  timestamp1 <- getTimestamp()
  signature <- paste0(timestamp1, id, key, method, url, body)
  hash_value <- base64enc::base64encode(hmac(secret, signature, algo = "sha256", raw = TRUE))
  auth_value <- paste("HMAC",paste(id, key, timestamp1, hash_value, sep = ":"))
  return(auth_value)
}

#RTTWebClient Class
#@name RTTWebClient
#@field web_api_address. Server address. Character
#@field web_api_port. Port. Integer
#@field web_api_id. Web Api Id. Character
#@field web_api_key. Web Api Key. Character
#@field web_api_secrer. Web Api Secret. Character
#' @import data.table
#' @import jsonlite
#' @import httr
#' @import digest
#' @import base64enc
RTTWebClient <- setRefClass("RTTWebClient",
                            fields = list(web_api_address = "character",
                                          web_api_port = "integer",
                                          web_api_id = "character",
                                          web_api_key = "character",
                                          web_api_secret= "character")
)


#Get All Dividend
#@name GetDividendsRawMethod
#@return a data.table with dividends.
RTTWebClient$methods(
  GetDividendsRawMethod = function() {
    #"Get All Dividend"
    address <- .self$web_api_address
    if(!grepl("^https://", address))
      address <- paste0("https://", address)

    portPattern <- paste0(":", .self$web_api_port, "$")
    if(!grepl(portPattern, address))
      address <- paste0(address, ":", .self$web_api_port)

    if(length(.self$web_api_id) != 0 && length(.self$web_api_key) != 0 && length(.self$web_api_secret) != 0){
      url_rel <- paste("/api/v2/dividend")
      url_abs <- paste0(address, url_rel)
      connect <- httr::GET(url_abs, httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L, verbose = FALSE),
                           httr::add_headers(Authorization = getHMACHeaders(url_abs, .self$web_api_id, .self$web_api_key, .self$web_api_secret)))
    }else{
      url_rel <- paste("/api/v2/public/dividend")
      url_abs <- paste0(address, url_rel)
      connect <- httr::GET(url_abs, httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L, verbose = FALSE))
    }
    data <- httr::content(connect, "text", encoding = "UTF-8")
    if(connect$status_code != 200) {
      stop(paste("status_code is not OK", connect$status_code, as.character(data)))
    }
    dividends <- fromJSON(data)
    dividends <- as.data.table(dividends)
    return(dividends)
  }
)

# #' Get All Current Quotes
# #' @name GetCurrentQuotesRawMethod
# #' @return a data.table with current quotes
RTTWebClient$methods(
  GetCurrentQuotesRawMethod = function() {
    "Get All Current Quotes"
    address <- .self$web_api_address
    if(!grepl("^https://", address))
      address <- paste0("https://", address)

    portPattern <- paste0(":", .self$web_api_port, "$")
    if(!grepl(portPattern, address))
      address <- paste0(address, ":", .self$web_api_port)
    if(length(.self$web_api_id) != 0 && length(.self$web_api_key) != 0 && length(.self$web_api_secret) != 0){
      url_rel <- paste("/api/v2/tick")
      url_abs <- paste0(address, url_rel)
      connect <- httr::GET(url_abs, httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L, verbose = FALSE),
                           httr::add_headers(Authorization = getHMACHeaders(url_abs, .self$web_api_id, .self$web_api_key, .self$web_api_secret)))
    }else{
      url_rel <- paste("/api/v2/public/tick")
      url_abs <- paste0(address, url_rel)
      connect <- httr::GET(url_abs, httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L, verbose = FALSE))

    }
    data <- httr::content(connect, "text", encoding = "UTF-8")
    if(connect$status_code != 200) {
      stop(paste("status_code is not OK", connect$status_code, as.character(data)))
    }
    # data <- content(connect, "text", encoding = "UTF-8")
    ticks <- fromJSON(data)
    ticks <- data.table("Timestamp" = ticks$Timestamp, "Symbol" = ticks$Symbol, "BidPrice" = ticks$BestBid$Price,
                        "BidVolume" = ticks$BestBid$Volume, "BidType" = ticks$BestBid$Type,  "AskPrice" = ticks$BestAsk$Price,
                        "AskVolume" = ticks$BestAsk$Volume, "AskType" = ticks$BestAsk$Type)
    return(ticks)
  }
)

# #' Get All Current Quotes
# #' @name GetPipsValueRawMethod
# #' @return a data.table with current quotes
RTTWebClient$methods(
  GetPipsValueRawMethod = function(targetCurrency, symbols) {
    "Get Pip Value"
    address <- .self$web_api_address
    if(!grepl("^https://", address))
      address <- paste0("https://", address)

    portPattern <- paste0(":", .self$web_api_port, "$")
    if(!grepl(portPattern, address))
      address <- paste0(address, ":", .self$web_api_port)
    if(length(.self$web_api_id) != 0 && length(.self$web_api_key) != 0 && length(.self$web_api_secret) != 0){
      url_rel <- paste("/api/v2/pipsvalue")
      url_rel <- paste0(url_rel,"?","targetCurrency=",targetCurrency,"&symbols=", symbols)
      # url_abs <- utils::URLencode(paste0(address, url_rel), reserved = FALSE)
      url_abs <- paste0(address, url_rel)
      connect <- httr::GET(url_abs, httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L, verbose = FALSE),
                           httr::add_headers(Authorization = getHMACHeaders(url_abs, .self$web_api_id, .self$web_api_key, .self$web_api_secret)))
    }else{
      url_rel <- paste("/api/v2/public/pipsvalue")
      url_rel <- paste0(url_rel,"?","targetCurrency=",targetCurrency,"&symbols=", symbols)
      # url_abs <- utils::URLencode(paste0(address, url_rel), reserved = FALSE)
      url_abs <- paste0(address, url_rel)
      connect <- httr::GET(url_abs, httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L, verbose = FALSE))

    }
    data <- httr::content(connect, "text", encoding = "UTF-8")
    if(connect$status_code != 200) {
      stop(paste("status_code is not OK", connect$status_code, as.character(data)))
    }
    # data <- content(connect, "text", encoding = "UTF-8")
    pipsValue <- as.data.table(fromJSON(data))
    return(pipsValue)
  }
)

# #' Get All Symbols
# #' @name GetSymbolsInfoRawMethod
# #' @return data.table with symbol info
RTTWebClient$methods(
  GetSymbolsInfoRawMethod = function(){
    "Get All Symbols"
    address <- .self$web_api_address
    if(!grepl("^https://", address))
      address <- paste0("https://", address)
    portPattern <- paste0(":", .self$web_api_port, "$")
    if(!grepl(portPattern, address))
      address <- paste0(address, ":", .self$web_api_port)
    if(length(.self$web_api_id) != 0 && length(.self$web_api_key) != 0 && length(.self$web_api_secret) != 0){
      url_rel <- paste("/api/v2/symbol")
      url_abs <- paste0(address, url_rel)
      connect <- httr::GET(url_abs, httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L, verbose = FALSE),
                           httr::add_headers(Authorization = getHMACHeaders(url_abs, .self$web_api_id, .self$web_api_key, .self$web_api_secret)))
    }else{
      url_rel <- paste("/api/v2/public/symbol")
      url_abs <- paste0(address, url_rel)
      connect <- httr::GET(url_abs, httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L, verbose = FALSE))

    }
    data <- httr::content(connect, "text", encoding = "UTF-8")
    if(connect$status_code != 200) {
      stop(paste("status_code is not OK", connect$status_code, as.character(data)))
    }
    data = fromJSON(data)
    symbols <- as.data.table(data)
    setkey(symbols, "Symbol")
    return(symbols)
  }
)

# #' Get All Currency Info
# #' @name GetCurrencyInfoRawMethod
# #' @return data.table with currency info
RTTWebClient$methods(
  GetCurrencyInfoRawMethod = function(){
    "Get All Symbols"
    address <- .self$web_api_address
    if(!grepl("^https://", address))
      address <- paste0("https://", address)
    portPattern <- paste0(":", .self$web_api_port, "$")
    if(!grepl(portPattern, address))
      address <- paste0(address, ":", .self$web_api_port)
    if(length(.self$web_api_id) != 0 && length(.self$web_api_key) != 0 && length(.self$web_api_secret) != 0){
      url_rel <- paste("/api/v2/currency")
      url_abs <- paste0(address, url_rel)
      connect <- httr::GET(url_abs, httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L, verbose = FALSE),
                           httr::add_headers(Authorization = getHMACHeaders(url_abs, .self$web_api_id, .self$web_api_key, .self$web_api_secret)))
    }else{
      url_rel <- paste("/api/v2/public/currency")
      url_abs <- paste0(address, url_rel)
      connect <- httr::GET(url_abs, httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L, verbose = FALSE))

    }
    data <- httr::content(connect, "text", encoding = "UTF-8")
    if(connect$status_code != 200) {
      stop(paste("status_code is not OK", connect$status_code, as.character(data)))
    }
    data = fromJSON(data)
    currency <- as.data.table(data)
    setkey(currency, "Name")
    return(currency)
  }
)

# #' Get Bar History
# #' @name GetBarRawMethod
# #' @param symbol a character. Symbol Name.
# #' @param barsType. a character. Bars Type. One from c("Ask", "Bid").
# #' @param periodicity. a character. Periodicity. From c("S1", "S10", "M1", "M5", "M15", "M30", "H1", "H4", "D1", "W1","MN1")
# #' @param startTimeMs. Long numeric. Timestamp from 1970-01-01 in ms.
# #' @param count. Integer. Count of returned Bars from startTimeMs. Max is 1000. Can be negative.
# #' @return data.table with Bar Info
RTTWebClient$methods(
  GetBarRawMethod = function(symbol, barsType, periodicity, startTimeMs, count){
    "Get Bar History"

    # nonScienceFormat <- options(scipen = 999)
    # on.exit(options(nonScienceFormat))
    withr::local_options(list(scipen = 999))
    address <- .self$web_api_address
    if(!grepl("^https://", address))
      address <- paste0("https://", address)
    portPattern <- paste0(":", .self$web_api_port, "$")
    if(!grepl(portPattern, address))
      address <- paste0(address, ":", .self$web_api_port)
    if(length(.self$web_api_id) != 0 && length(.self$web_api_key) != 0 && length(.self$web_api_secret) != 0){
      url_rel <- paste("/api/v2/quotehistory",symbol, periodicity, "bars", barsType, sep= "/")
      url_rel <- paste0(url_rel,"?","timestamp=",round(startTimeMs, 0),"&count=", count)
      url_abs <- paste0(address, url_rel)
      connect <- httr::GET(url_abs, httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L, verbose = FALSE),
                           httr::add_headers(Authorization = getHMACHeaders(url_abs, .self$web_api_id, .self$web_api_key, .self$web_api_secret)))
    }else{
      url_rel <- paste("/api/v2/public/quotehistory",symbol, periodicity, "bars", barsType, sep= "/")
      url_rel <- paste0(url_rel,"?","timestamp=",round(startTimeMs, 0),"&count=", count)
      url_abs <- paste0(address, url_rel)
      connect <- httr::GET(url_abs, httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L, verbose = FALSE))
    }
    data <- httr::content(connect, "text", encoding = "UTF-8")
    if(connect$status_code != 200) {
      stop(paste("status_code is not OK", connect$status_code, as.character(data)))
    }
    bars <- fromJSON(data)
    if(length(bars$Bars) < 1)
      return(data.table("Volume"= numeric(),
                 "Close" = numeric(), "Low"= numeric(),"High"= numeric(), "Open"= numeric(),
                 "Timestamp"= numeric()))
    return(as.data.table(bars$Bars))
  }
)

# #'Get Ticks History
# #' @name GetTicksRawMethod
# #' @param symbol. A character. Symbol Name.
# #' @param startTimeMs. Long numeric. Timestamp from 1970-01-01 in ms.
# #' @param count. Integer. Count of returned Bars from startTimeMs. Max is 1000. Can be negative.
# #' @return data.table with Ticks Info.
RTTWebClient$methods(
  GetTicksRawMethod = function(symbol, startTimeMs, count){
    "Get Ticks History"

    # nonScienceFormat <- options(scipen = 999)
    # on.exit(options(nonScienceFormat))
    withr::local_options(list(scipen = 999))
    address <- .self$web_api_address
    if(!grepl("^https://", address))
      address <- paste0("https://", address)

    portPattern <- paste0(":", .self$web_api_port, "$")
    if(!grepl(portPattern, address))
      address <- paste0(address, ":", .self$.self$web_api_port)
    if(length(.self$web_api_id) != 0 && length(.self$web_api_key) != 0 && length(.self$web_api_secret) != 0){
      url_rel <- paste("/api/v2/quotehistory",symbol, "ticks", sep= "/")
      url_rel <- paste0(url_rel,"?","timestamp=",round(startTimeMs, 0),"&count=", count)
      url_abs <- paste0(address, url_rel)
      connect <- httr::GET(url_abs, httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L, verbose = FALSE),
                           httr::add_headers(Authorization = getHMACHeaders(url_abs, .self$web_api_id, .self$web_api_key, .self$web_api_secret)))
    }else{
      url_rel <- paste("/api/v2/public/quotehistory",symbol, "ticks", sep= "/")
      url_rel <- paste0(url_rel,"?","timestamp=",round(startTimeMs, 0),"&count=", count)
      url_abs <- paste0(address, url_rel)
      connect <- httr::GET(url_abs, httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L, verbose = FALSE))
    }
    data <- httr::content(connect, "text", encoding = "UTF-8")
    if(connect$status_code != 200) {
      stop(paste("status_code is not OK", connect$status_code, as.character(data)))
    }
    ticks <- fromJSON(data)
    ticks <- ticks$Ticks
    if(length(ticks) < 1)
      return(data.table("Timestamp" = numeric(),
                        "BidPrice" = numeric() , "BidVolume" = numeric(),
                        "BidType" = character(), "AskPrice" = numeric(),
                        "AskVolume" = numeric(),  "AskType" = character()))
    return(data.table("Timestamp" = ticks$Timestamp, "BidPrice" = ticks$BestBid$Price,
                      "BidVolume" = ticks$BestBid$Volume, "BidType" = ticks$BestBid$Type,  "AskPrice" = ticks$BestAsk$Price,
                      "AskVolume" = ticks$BestAsk$Volume, "AskType" = ticks$BestAsk$Type))
  }
)
# #' Init Public Web Client Obj
# #'@name InitPublicWebClient
# #'@param server a character. Web Address.
# #'@param port an integer. Port Number. Default is 8443
# #'@return rTTWebClient obj.
InitPublicWebClient <- function(server = "ttlivewebapi.fxopen.com", port=8443L) {
  return(RTTWebClient(web_api_address=server,
                      web_api_port = port))
}

# #' Init Private Web Client Obj
# #'@name InitPrivateWebClient
# #'@param server a character. Web Address.
# #'@param port an integer. Port Number. Default is 8443
# #'@param id a character. HMAC client id.
# #'@param key a character. HMAC client key.
# #'@param secret a character. HMAC secret key.
# #'@return rTTWebClient obj.
InitPrivateWebClient <- function(server = "ttlivewebapi.fxopen.com", port=8443L, id = "", key = "", secret = "") {
  return(RTTWebClient(web_api_address=server,
                      web_api_port = port,
                      web_api_id = id,
                      web_api_key=key,
                      web_api_secret=secret))
}



#' RTTWebApiHost
#' @name RTTWebApiHost
#' @field client. RTTWebClient obj.
RTTWebApiHost <- setRefClass("RTTWebApiHost",
                            fields = list(client = "RTTWebClient"),
                            methods = list(
                              initialize = function(newWebClient){
                                .self$client <- newWebClient
                              }
                            ))

#' Get All Dividend
#' @name GetDividends
#' @return a data.table with dividends.
RTTWebApiHost$methods(
  GetDividends = function()
  {
    "Get All Dividends"
    # return(.self$client$GetDividendsRawMethod())
    dividends <- Throttling(.self$client$GetDividendsRawMethod)
    dividends[, Time := as.POSIXct(Time / 1000, origin = "1970-01-01", tz = "GMT")]
    return(dividends)
  }
)

#' Get All Symbols
#' @name GetSymbolsInfo
#' @return data.table with symbol info
RTTWebApiHost$methods(
  GetSymbolsInfo = function() {
    "Get All Symbols"
    # symbols <- .self$client$GetSymbolsInfoRawMethod()
    symbols <- Throttling(.self$client$GetSymbolsInfoRawMethod)
    symbols[!grepl("_L$", Symbol), PipsValue := tryCatch(.self$GetPipsValue("USD", Symbol)[,(Value)], error = function(e) {print(e); as.numeric(NA)})]
    currentQuotes <- .self$GetCurrentQuotes()

    symbols[currentQuotes, on = .(Symbol), c("LastTimeUpdate", "LastBidPrice", "LastBidVolume", "LastAskPrice", "LastAskVolume") := list(i.Timestamp, i.BidPrice, i.BidVolume, i.AskPrice, i.AskVolume)]
    return(symbols)
  }
)

#' Get All Currency
#' @name GetCurrencyInfo
#' @return data.table with currency info
RTTWebApiHost$methods(
  GetCurrencyInfo = function() {
    "Get All Currencies"
    currency <- Throttling(.self$client$GetCurrencyInfoRawMethod)
    return(currency)
  }
)

#' Get All Current Quotes
#' @name GetCurrentQuotes
#' @return a data.table with current quotes
RTTWebApiHost$methods(
  GetCurrentQuotes = function() {
    "Get All Current Quotes"
    # currentQuotes <- .self$client$GetCurrentQuotesRawMethod()
    currentQuotes <- Throttling(.self$client$GetCurrentQuotesRawMethod)
    currentQuotes[, Timestamp := as.POSIXct(Timestamp / 1000, origin = "1970-01-01", tz = "GMT")]
    return(currentQuotes)
  }
)

#' Get Pips Value
#' @name GetPipsValue
#' @param targetCurrency a character. Currency Name.
#' @param symbols a character vectors. Symbols vector.
#' @return a data.table with pips value in targetCurrency for every symbol in symbols vector
RTTWebApiHost$methods(
  GetPipsValue = function(targetCurrency, symbols) {
    "Get Pips Value"
    symbols <- paste(sapply(symbols, URLencode, reserved = TRUE, USE.NAMES = FALSE), collapse = URLencode(" ", reserved = FALSE))
    # pipsValue <- .self$client$GetPipsValueRawMethod(targetCurrency, symbols)
    pipsValue <- Throttling(.self$client$GetPipsValueRawMethod, targetCurrency, symbols)
    setcolorder(pipsValue, c(2,1))
    setkey(pipsValue, "Symbol")
    return(pipsValue)
  }
)

#' Get Bar History
#' @name GetBarsHistory
#' @param symbol a character. Symbol Name.
#' @param barsType. a character. Bars Type. One from c("Ask", "Bid").
#' @param periodicity. a character. Periodicity. From c("S1", "S10", "M1", "M5", "M15", "M30", "H1", "H4", "D1", "W1","MN1")
#' @param startTime a POSIXct obj. Start Time in UTC.
#' @param endTime a POSIXct obj. End Time in UTC.
#' @param count. Integer. Count of returned Bars from startTime. Max is 1000. Can be negative. If count == 0, use time interval between startTime and endTime.
#' @return data.table with Bar Info
RTTWebApiHost$methods(
  GetBarsHistory = function(symbol, barsType = "Bid", periodicity = "M1", startTime, endTime = as.POSIXct(Sys.Date(), tz = "GMT"), count = 0L) {
    "Get Bar History"
    symbol <- sapply(symbol, URLencode, reserved = TRUE, USE.NAMES = FALSE)
    if(barsType == "Bid" || barsType == "Ask"){
      return(GetBars(.self$client$GetBarRawMethod, symbol, barsType, periodicity, startTime, endTime, count))
    }
    if(barsType == "BidAsk"){
      bid <- GetBars(.self$client$GetBarRawMethod, symbol, "Bid", periodicity, startTime, endTime, count)
      ask <- GetBars(.self$client$GetBarRawMethod, symbol, "Ask", periodicity, startTime, endTime, count)
      res <- merge(bid, ask, by = "Timestamp", all = TRUE)
      setnames(res, seq_along(res), c("Timestamp", "BidOpen", "BidLow", "BidHigh", "BidClose", "BidVolume",
                                      "AskOpen", "AskLow", "AskHigh", "AskClose", "AskVolume"))
      setnafill(res, type=c("locf"), cols=seq_along(res))
      setnafill(res, type=c("nocb"), cols=seq_along(res))
      return(res)

    }
    stop("Wrong Bar Type")
  }
)

#'Get Ticks History
#' @name GetTickHistory
#' @param symbol. A character. Symbol Name.
#' @param startTime a POSIXct obj. Start Time in UTC.
#' @param endTime a POSIXct obj. End Time in UTC.
#' @param count. Integer. Count of returned Ticks from startTime. Max is 1000. Can be negative. If count == 0, use time interval between startTime and endTime.
#' @return data.table with Ticks Info.
RTTWebApiHost$methods(
  GetTickHistory = function(symbol, startTime, endTime = as.POSIXct(Sys.Date(), tz = "GMT"), count = 0L) {
    "Get Bar History"
    symbol <- sapply(symbol, URLencode, reserved = TRUE, USE.NAMES = FALSE)
    return(GetTicks(.self$client$GetTicksRawMethod, symbol, startTime, endTime, count))
  }
)


#' Init RTTWebApiHost
#'@param server a character. Web Address.
#'@param port an integer. Port Number. Default is 8443
#'@param id a character. HMAC client id.
#'@param key a character. HMAC client key.
#'@param secret a character. HMAC secret key.
#'@return RTTWebApiHost ref class.
#'@importFrom methods new
#'@importFrom withr local_options
#'@export
InitRTTWebApiHost <- function(server = "ttlivewebapi.fxopen.com", port=8443L, id = NULL, key = NULL, secret = NULL){
  if(length(id) != 0 && length(key) != 0 && length(secret) != 0)
    return(RTTWebApiHost$new(InitPrivateWebClient(server, port, id, key, secret)))
  return(RTTWebApiHost$new(InitPublicWebClient(server, port)))
}

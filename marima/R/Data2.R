
##' @name C20
##'
##' @title Data set for testing marima package (Copenhagen Stocks)
##'
##' @description Two years of prices for 18 shares from the Copenhagen
##' Stock Exchange C20 index, covering the most valuable companies.
##' Two shares have been removed (Maersk A = almost identical to Maersk B)
##' and ISS which is incomplete for the period considered.
##'
##' @usage data(C20)
##'
##' @docType data
##'
##' @format A data frame (C20) with 1+18+18 columns and 517 rows (about
##' two full years).
##'
##' \describe{
##' \item{Dates}{Format for date is 2016-04-01} 
##' \item{CARL.fin}{Closing price for stock 'Carlsberg' .}
##' \item{CHR..fin}{Closing price for stock 'Christian Hansen' .}
##' \item{COLO.fin}{Closing price for stock 'Coloplast' .}
##' \item{DANS.fin}{Closing price for stock 'Danske Bank' .}
##' \item{DSV..fin}{Closing price for stock 'DSV' .}
##' \item{GEN..fin}{Closing price for stock 'Genmap' .}
##' \item{GN.2.fin}{Closing price for stock 'GN St. Nord' .}
##' \item{FLS..fin}{Closing price for stock 'FL Smidth' .}
##' \item{JYSK.fin}{Closing price for stock 'Jyske Bank' .}
##' \item{MAER.fin}{Closing price for stock 'Maersk B' .}
##' \item{NDA..fin}{Closing price for stock 'Nordea Bank' .}
##' \item{NOVO.fin}{Closing price for stock 'Novo' .}
##' \item{NZYM.fin}{Closing price for stock 'Novozymes' .}
##' \item{PNDO.fin}{Closing price for stock 'Pandora' .}
##' \item{TDC..fin}{Closing price for stock 'TDC' .} 
##' \item{TRYG.fin}{Closing price for stock Pandora' .}
##' \item{VWS..fin}{Closing price for stock 'Vestas Wind' .}
##' \item{WDH..fin}{Closing price for stock 'Wiliam Demant' .}
##' \item{CARL.ave}{Average price for stock 'Carlsberg' .}
##' \item{CHR..ave}{Average price for stock 'Christian Hansen' .}
##' \item{COLO.ave}{Average price for stock 'Coloplast' .}
##' \item{DANS.ave}{Average price for stock 'Danske Bank' .}
##' \item{DSV..ave}{Average price for stock 'DSV' .}
##' \item{GEN..ave}{Average price for stock 'Genmap' .}
##' \item{GN.2.ave}{Average price for stock 'GN St. Nord' .}
##' \item{FLS..ave}{Average price for stock 'FL Smidth' .}
##' \item{JYSK.ave}{Average price for stock 'Jyske Bank' .}
##' \item{MAER.ave}{Average price for stock 'Maersk B' .}
##' \item{NDA..ave}{Average price for stock 'Nordea Bank' .}
##' \item{NOVO.ave}{Average price for stock 'Novo' .}
##' \item{NZYM.ave}{Average price for stock 'Novozymes' .}
##' \item{PNDO.ave}{Average price for stock Pandora' .}
##' \item{TDC..ave}{Average price for stock 'TDC' .}
##' \item{TRYG.ave}{Average price for stock 'Pandora' .}
##' \item{VWS..ave}{Average price for stock 'Vestas Wind' .}
##' \item{WDH..ave}{Average price for stock 'William Demant' .}
##' }
##'
##' @examples
##'
##' # Example 1:
##'
##' library(marima)
##' data(C20)
##'
##' selects <- c(2,7,11)
##'
##' cat("Multivariate model for ",colnames(C20)[selects]," \n")
##'
##' Data <- data.frame(C20[,selects])
##' colnames(Data) <- colnames(C20)[selects]
##'
##' log.Data <- log(Data)
##' kvar <- length(selects)
##' k    <- c(1:kvar)
##' difs <- rep(1,length(selects))
##'
##' difference <- rbind(k , difs)
##'
##' dlog.Data <- 100*t(define.dif(log.Data,difference)$y.dif)
##'
##' cat("dlog.Data represents the percentage change from day
##' to day. \n")
##' 
##' mod <- define.model(kvar = kvar, ar=c(1:2),ma=c(1))
##' 
##' Model <- marima(dlog.Data,
##'    ar.pattern=mod$ar.pattern, ma.pattern=mod$ma.pattern,penalty=2)
##' 
##' short.form(Model$ar.estimates,leading=FALSE)
##' short.form(Model$ma.estimates,leading=FALSE)
##'
##' # Example 2:

##' library(marima)
##' data(C20)
##'
##' selects <- c(13)
##'
##' cat("Univariate model for ",colnames(C20)[selects]," \n")
##'
##' Data <- data.frame(C20[,selects])
##' colnames(Data) <- colnames(C20)[selects]
##'
##' log.Data <- log(Data)
##' kvar <- length(selects)
##' k    <- c(1:kvar)
##' difs <- rep(1,length(selects))
##'
##' difference <- rbind(k , difs)
##'
##' dlog.Data <- 100*t(define.dif(log.Data,difference)$y.dif)
##'
##' mod <- define.model(kvar = kvar, ar=c(1:2),ma=c(1))
##' 
##' Model <- marima(dlog.Data,
##'    ar.pattern=mod$ar.pattern, ma.pattern=mod$ma.pattern,penalty=2)
##' 
##' short.form(Model$ar.estimates,leading=FALSE)
##' short.form(Model$ma.estimates,leading=FALSE)
##'
NULL

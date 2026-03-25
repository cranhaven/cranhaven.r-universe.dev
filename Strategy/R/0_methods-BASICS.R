setMethod(f = "print",
          signature = "Strategy",
          definition = function(x) {
            signals <- getSignals(x, use.backtest=F)
            cat("\n***********************************")
            cat("\n", getStratName(x, include.params=T), sep="")
            cat("\n")
            cat("\n***********************************")
            cat("\nSignals")
            cat("\nFrom:        ", format(start(signals), format="%Y-%m-%d"), sep="")
            cat("\nUntil:       ", format(end(signals), format="%Y-%m-%d"), sep="")
            cat("\nPeriodicity: ", periodicity(signals)[["scale"]], sep="")
            cat("\n")
            cat("\nData:        ", paste0(colnames(signals[,1])), sep="")
            if (ncol(signals) > 1) {
              for (i in 2:ncol(signals))
                cat("\n            ", paste0(colnames(signals[,i])))
            }
            backtest <- head(x@backtest.signals,n=1)
            backtestExecuted <- "Executed"
            if (!is.numeric(backtest) || is.null(backtest)) {
              backtestExecuted <- "Not yet executed"
            }
            cat("\n")
            cat("\nBacktest:    ", backtestExecuted, sep="")
            cat("\n***********************************")
          }
)

setMethod(f = "show",
          signature = "Strategy",
          definition = function(object){
            print(object)
          }
)

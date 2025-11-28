##############################################################
#' Methods for 'show()' in Package 'pbox'
#'
#' Methods for function show in package \bold{pbox}.
#'
#' @export
#' @name show
#' @aliases show-pbox show,pbox-method
#' @docType methods
#' @include pbox.R
#' @param object an object of class \bold{pbox}.
#'
#' @importFrom stats cor

#' @rdname show-pbox
#' @aliases show-pbox

setMethod(f = "show",
          signature = "pbox",
          definition = function(object){
            cat("Probabilistic Box Object of class pbox\n")
            cat("\n||--General Overview--||")
            cat("\n----------------\n")
            cat("1)Data Structure\n")
            cat("Number of Rows: ", nrow(object@data), "\n")
            cat("Number of Columns: ", ncol(object@data), "\n")
            cat("\n")
            cat("1.1)Variable Statistics:\n")
            print(object@data[, rbindlist(lapply(.SD, fun_stats), idcol = "var")])
            cat("\n----------------\n")
            cat("2)Copula Summary:\n")
            cat("Type:",object@copula@copula@fullname,"\n")
            print(object@copula@copula)
            cat("\n")
            cat("2.1)Copula margins:\n")
            print(object@copula@margins)
            cat("2.1)Kendall correlation:\n")
            print(stats::cor(object@data,method = "kendall"))
            cat("\n-------------------------------\n")

          })

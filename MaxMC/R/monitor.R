#' Format the optimization method controls
#'
#' This function provides a way to merge the user specified controls for the
#' optimization methods with their respective default controls.
#'
#' @param object A data.frame
#' @inheritParams mmc
#'
#' @return A list. Arguments to be used to control the behavior
#' of the algorithm chosen in \code{method}.
#'
#' @example /inst/examples/monitor_mmc_example.R
#'
#' @keywords internal
#'
monitor_mmc <- function(object, alpha = NULL, monitor = TRUE){

    if(monitor==FALSE){
        return()
    } else {
        object<- stats::na.omit(object)
        current <- object[nrow(object),]

        cat("Iteration", current$ite, "| Current", current$pval,
            "| Best", current$max, "\r")
        utils::flush.console()
        x <- list(opt_trace = object, alpha = alpha)
        plot.mmc(x)
    }
}

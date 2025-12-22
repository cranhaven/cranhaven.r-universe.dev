# Error and warning message handling for parallel computation
#
# concept found on
# - the help file of tryCatch; see demo(error.catching).
# - R help: https://stat.ethz.ch/pipermail/r-help/2010-December/262626.html
# - package simsalapar: R/tryCatchWE.R function tryCatch.W.E() and R/doCallWE.R
#   with function doCallWE()
# - https://stackoverflow.com/questions/4948361
#
# We would also like to thank Martin Maechler.
#
# @param expr expression to be evaluated.
# @param ret.obj return argument ret.obj (input) if an error occcurs.
tryCatch_WEM <- function(expr, ret.obj) {
  # warning handler
  warhandler <- function(w) {
    warn <<- append(warn, conditionMessage(w))
    invokeRestart("muffleWarning")
  }

  # message handler
  msghandler <- function(m) {
    msg <<- append(msg, conditionMessage(m))
    invokeRestart("muffleMessage")
  }

  # error handler
  errhandler <- function(e) {
    err <<- conditionMessage(e)
    ret.obj # Return argument ret.obj if an error occcurs.
  }

  # evaluate the expression
  warn <- err <- msg <- NULL
  value <- withCallingHandlers(tryCatch(expr,
                                        error = errhandler),
                               message = msghandler,
                               warning = warhandler)

  # return a list with value, error warning and message:
  list(value = value, error = err, warning = warn, message = msg)
}

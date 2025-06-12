# Prepare package internal namespace
.microEnv <- new.env(parent = emptyenv())
microEnv <- function() .microEnv
putmicroEnv <- function(x, value) assign(x, value, envir = microEnv())
getmicroEnv <- function(x, mode = "any") get0(x, envir = microEnv(), mode = mode, inherits = FALSE)

## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
commandArgs <- function() { ## nolint
  list(
    "--args",
    "a-integer",
    "42",
    "a-float",
    "3.14",
    "a-boolean",
    "FALSE",
    "a-list",
    "1,2,3"
  )
}

## ----setup--------------------------------------------------------------------
library(W4MRUtils)

## ----example parameters parsed------------------------------------------------
param_printer <- function(name, args) {
  sprintf(
    "%s[%s] %s",
    name,
    class(args[[name]])[1],
    paste(args[[name]], collapse = " ")
  )
}
args <- W4MRUtils::parse_args(commandArgs())
args
param_printer("a_integer", args)
param_printer("a_float", args)
param_printer("a_boolean", args)
param_printer("a_list", args)
args$a_list <- as.numeric(strsplit(args$a_list, ",")[[1]])
param_printer("a_list", args)

## ----example parameters not parsed--------------------------------------------
param_printer <- function(name, args) {
  sprintf(
    "%s[%s] %s",
    name,
    class(args[[name]])[1],
    paste(args[[name]], collapse = " ")
  )
}
args <- W4MRUtils::parse_args(
  commandArgs(),
  convert_booleans = FALSE,
  convert_numerics = FALSE
)
args
param_printer("a-integer", args)
param_printer("a-float", args)
param_printer("a-boolean", args)
param_printer("a-list", args)


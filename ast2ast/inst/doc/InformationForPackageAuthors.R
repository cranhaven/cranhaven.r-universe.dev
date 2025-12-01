## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval = FALSE)

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  f <- function(a) {
#    d_db = 1
#    ret <- a + 2 + d_db
#    return(ret)
#  }

## ---- eval = TRUE-------------------------------------------------------------
f <- function(a) {
  b <- a + 2
  return(b)
}  
library(ast2ast)
f_cpp <- translate(f, output = "XPtr", types_of_args = "sexp", return_type = "sexp")

## ---- eval = TRUE-------------------------------------------------------------
call_package(f_cpp)

## ---- eval = TRUE-------------------------------------------------------------
trash <- fct()

## ---- eval = TRUE-------------------------------------------------------------
trash <- fct()

## ---- eval = TRUE-------------------------------------------------------------
trash <- fct()

## ---- eval = TRUE-------------------------------------------------------------
f <- function(a) {
  a <- a + 2
}

library(ast2ast)
fa2a <- translate(f, reference = TRUE, output = "XPtr", types_of_args = "sexp", return_type = "void")
trash <- call_package(fa2a)


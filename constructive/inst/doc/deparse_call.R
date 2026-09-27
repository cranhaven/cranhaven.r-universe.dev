## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----error = TRUE-------------------------------------------------------------
try({
x <- call('+', c(1, 2))
base::deparse(x)
rlang::expr_deparse(x)
constructive::deparse_call(x)

# this is different
y <- quote(+c(1, 2))
x[[2]]
y[[2]]
})

## -----------------------------------------------------------------------------
x <- quote(`*`(a + b, c))
base::deparse(x)
rlang::expr_deparse(x)
constructive::deparse_call(x)

y <- quote((a + b) * c)
base::deparse(y)
rlang::expr_deparse(y)
constructive::deparse_call(y)

# x and y are different, parentheses are code!
x[[2]]
y[[2]]

## -----------------------------------------------------------------------------
x <- call("[")
base::deparse(x)
rlang::expr_deparse(x)
constructive::deparse_call(x)

## ----setup, echo= FALSE-------------------------------------------------------
library(constructive)
#deparse_call <- function(x) gsub("`", "\\\\`", constructive::deparse_call(x))

deparse_call <- function(x) paste(sprintf("`` %s ``", constructive::deparse_call(x)), collapse = "<br>")
deparse <- function(x) paste(sprintf("`` %s ``", base::deparse(x)), collapse = "<br>")
expr_deparse <- function(x) paste(sprintf("`` %s ``", rlang::expr_deparse(x)), collapse = "<br>")

# deparse <- function(x) as_constructive_code(base::deparse(x))
# expr_deparse <- function(x) as_constructive_code(rlang::expr_deparse(x))
compare_deparse_call <- function(x) 
  identical(unclass(constructive::deparse_call(x)), base::deparse(x)) &&
  identical(base::deparse(x), rlang::expr_deparse(x))


## ----error = TRUE-------------------------------------------------------------
try({
x <- call("(", -1)
base::deparse(x)
rlang::expr_deparse(x)
constructive::deparse_call(x)

# this is different! `-` is code!
y <- quote((-1))
base::deparse(y)
rlang::expr_deparse(y)
constructive::deparse_call(y)


x <- call("fun", quote(expr = ))
base::deparse(x)
rlang::expr_deparse(x)
constructive::deparse_call(x) # this is wrong!

# no agument and 1 missing argument is not the same!
y <- call("fun")
base::deparse(y)
rlang::expr_deparse(y)
constructive::deparse_call(y)

x <- call("!", quote(expr = ))
base::deparse(x)
rlang::expr_deparse(x)
constructive::deparse_call(x)
})


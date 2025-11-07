## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(dyn.log)

configs <- dyn.log::get_configurations(pkgname = "dyn.log")

init_logger(configs$knitr)

## ---- comment="", results="asis", echo=F--------------------------------------
options(crayon.enabled=TRUE)
knitr::opts_chunk$set(collapse = TRUE,
                      fig.path = "man/figures/FORMAT-",
                      comment = "")
old.hooks <- fansi::set_knit_hooks(knitr::knit_hooks)

## -----------------------------------------------------------------------------
new_log_layout(
  format = list(
    new_fmt_log_level(),
    new_fmt_timestamp(crayon::silver$italic),
    new_fmt_log_msg()    
  ),
  association = "default_via_r"
)

## -----------------------------------------------------------------------------
Logger$info("this is the default layout")

## -----------------------------------------------------------------------------
Logger$info("this is the custom layout object", layout = "default_via_r")

## -----------------------------------------------------------------------------
new_log_layout(
  format = list(
    new_fmt_metric(crayon::green$bold, "sysname"),
    new_fmt_metric(crayon::yellow$bold, "release"),
    new_fmt_line_break(),
    new_fmt_log_level(),
    new_fmt_timestamp(crayon::silver$italic),
    new_fmt_exec_scope(crayon::magenta$bold, "calling_fn"),
    new_fmt_literal(crayon::blue$italic, "literal text"),
    new_fmt_log_msg(),
    new_fmt_line_break(),
    new_fmt_exec_scope(crayon::cyan$bold, "call_stack")
  ),
  seperator = '-',
  association = "log-with-callstack"
)

## -----------------------------------------------------------------------------
log_fn <- function() {
  outer <- function() {
    inner <- function() {
      
      var1 <- "abc"; var2 <- 123; var3 <- round(runif(1), digits = 6)
      
      Logger$debug("my log message - var1: '{var1}', var2: '{var2}', var3: '{var3}'", 
                   layout = 'log-with-callstack')
    }
    inner()
  }
  outer()
}

log_fn()


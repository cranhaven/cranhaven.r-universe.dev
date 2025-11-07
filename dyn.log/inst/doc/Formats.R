## ----setup--------------------------------------------------------------------
library(dyn.log)

configs <- dyn.log::get_configurations()

# set logging config with tweaks for knitr output
init_logger(configs$knitr)

## ---- comment="", results="asis", echo=F--------------------------------------
options(crayon.enabled = TRUE)

knitr::opts_chunk$set(collapse = TRUE,
                      fig.path = "man/figures/FORMAT-",
                      comment = "")

old.hooks <- fansi::set_knit_hooks(knitr::knit_hooks)

## ---- sys_context_ex----------------------------------------------------------
sys_context()

## ---- sys_context_simple------------------------------------------------------
new_log_layout(
  format = list(
    new_fmt_log_level(),
    new_fmt_metric(crayon::green$bold, "sysname"),
    new_fmt_metric(crayon::yellow$bold, "release"),
    new_fmt_timestamp(crayon::silver$italic, "[%x %H:%M:%S]"),
    new_fmt_log_msg()
  ),
  seperator = '-',
  association = "ex-sys-layout"
)

var1 <- "abc"; var2 <- 123; var3 <- round(runif(1), digits = 6)

Logger$debug("my log message - var1: {var1}, var2: {var2}, var3: {var3}",
             layout = "ex-sys-layout")

## ---- sys_context_literals----------------------------------------------------
new_log_layout(
  format = list(
    new_fmt_metric(crayon::green$bold, "sysname"),
    new_fmt_literal(crayon::magenta, "["),
    new_fmt_metric(crayon::blue$bold, "release"),
    new_fmt_literal(crayon::magenta, "]"),
    new_fmt_line_break(),
    new_fmt_log_level(),
    new_fmt_timestamp(crayon::silver$italic, "[%x %H:%M:%S]"),
    new_fmt_log_msg()
  ),
  seperator = ' ',
  association = "ex-syslit-layout"
)

var1 <- "abc"; var2 <- 123; var3 <- round(runif(1), digits = 6)

Logger$debug("my log message - var1: {var1}, var2: {var2}, var3: {var3}",
             layout = "ex-syslit-layout")

## ---- set_knitter_offset, echo=F----------------------------------------------
settings <- dyn.log::get_active_settings()
callstack_settings <- settings$callstack

## ---- exec_scope_ex, eval=T---------------------------------------------------
test <- function(a, b, c) {
  wrapper <- function(x, y, z) {
    outer <- function(d, e, f) {
      inner <- function(g, h, i) {
        # call_subset is used here to skip past knitr execution calls
        exec_context(max_calls = 30, call_subset = c(callstack_settings$start,
                                                     callstack_settings$stop))
      }

      inner(d, e, f)
    }

    outer(x, y, z)
  }
  wrapper(a, b, c)
}

exec_context <- test(1,2,3)

## -----------------------------------------------------------------------------
exec_context

## ---- sysexec_example---------------------------------------------------------
new_log_layout(
  format = list(
    new_fmt_metric(crayon::green$bold, 'sysname'),
    new_fmt_metric(crayon::blue$yellow, 'release'),
    new_fmt_line_break(),
    new_fmt_log_level(),
    new_fmt_timestamp(crayon::silver$italic, '[%x %H:%M:%S]'),
    new_fmt_literal(crayon::magenta$bold, 'fn('),
    new_fmt_exec_scope(crayon::magenta$bold, 'calling_fn'),
    new_fmt_literal(crayon::magenta$bold, ')'),
    new_fmt_log_msg(),
    new_fmt_line_break(),
    new_fmt_exec_scope(crayon::bgYellow$blue$bold, 'call_stack')
  ),
  seperator = '-',
  association = 'ex-sysexec-cs-layout'
)

local_fn <- function() {
  outer <- function() {
    inner <- function() {
      var1 <- "abc"; var2 <- 123; var3 <- round(runif(1), digits = 6)

      Logger$debug("my log message - var1: '{var1}', var2: '{var2}', var3: '{var3}'",
                   layout = 'ex-sysexec-cs-layout')
    }
    inner()
  }
  outer()
}

local_fn()


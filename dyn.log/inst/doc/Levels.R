## ----setup--------------------------------------------------------------------
library(dyn.log)

init_logger()

## ---- comment="", results="asis", echo=F--------------------------------------
options(crayon.enabled=TRUE)
knitr::opts_chunk$set(collapse = TRUE, comment = "")
old.hooks <- fansi::set_knit_hooks(knitr::knit_hooks)

## ---- simple_debug_example----------------------------------------------------
var1 <- "abc"; var2 <- 123; var3 <- round(runif(1), digits = 6)

Logger$debug("my log message - var1: {var1}, var2: {var2}, var3: {var3}")

## ---- log_levels_ex, eval = F-------------------------------------------------
#  log_levels()

## ---- echo = F----------------------------------------------------------------
pander::pander(level_severities())

## ---- level_info_ex-----------------------------------------------------------
level_info("debug")

## ---- display_log_levels_ex---------------------------------------------------
display_log_levels()


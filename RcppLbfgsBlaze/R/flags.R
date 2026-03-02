## Copyright (C) 2024 Ching-Chuan Chen
##
## This file is part of RcppLbfgsBlaze.
##
## RcppLbfgsBlaze is free software: you can redistribute it and/or modify it
## under the terms of the MIT License. You should have received
## a copy of MIT License along with RcppLbfgsBlaze.
## If not, see https://opensource.org/license/mit.

RcppLbfgsBlazeCxxFlags <- function() {
  paste0('-I"', system.file("include", package = "RcppLbfgsBlaze"), '"')
}

CxxFlags <- function() cat(RcppLbfgsBlazeCxxFlags())

# Inline plugin used by sourceCpp.
#' @importFrom Rcpp Rcpp.plugin.maker evalCpp sourceCpp
#' @importFrom RcppBlaze blaze_version
inlineCxxPlugin <-  function() {
  openmpFlag <- ifelse(Sys.info()[["sysname"]] == "Darwin", "", "$(SHLIB_OPENMP_CFLAGS)")
  getSettings <- Rcpp.plugin.maker(
    include.before = "#include <RcppBlaze.h>\n#include <lbfgs.h>",
    libs = paste(openmpFlag, "$(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)"),
    package = c("RcppBlaze", "RcppLbfgsBlaze")
  )
  settings <- getSettings()
  settings$env$PKG_CPPFLAGS <- paste("-I../inst/include", openmpFlag)
  return(settings)
}

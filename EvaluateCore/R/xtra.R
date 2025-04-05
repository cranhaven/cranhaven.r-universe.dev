### This file is part of 'EvaluateCore' package for R.

### Copyright (C) 2018-2022, ICAR-NBPGR.
#
# EvaluateCore is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# EvaluateCore is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.r-project.org/Licenses/


wlcm <- paste0("\n",
               "--------------------------------------------------------------------------------\n",
               "Welcome to EvaluateCore version ", utils::packageDescription("EvaluateCore")$Version, "\n",
               "\n", "\n",
               # "# To know how to use this package type:", "\n",
               # "  browseVignettes(package = 'EvaluateCore')", "\n", "  for the package vignette.", "\n",
               # "\n",
               "# To know whats new in this version type:", "\n",
               "  news(package='EvaluateCore')", "\n", "  for the NEWS file.", "\n",
               "\n",
               "# To cite the methods in the package type:", "\n",
               "  citation(package='EvaluateCore')", "\n",
               "\n",
               "# To suppress this message use:", "\n",
               "  suppressPackageStartupMessages(library(EvaluateCore))", "\n",
               "--------------------------------------------------------------------------------\n")

.onAttach <- function(lib, pkg, ...) {
  packageStartupMessage(wlcm)


}

# #' @import rJava
# .onLoad <- function(libname, pkgname) {
#   rJava::.jpackage("corehunter")
#   rJava::.jpackage(pkgname, lib.loc = libname)
#
#   # # check Java version
#   # req.version <- 8
#   # version.string <- J("java.lang.System")$getProperty("java.version")
#   # version <- as.integer(strsplit(version.string, ".", fixed = TRUE)[[1]][2])
#   # if (version < req.version) {
#   #   stop(sprintf(
#   #     'Java version %d or later required for "dist.evaluate.core()". Found version %d.',
#   #     req.version, version
#   #   ))
#   # }
#
#   # check Java version
#   .jinit()
#   jv <- .jcall("java/lang/System", "S", "getProperty", "java.runtime.version")
#   if(substr(jv, 1L, 1L) == "1") {
#     jvn <- as.numeric(paste0(strsplit(jv, "[.]")[[1L]][1:2], collapse = "."))
#     if(jvn < 1.8) {
#       stop("Java >= 8 is needed for this package but not available.")
#     }
#   }
# }

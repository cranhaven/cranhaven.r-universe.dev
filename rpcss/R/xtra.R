### This file is part of 'rpcss' package for R.

### Copyright (C) 2024-2025, ICAR-NBPGR.
#
# rpcss is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# rpcss is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.r-project.org/Licenses/

wlcm <-
  paste0("\n",
         "------------------------------------------------------------------",
         "--------------\n",
         "Welcome to rpcss version ",
         utils::packageDescription("rpcss")$Version, "\n",
         "\n", "\n",
         # "# To know how to use this package type:", "\n",
         # "  browseVignettes(package = 'rpcss')", "\n",
         # "  for the package vignette.", "\n",
         # "\n",
         "# To know whats new in this version type:", "\n",
         "  news(package='rpcss')", "\n", "  for the NEWS file.", "\n",
         "\n",
         "# To cite the methods in the package type:", "\n",
         "  citation(package='rpcss')", "\n",
         "\n",
         "# To suppress this message use:", "\n",
         "  suppressPackageStartupMessages(library(rpcss))", "\n",
         "------------------------------------------------------------------",
         "--------------\n")

.onAttach <- function(lib, pkg, ...) {
  packageStartupMessage(wlcm)


}

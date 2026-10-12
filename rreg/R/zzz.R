## Copyright (C) Yusman Kamaleri

## This file is part of rreg package.

## require(rreg)
## detach("package:rreg")

.onAttach <- function(lib, pkg, ...){
  packageStartupMessage(pkgWelcomeMessage())
}


pkgWelcomeMessage <- function(){
   
  paste("\n",     
        "Welcome to rreg version ", utils::packageDescription("rreg")$Version, "\n",
        "\n",
        # "Type ?aimPlot to access the overall documentation and\n",
        "More information is available on the Github:\n",
        "https://github.com/ybkamaleri/rreg/\n",
        "\n",               
        "Contact: <ybkamaleri@gmail.com>\n",
        "Suggestions and bug-reports can be submitted at: https://github.com/ybkamaleri/rreg/issues\n",
        "\n",
        "\tTo suppress this message use:\n",
        "\tsuppressPackageStartupMessages(library(rreg))\n",
        sep="")
}

## .onLoad <- function(libname, pkgname) {

##   ## henter pakker eller installere om ikke finnes
##   inspak <- function(pkg){
##     nypkg <- pkg[!(pkg %in% utils::installed.packages()[, "Package"])]
##     if (length(nypkg))
##       utils::install.packages(nypkg, dependencies = TRUE, repos = "http://cran.rstudio.com")
##   }

##   pakke <- c("ggplot2", "directlabels")
##   inspak(pakke)

##   ## Upload libraries
##   library(ggplot2)
##   library(directlabels)
## }

library("knitr")

old_wd <- getwd()
setwd("vignettes/")
knit("verification.Rmd.orig", "verification.Rmd")
knit("simulations.Rmd.orig", "simulations.Rmd")
setwd(old_wd)

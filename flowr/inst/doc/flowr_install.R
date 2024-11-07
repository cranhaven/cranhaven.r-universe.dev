## ----echo=FALSE, message=FALSE------------------------------------------------
library(params)
library(flowr)

## ----eval=FALSE---------------------------------------------------------------
#  ## for a latest released version (from CRAN)
#  install.packages("flowr", repos = CRAN="http://cran.rstudio.com")
#  
#  ## OR latest version
#  devtools::install_github("sahilseth/flowr", ref = "master")
#  

## ----eval=FALSE---------------------------------------------------------------
#  library(flowr)
#  setup()

## ----build_pipe_flow_def_cols, echo=FALSE, message=FALSE----------------------
#extdata = file.path(system.file(package = "flowr"), "extdata")
mat = read_sheet("files/flow_def_columns.txt")
kable(mat, col.names = c("flowdef variable", "submission template variable"))


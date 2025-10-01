## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Asiaverse)

## ----install_package, eval=FALSE----------------------------------------------
# 
# # Install from CRAN
# install.packages("Asiaverse")
# 
# # Then load the package:
# library(Asiaverse)
# 
# 

## ----asiaverse_function, eval=FALSE-------------------------------------------
# 
# Asiaverse()
# 

## ----detach, eval=FALSE-------------------------------------------------------
# 
# # This will raise an error
# detach("package:SouthKoreAPIs", unload = TRUE)
# 

## ----detach_way, eval=FALSE---------------------------------------------------
# 
# # First detach the metapackage
# detach("package:Asiaverse", unload = TRUE)
# 
# # Now you can safely detach the subpackage
# detach("package:SouthKoreAPIs", unload = TRUE)
# 


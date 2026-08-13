## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(DataSetsVerse)

## ----install_package, eval=FALSE----------------------------------------------
# 
# # Install from CRAN
# install.packages("DataSetsVerse")
# 
# # Then load the package:
# library(DataSetsVerse)
# 
# 

## ----datasetsverse_function, eval=FALSE---------------------------------------
# 
# DataSetsVerse()
# 

## ----detach, eval=FALSE-------------------------------------------------------
# 
# # This will raise an error
# detach("package:OncoDataSets", unload = TRUE)
# 

## ----detach_way, eval=FALSE---------------------------------------------------
# 
# # First detach the metapackage
# detach("package:DataSetsVerse", unload = TRUE)
# 
# # Now you can safely detach the subpackage
# detach("package:OncoDataSets", unload = TRUE)
# 


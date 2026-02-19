## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  library(naturaList)
#  data("A.setosa") # occurrence points for A.setosa
#  data("speciaLists") # list of specialists for A.setosa
#  
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  occ.class <- classify_occ(A.setosa, speciaLists)
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  occ.select <- map_module(occ.class)
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  occ.select1 <- map_module(occ.class, action = "flag")
#  occ.select2 <- map_module(occ.class, action = "clean")
#  


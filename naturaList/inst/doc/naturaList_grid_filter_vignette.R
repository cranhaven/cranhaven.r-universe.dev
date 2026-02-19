## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----grid_filter, eval=F------------------------------------------------------
#  # Load package and data
#  library(naturaList)
#  
#  data("A.setosa")
#  data("speciaLists")
#  
#  # classification
#  occ.class <- classify_occ(A.setosa, speciaLists)
#  
#  # grid filtering
#  occ.grid <- grid_filter(occ.class)
#  
#  dim(occ.grid)
#  table(occ.grid$naturaList_levels)


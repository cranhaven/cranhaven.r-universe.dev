## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#"
)

## ----install, eval=F, echo = T------------------------------------------------
#  install.packages("naturaList")

## ----setup, eval=FALSE--------------------------------------------------------
#  # Load package and data
#  library(naturaList)
#  
#  data("A.setosa")
#  data("speciaLists")
#  
#  # see the size of datasets
#  dim(A.setosa) # see ?A.setosa for details
#  dim(speciaLists) # see ?speciaLists for details

## ----classify, eval=F, echo = T-----------------------------------------------
#  # classification
#  occ.class <- classify_occ(A.setosa, speciaLists)
#  dim(occ.class)

## ----levels, eval=F, echo = T-------------------------------------------------
#  table(occ.class$naturaList_levels)

## ----create_spec_df, eval=F, echo = T-----------------------------------------
#  # create a specialist table example
#  br.musicians <- c("Caetano Veloso", "Antônio Carlos Tom Jobim",
#                    "Gilberto Gil", "Vinícius de Morais")
#  
#  spec_df <- create_spec_df(br.musicians)
#  spec_df

## ----get_det_names, eval=F, echo = T------------------------------------------
#  
#  # check out if there are strings which are not taxonomists
#  get_det_names(A.setosa)
#  
#  # include these strings in a object
#  ig.names <- c("Sem Informação" , "Anonymous")
#  
#  # use 'ignore.det.names' to ignore those strings in classify_occ()
#  occ.class <- classify_occ(A.setosa, speciaLists, ignore.det.names = ig.names)
#  
#  
#  table(occ.class$naturaList_levels)
#  


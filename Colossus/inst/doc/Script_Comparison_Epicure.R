## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Colossus)
library(data.table)
library(parallel)

## ----eval=FALSE---------------------------------------------------------------
#  df_Dose   <- fread("EX_DOSE.csv")

## ----eval=FALSE---------------------------------------------------------------
#  col_list <- c("SES_CAT","YOB_CAT","dose_cat")
#  val <- factorize(df_Dose,col_list)
#  df_Dose <- val$df
#  
#  t0 <- "age_entry"
#  t1 <- "age_exit"
#  event <- "nonCLL"

## ----eval=FALSE---------------------------------------------------------------
#  
#  # ERR
#  names <- c('cumulative_dose',"SES_CAT_1", "SES_CAT_2", "YOB_CAT_1","YOB_CAT_2","YOB_CAT_3","YOB_CAT_4", "sexm")
#  tform <- c("plin",rep('loglin',length(names)-1))
#  control <- list("Ncores"=8, 'maxiter'=50, 'verbose'=F)

## ----eval=FALSE---------------------------------------------------------------
#  e <- RunCoxRegression(df_Dose, t0, t1, event, names, tform=tform, control = control)

## ----eval=FALSE---------------------------------------------------------------
#  # HR
#  names <- c('cumulative_dose',"SES_CAT_1", "SES_CAT_2", "YOB_CAT_1","YOB_CAT_2","YOB_CAT_3","YOB_CAT_4", "sexm")
#  tform <- rep('loglin',length(names))
#  e <- RunCoxRegression(df_Dose, t0, t1, event, names, tform=tform, control = control)
#  
#  # Categorical
#  names <- c('dose_cat_1','dose_cat_2','dose_cat_3','dose_cat_4','dose_cat_5','dose_cat_6', "SES_CAT_1", "SES_CAT_2", "YOB_CAT_1","YOB_CAT_2","YOB_CAT_3","YOB_CAT_4", "sexm")
#  tform <- rep('loglin',length(names))
#  e <- RunCoxRegression(df_Dose, t0, t1, event, names, tform=tform, control = control)


## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(matahari)

## ----eval = FALSE-------------------------------------------------------------
# # Start logging your R commands run in the console
# dance_start()
# 
# 4 + 4
# "wow!"
# mean(1:10)
# 
# # Pause logging
# dance_stop()
# 
# # Look at your log as a tidy data frame
# dance_tbl()

## ----eval = FALSE-------------------------------------------------------------
# dance_remove()

## ----eval = FALSE-------------------------------------------------------------
# # Start logging your R commands run in the console
# dance_start(value = TRUE)
# 
# 4 + 4
# "wow!"
# mean(1:10)
# 
# # Pause logging
# dance_stop()
# 
# # Look at your log as a tidy data frame
# dance_tbl()

## ----eval=FALSE---------------------------------------------------------------
# dance_recital("
# 4 + 4
# 'wow!'
# mean(1:10)
#              ")

## -----------------------------------------------------------------------------
file <- system.file("test", "sample_code.R", package = "matahari")

## ----eval=FALSE---------------------------------------------------------------
# dance_recital(file)


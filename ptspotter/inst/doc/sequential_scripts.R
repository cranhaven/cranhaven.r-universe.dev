## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_knit$set(root.dir = tempdir())

## ----setup, message=FALSE-----------------------------------------------------

library(ptspotter)



## -----------------------------------------------------------------------------
# old_wd <- setwd(tempdir())

# create a folder with some sequentially labelled scripts
seq_file_ops(n = 10, target_dir = "munge", filetype = "R")

# take a peek to confirm
list.files("munge")

## -----------------------------------------------------------------------------
adj_file_nos(target = 2, directory = "munge")

list.files("munge")

## -----------------------------------------------------------------------------
file.create("munge/02-.R")

list.files("munge")

## -----------------------------------------------------------------------------
rm_these <- c("06-.R", "07-.R", "08-.R")
file.remove(paste("munge", rm_these, sep = "/"))
list.files("munge")

## -----------------------------------------------------------------------------
adj_file_nos(target = 9, directory = "munge", action = "down", step = 3)
list.files("munge")

## ---- include=FALSE-----------------------------------------------------------
unlink("munge", recursive = TRUE)


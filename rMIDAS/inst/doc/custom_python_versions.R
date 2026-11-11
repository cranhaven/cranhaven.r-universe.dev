## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----eval = FALSE-------------------------------------------------------------
# 
# library(rMIDAS)
# 
# # Decline the automatic setup
# 
# set_python_env(x = "~/path/to/bin/python")
# 
# # Then proceed as normal...
# 

## ----eval = FALSE-------------------------------------------------------------
# 
# library(rMIDAS)
# 
# # Decline the automatic setup
# 
# set_python_env(x = "myenv", type = "virtualenv")
# 
# # or
# 
# set_python_env(x = "mycondaenv", type = "conda")
# 
# # Then proceed as normal...
# 

## ----eval=FALSE---------------------------------------------------------------
# set_python_env(x = "rmidas", type = "conda")

## ----eval = FALSE-------------------------------------------------------------
# 
# library(rMIDAS)
# 
# # Decline the automatic setup
# 
# reticulate::use_condaenv(condaenv = "myenv", conda = "some_conda_executable", required = FALSE)
# midas_setup()
# 
# # Then proceed as normal...
# 

## ----eval = FALSE-------------------------------------------------------------
# 
# set_python_env(x = "/usr/local/bin/python3")
# 
# # Then proceed as normal...

## ----eval = FALSE-------------------------------------------------------------
# 
# reticulate::virtualenv_create(envname = "myenv", python = "/path/to/your/python3/bin")
# set_python_env(x = "myenv", type = "virtualenv")
# 
# # Then proceed as normal...
# 


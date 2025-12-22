## ----options, include = FALSE-------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = ""
  
)

## ----setup, echo=FALSE--------------------------------------------------------
library(stringr)


## ----echo = FALSE-------------------------------------------------------------
models_path <- system.file(package = "mbbe","examples")

## ---- echo=FALSE--------------------------------------------------------------
file.path(models_path,list.files(models_path, pattern = "\\.mod$"))

## ---- echo = FALSE------------------------------------------------------------
readLines(list.files(models_path, pattern = "\\.mod$", full.names = TRUE)[1])

## ----include=FALSE------------------------------------------------------------
models_path <- file.path(system.file(package = "mbbe"),"inst","examples")
if( .Platform$OS.type == "windows" )
  models_path <- str_replace_all(models_path,"//","\\")

## ---- echo=FALSE--------------------------------------------------------------
cat(models_path)

## ----include=FALSE------------------------------------------------------------
data_path <- file.path(system.file(package = "mbbe"),"inst","examples","data.csv")
data_sim_path <- file.path(system.file(package = "mbbe"),"inst","examples","data_sim.csv")
if( .Platform$OS.type == "windows" )
  data_path <- str_replace_all(data_path,"//","\\")
if( .Platform$OS.type == "windows" )
  data_sim_path <- str_replace_all(data_sim_path,"//","\\")

## ---- echo=FALSE--------------------------------------------------------------
cat(data_path)
cat(data_sim_path)

## ----include=FALSE------------------------------------------------------------
MBBE_file <- file.path(system.file(package = "mbbe"),"inst","examples","mbbeargs.json") 
if( .Platform$OS.type == "windows" )
  MBBE_file <- str_replace_all(MBBE_file,"//","\\")

## ---- echo=FALSE--------------------------------------------------------------
cat(MBBE_file) 

## ----include=FALSE------------------------------------------------------------
Rcode_path <- file.path(system.file(package = "mbbe"),"inst","example","RPenaltyCode.R")
if( .Platform$OS.type == "windows" )
  Rcode_path <- str_replace_all(Rcode_path,"//","\\")

## ---- echo=FALSE--------------------------------------------------------------
cat(Rcode_path)

## ----include=FALSE------------------------------------------------------------
MBBE_file <- file.path(system.file(package = "mbbe"),"inst","examples","mbbeargs.json")
if( .Platform$OS.type == "windows" )
  MBBE_file <- str_replace_all(MBBE_file,"//","\\")


## ---- echo=FALSE--------------------------------------------------------------
cat(MBBE_file)


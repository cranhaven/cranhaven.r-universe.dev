## ----echo=FALSE, include=FALSE------------------------------------------------
pkg <- read.dcf("../DESCRIPTION", fields = "Package")[1]
library(pkg, character.only = TRUE)

## -----------------------------------------------------------------------------
v <- "devel"
f1 <- use_workflow(name = paste("rworkflows",v,sep="."), 
                   branches = v, 
                   runners = construct_runners(bioc = v), 
                   preview = TRUE,
                   force_new = TRUE,
                   save_dir = tempdir() # For demo only, use default in practice
                   ) 

## -----------------------------------------------------------------------------
v <- "RELEASE_3_17"
f2 <- use_workflow(name = paste("rworkflows",v,sep="."), 
                   branches = v, 
                   runners = construct_runners(bioc = v), 
                   preview = TRUE,
                   force_new = TRUE,
                   save_dir = tempdir() # For demo only, use default in practice
                   ) 

## ----Session Info-------------------------------------------------------------
utils::sessionInfo()


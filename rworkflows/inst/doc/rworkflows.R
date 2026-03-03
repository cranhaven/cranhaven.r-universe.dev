## ----echo=FALSE, include=FALSE------------------------------------------------
pkg <- read.dcf("../DESCRIPTION", fields = "Package")[1]
library(pkg, character.only = TRUE)

## -----------------------------------------------------------------------------
workflow <- rworkflows::use_workflow(run_bioccheck = FALSE, 
                                     run_rcmdcheck = TRUE,  
                                     run_pkgdown = TRUE, 
                                     run_docker = TRUE,
                                     docker_user = "bschilder",
                                     docker_org = "neurogenomicslab",
                                     force_new = TRUE,
                                     ## Use default save_dir in practice
                                     save_dir = tempdir())

## -----------------------------------------------------------------------------
workflow_static <- rworkflows::use_workflow(name = "rworkflows_static",
                                     run_bioccheck = FALSE, 
                                     run_rcmdcheck = TRUE,  
                                     run_pkgdown = TRUE, 
                                     run_docker = TRUE,
                                     docker_user = "bschilder",
                                     docker_org = "neurogenomicslab",
                                     force_new = TRUE,
                                     ## Use default save_dir in practice
                                     save_dir = tempdir())

## ----results='asis'-----------------------------------------------------------
badges <- rworkflows::use_badges()

## ----results='asis'-----------------------------------------------------------
## Use default save_dir in practice
dockerfile <- rworkflows::use_dockerfile(save_dir = tempdir()) 

## -----------------------------------------------------------------------------
## Use default save_dir in practice
readme <- rworkflows::use_readme(save_dir = tempdir())

## -----------------------------------------------------------------------------
## Use default save_dir in practice
vignette1 <- rworkflows::use_vignette_getstarted(package = "mypackage",
                                                 save_dir = tempdir())

## -----------------------------------------------------------------------------
## Use default save_dir in practice
vignette2 <- rworkflows::use_vignette_docker(docker_org = "neurogenomics",
                                             save_dir = tempdir())

## -----------------------------------------------------------------------------
runners <- rworkflows::construct_runners(
  python_version = list("ubuntu-latest"="3.10.11",
                        "macOS-latest"="3.9",
                        "windows-latest"="3.9.1"))
workflow <- rworkflows::use_workflow(runners = runners,
                                     preview = TRUE,
                                     force_new = TRUE,
                                     ## Use default save_dir in practice
                                     save_dir = tempdir())

## -----------------------------------------------------------------------------
environment_file <- construct_conda_yml(name = "myenv", 
                                        channels = c("conda-forge",
                                                     "bioconda"),
                                        dependencies = c("python>=3.7",
                                                         "scanpy",
                                                         "anndata"), 
                                        pip = c("scarches",
                                                "magic-impute"),
                                        preview = TRUE)

## -----------------------------------------------------------------------------
workflow <- rworkflows::use_workflow(environment_file = environment_file, 
                                     preview = TRUE, 
                                     force_new = TRUE,
                                     ## Use default save_dir in practice
                                     save_dir = tempdir())

## -----------------------------------------------------------------------------
## Cleanup files
try({file.remove(environment_file)})

## ----Session Info-------------------------------------------------------------
utils::sessionInfo()


params <-
list(cont = "ghcr.io/neurogenomics/rworkflows", docker_registry = "ghcr.io", 
    docker_org = "neurogenomics")

## ----setup, include=FALSE-----------------------------------------------------
#### Package name ####
PKG <- read.dcf("../DESCRIPTION", fields = "Package")[1]
library(PKG, character.only = TRUE)
## Docker containers must be lowercase
pkg <- tolower(PKG)
#### Username of DockerHub account ####
docker_org <- params$docker_org
docker_registry <- params$docker_registry 
cont <- params$cont 
docker_url <- if(grepl("ghcr.io",docker_registry)){
  paste("https://ghcr.io",cont,sep="/")
} else {
  paste("https://hub.docker.com/repository/docker",docker_org,pkg,sep="/")
}


## ----Session Info-------------------------------------------------------------
utils::sessionInfo()


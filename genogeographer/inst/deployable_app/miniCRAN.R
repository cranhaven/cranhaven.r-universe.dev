## From: https://cran.r-project.org/web/packages/miniCRAN/vignettes/miniCRAN-introduction.html

miniCRAN_path <- "../ggg_miniCRAN"

create_local_CRAN <- FALSE
install_local_CRAN <- FALSE
RGA_install <- FALSE

if(install_local_CRAN){
  ## At Section of Forensic Genetics, Department of Forensic Medicine, Factulty of Health and Medical Sciences, University of Copenhagen:
  if(RGA_install) setwd("root")
  install.packages("genogeographer", repos = paste0("file:///", miniCRAN_path), type = "source")
  ## possibly the packages in ggg_loaded_packages
}

if(create_local_CRAN){

  ggg_loaded_packages <- 
    c("genogeographer", 
      "leaflet", "shiny", "shinyjs", "knitr", "DT", "shinycssloaders", "rmarkdown", "rio", "shinyWidgets",  ## others
      "purrr", "dplyr", "magrittr", "tidyr", "ggplot2", "tibble", "forcats", "readr", "tidyverse", "readxl") ## tidyverse

  ## patchwork imports: ggplot2, gtable, grid, stats, grDevices, utils
  
  library(miniCRAN) ## install.packages("miniCRAN")

  online_CRAN_repos <- c(CRAN = "http://cloud.r-project.org/")

  genogeographer_PkgList <- pkgDep(ggg_loaded_packages, repos=online_CRAN_repos, type="source", suggests = FALSE)
  
  if(!file.exists(miniCRAN_path)) dir.create(miniCRAN_path)
  
  makeRepo(genogeographer_PkgList, path=miniCRAN_path, repos=online_CRAN_repos, type=c("source", "win.binary"))
  
  # list.files(miniCRAN_path, recursive=TRUE, full.names=FALSE) %>% basename() %>% 
  #   tolower() %>% sort()
  
  # pkgAvail(repos=miniCRAN_path, type="source") %>% as_tibble() %>% select(Package:Priority, Imports) %>% print(n = Inf)

}

## Adding to existing
if(FALSE){
  addPackage("maps", path = miniCRAN_path, repos = online_CRAN_repos, type = c("source", "win.binary"))
  pkgAvail(repos=miniCRAN_path, type="source") %>% as_tibble() %>% select(Package:Priority, Imports) %>% print(n = Inf)
}

## Update repos
if(FALSE){
  updatePackages(path = miniCRAN_path, repos = online_CRAN_repos, type = "source", ask = FALSE) # should need update
}
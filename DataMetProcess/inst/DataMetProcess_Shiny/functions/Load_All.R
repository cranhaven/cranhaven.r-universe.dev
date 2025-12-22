load_all <- function(path = "./inst/DataMetProcess_Shiny/"){
  fns <- list.files(paste0(path,"functions/"),full.names = T,recursive = T)
  part <- list.files(paste0(path,"partitions/"),full.names = T,recursive = T)
  for(i in c(fns,part)){
    source(i)
  }

}


.onLoad <- function(libname, pkgname) { # nolint
  config <- tools::R_user_dir('config')
  if(!dir.exists(config)) dir.create(config,recursive=T)
  
  # if there is already a config file 
  if( length(list.files(config,all.files=T)[grepl('.Roptions.Rdata',list.files(config,all.files=T))]) > 0){
    load(file=paste0(config,"/.Roptions.Rdata"))
    op <- options()
    toset <- !(names(my.options) %in% names(op))
    if (any(toset)) options(my.options[toset])
    invisible()
    } else {
    
    dataset_root <- "TABS"
    dataset_datatype <- list('topo'='tif', 'labs'='gpkg','curve'=c('ASC','tif'))
    dataset_list <- names(dataset_datatype)
    update <- c(F,F,F)
    source <- c(NA,NA,NA)
    datasetpathfull <- c(NA,NA,NA)
    lastupdate <- c(NA,NA,NA)
    updateasked <- c(NA,NA,NA)
    
    names(source) <- dataset_list
    names(update) <- dataset_list
    names(lastupdate) <- dataset_list
    names(datasetpathfull) <- dataset_list
    names(updateasked) <- dataset_list
    datasetCurves <- list('Lambeck'='https://www.pnas.org/doi/full/10.1073/pnas.1411762111',
                          'Cutler'='https://doi.org/10.1016/S0012-821X(02)01107-X',
                          'Bintanja'='https://doi.org/10.1038/nature03975',
                          'st_curve'='https://doi.org/10.1111/geb.13573')
    op <- options()
    op_tabs <- list(
      tabs.CachePath = NULL,
      tabs.datasetNames = dataset_list,
      tabs.datasetDatatype = dataset_datatype, 
      tabs.datasetRoot = dataset_root,
      tabs.datasetDefaultPath = gsub('\\\\','/',tools::R_user_dir(dataset_root, which = "data")), # default path 
      tabs.datasetPath = NULL, # '/Volumes/Extreme SSD/TEMP/atlas' will be the same as default when the default directory is used 
      # tabs.bathymetry = F, # update available 
      # tabs.reference = F, # update available 
      # tabs.curve = F, # update available 
      tabs.datasetUpdate = update, 
      tabs.datasetSource = source,
      tabs.datasetFullPath = datasetpathfull,
      tabs.datasetLastUpdate = lastupdate,
      tabs.datasetUpdateAsked = updateasked,
      tabs.datasetCurves = datasetCurves,
      tabs.current_extent_vect = FALSE, # current extent_vector
      tabs.current_extent_poly = FALSE, # current extent_polygon
      tabs.current_extent_leaf = FALSE, # current extent_leaf
      tabs.current_extent = FALSE # current extent 
    )
    toset <- !(names(op_tabs) %in% names(op))
    #toset <- rep(T,length(op_tabs))
    if (any(toset)) options(op_tabs[toset])
    invisible()
    
    my.options <- op_tabs
    save(my.options, file=paste0(config,"/.Roptions.Rdata"))
    
  }
 
}




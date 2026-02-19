#' @importFrom httr HEAD GET set_config config http_error progress
#' @importFrom utils menu read.csv unzip write.csv write.table zip
#' 
#' @title create_structure
#' 
#' @author Johannes De Groeve
#' 
#' @description create the directory where the data will be saved 
#'
#' 
#' @return a vector with the download link
#'
#' @noRd
#' 
#' @keywords internal
#' 
create_structure <- function(path=NULL,name=options()$tabs.datasetNames,verbose=F){
  
  # check if arg name has more than one value 
  if(length(name) < 2){name <- match.arg(name)}
  
  # if path is null, specify the default path where to save output 
  if(is.null(path)){
    path <- options()$tabs.datasetDefaultPath #tools::R_user_dir("tabs_data", which = "data")
    options(list(tabs.datasetPath = path))
  } else {
    if(!grepl(options()$tabs.datasetRoot,path)){
    path <- gsub('//','/',paste0(path,'/',options()$tabs.datasetRoot))
    }
    options(list(tabs.datasetPath = path))
    # op_tabs <- list(
    #   tabs.datasetPath = path)
    # toset <- !(names(op_tabs) %in% names(op))
    # if (any(toset)) 
    
  }

  # create directory per dataset
  path_dataset  <- as.vector(unlist(lapply(name, function(x) {
      path_dataset <- gsub('//','/',paste0(path, '/',x))
      if(!dir.exists(path_dataset)){
        dir.create(path_dataset, recursive=TRUE)
      } else {
        if(as.numeric(verbose) > 1){
        message(paste('directory', path_dataset, 'exists'))
        }
      }
      return(path_dataset)
    })))
  #message(paste('structure created in directory', path))
  
  return(path_dataset)
}

#' @title download_link_topo
#' 
#' @author Johannes De Groeve
#' @description return the download link of the most recent available global bathymetry dataset
#'
#' 
#' @return a vector with the download link
#'
#' @noRd
#' 
#' @keywords internal 
#'
#' 
download_link_topo <- function(){ # dataset=c('gebco_2022_sub_ice_topo','gebco_2022','gebco_2021_sub_ice_topo','gebco_2021','gebco_2020','gebco_2019')
  httr::set_config(httr::config(followlocation=TRUE, ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
  # check year 
  Y <- format(Sys.Date(), "%Y")
  # check for new dataset 
  #url <- paste0('https://www.bodc.ac.uk/data/open_download/gebco/gebco_',Y,'_sub_ice_topo/zip/')
  url <- 'https://uvaauas.figshare.com/ndownloader/files/52652597'
  # update link if there is no dataset for 2023
  if(httr::http_error(url)){ 
    url <- gsub(Y,as.numeric(Y)-1,url)
  } 
  return(url)
}


#' @title download_link_curve
#' 
#' @author Johannes De Groeve
#' @description return the download link of the spatio-temporal sea level curve (RSL)
#' 
#' @return a vector with the download link
#'
#' @noRd
#' 
#' @keywords internal 
#'
#' 
download_link_curve <- function(){
  httr::set_config(httr::config(followlocation=TRUE, ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
  # check if url exists
  url <- 'https://figshare.com/ndownloader/articles/20029991'
  if(httr::HEAD(url)$status == 401){
    url <- paste0(url,'?private_link=18773cb0b573add21388')
  }
  # if(!RCurl::url.exists(url)){ 
  # }
  return(url)
}

#' @title download_link_labs
#' 
#' @author Johannes De Groeve
#' @description return the download link of the labeling island data set 
#' 
#' @return a vector with the download link
#'
#' @noRd
#' 
#' @keywords internal 
#' 
#' 
download_link_labs <- function(source=F){
  if(source){
   url <- 'https://rmgsc.cr.usgs.gov/outgoing/ecosystems/Global/USGSEsriWCMC_GlobalIslands_v3.mpk'
  } else {
  httr::set_config(httr::config(followlocation=TRUE, ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
  # check if url exists
  #url <- 'https://uvaauas.figshare.com/ndownloader/files/39361361'
  url <- 'https://uvaauas.figshare.com/ndownloader/files/46942207'
  # if(httr::HEAD(url)$status == 502){
  #   url <- paste0(url,'?private_link=d8c204cfda92363b4377')
  # }
  #url2 <- 'https://uvaauas.figshare.com/ndownloader/articles/39361361'
  # if(!RCurl::url.exists(url)){
  #   #url <- paste0(url,'?private_link=4ea1241ef0aae19d7494')
  #   url <- paste0(url,'?private_link=d8c204cfda92363b4377')
  # }
  }
  return(url)
} 


#' @title download_link
#' 
#' @author Johannes De Groeve
#' @description return the download link of our default datasets 
#' 
#' @return a vector with the download link
#'
#' @noRd
#' 
#' @keywords internal
#'
#' 
download_link <- function(name=options()$tabs.datasetNames){
  if(length(name) > 1){
  name <- match.arg(name)
  }
  
  if(name=='curve'){
   url <- download_link_curve()
  }
  if(name=='topo'){
   url <- download_link_topo()
  }
  if(name=='labs'){
   url <- download_link_labs()
  }
  return(url) 
}

#' @title download default datasets 
#' 
#' @author Johannes De Groeve
#' @description download default datasets (labs, topo, curve)
#' 
#' @return a vector with the download link
#'
#' @noRd
#' 
#' @keywords internal 
#' 
download <- function(path=NULL, name=options()$tabs.datasetNames, autoupdate=FALSE, verbose=FALSE){
  
  # examples 
  # p_bat <- download(path="/Volumes/Extreme SSD/TEMP/",name='topo',verbose=T)
  # p_ref <- download(path="/Volumes/Extreme SSD/TEMP/",name='labs',verbose=T)
  # p_cur <- download(path="/Volumes/Extreme SSD/TEMP/",name='curve',verbose=T)
  # p_cur <- download(path="/Volumes/Extreme SSD/TEMP/",name='curve',verbose=T,autoupdate=T)
  # p_bc <- download(path="/Volumes/Extreme SSD/TEMP/",name=c('topo','curve'))
  
  # create structure of the data required for the package 
  path <- create_structure(path=path,name=name)
  # root path 
  root_path <- unique(dirname(path))
  if(as.numeric(verbose) > 1) message('root directory: ',root_path)
  # obtain the download link 
  url <- as.vector(unlist(lapply(path, function(x) {
    y <- basename(x)
    # download link
    url <- download_link(name=y)
    return(url)
  })))
  names(url) <- path
  
  #### LOOP = START ####
  
  # if update is true, update to the most recent version of the dataset 
    lapply(1:length(url) , function(x){
      # print url
      if(as.numeric(verbose) > 1) message('url: ',url[x]); message('path: ',names(url[x]))
      
      #### UPDATE AVAILABILITY ####
      # check for availability updates 
      if(!is.na(as.vector(options()$tabs.datasetSource[basename(names(url)[x])])))
        { # check if there is already a source path (is not NA)
        # obtain the current version and the last update made 
        version <- as.vector(options()$tabs.datasetSource[basename(names(url)[x])] )
        lastupdate <- as.Date(options()$tabs.datasetLastUpdate[basename(names(url)[x])])

        # check if the source has changed (update available)
        if(version == as.vector(url[x])){ # open - if url did not change
          # when link is not changing, recommend to update to obtain the newest 
          # available version (will not be the case for RSL, but might be for labs)
          days <- as.numeric(Sys.Date()  - lastupdate) 
          if(days >= 365 & !grepl('topo',names(url)[x])){ 
            message(version,' installed ',days, ' days ago [',lastupdate,']. ','\nSet autoupdate to true to update dataset') #TODO now update is recommended, not performed 
            # save in environmental variables, whether update recommendation is given [after 1 year]
            setupdate <- options()[paste0('tabs.datasetUpdate')]
            setupdate[[1]][basename(names(url)[x])] <- T
            options(setupdate)
            }
          } else { # else, recommend update if new version is available 
            message('A new version of the default ', basename(names(url[x])),' is available. \nSet autoupdate to true to update') #TODO now update is recommended, not performed 
            # save in environmental variables, whether an update is available 
            setupdate <- options()[paste0('tabs.datasetUpdate')]
            setupdate[[1]][basename(names(url)[x])] <- T
            options(setupdate)
          } # open - if the version is the same as before
        } #else { # if the source is na, we will need to update this info. 

      #### DOWNLOAD ####
      # if there are no files in the directory or if autoupdate is true
      if(length(list.files(names(url[x]))) == 0 | autoupdate){ # open - check if files exist or update is set to true

        #### remove datasets if updating ####
        if(autoupdate){
          files_to_remove <- list.files(names(url[x]),full.names = T)
          unlink(files_to_remove,recursive=TRUE)
          #file.remove(files_to_remove)
          if(as.numeric(verbose) > 1) message('remove data if autoupdate is true')
        }

        #### create temp_file in the defined root_path ####
        # if(.Platform$OS.type=='unix'){
        # temp_dir <- tempdir()
        # temp_file <- paste0(base::tempfile(pattern=paste0(basename(names(url[x]))),tmpdir=paste0(root_path,temp_dir)),'.zip') # MAYBE REMOVE
        # } else {
        temp_file <- paste0(base::tempfile(pattern=paste0(basename(names(url[x]))),tmpdir=paste(root_path,basename(tempfile()),sep='/')),'.zip') # MAYBE REMOVE 
        #}
        temp_dir <- dirname(temp_file)
        if(!dir.exists(temp_dir)) dir.create(dirname(temp_file),recursive=T)
        
        #### check if an url exists by evaluating the status message ####
        url_exists <- httr::HEAD(as.vector(url[x]))
        if(as.numeric(verbose) > 1){
        message(names(url_exists['status_code']),' ',
                url_exists$status_code,' ')
        }
        
        #### download and unzip if URL exists #### 
        if(url_exists$status_code %in% c(502,200,403)){ # open - only install if the URL exists 
          # download 
          #download.file(url=url[x], temp_file) # download
          
          bin <- httr::content(httr::GET(url[x],httr::progress()))
          # bin <- getBinaryURL(url,opts=list(followlocation=TRUE, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE))
          con <- file(temp_file, open = "wb")
          writeBin(bin, con)
          close(con)
          
          if(as.numeric(verbose) > 1) message('downloaded ',url[x])

          # unzip 
          unzip(zipfile=temp_file,exdir=names(url[x]),overwrite=autoupdate) # unzip 

          if(as.numeric(verbose) > 1) message('unzipped in temporary directory as ',names(url[x]))
            
          #### files that are still zipped need to be unzipped ####
          zip_files <- list.files(names(url[x]),full.names = T) # list files that are unzipped 
          if(length(zip_files) > 0) unlink(temp_dir,recursive=T)
          if(any(grepl('zip',zip_files))){ # open - check if there are more zips and unzip them 
                lapply(zip_files[grepl('zip',zip_files)], function(y) { 
                  exdir <- gsub('.zip','',y) # directory to unzip in 
                  unzip(zipfile=y, exdir=exdir, overwrite=autoupdate) # unzip files
                  if(dir.exists(exdir)) file.remove(y) # remove zips
              })
          } # close - check if there are more zips and unzip them 
            
          #### store version information ####
          # set source
          setsource <- options()[paste0('tabs.datasetSource')]
          setsource[[1]][basename(names(url)[x])] <- url[x]
          options(setsource)
          message('save source to tabs.datasetSource')
          # set update date
          setlastupdate <- options()[paste0('tabs.datasetLastUpdate')]
          setlastupdate[[1]][basename(names(url)[x])] <- as.character(Sys.Date())
          options(setlastupdate)
          message('save source to tabs.datasetLastUpdate')
          # set update 
          setupdate <- options()[paste0('tabs.datasetUpdate')]
          setupdate[[1]][basename(names(url)[x])] <- F
          options(setupdate)
          message('save source to tabs.datasetUpdate')   
          # set full path
          setdatasetpathfull <- options()[paste0('tabs.datasetFullPath')]
          setdatasetpathfull[[1]][basename(names(url)[x])] <- names(url[x])
          options(setdatasetpathfull)
          message('save source to tabs.datasetFullPath')
          } # close - only install if the URL exists 
     } # close - run if auto update is true, or if files do not exist yet 
  }) 
  #### LOOP = END ####
  update_options_config()
  
  return(url)
}

#' @title Setup - Download of default datasets
#' @description download default datasets (labs, topo, curve) in default or custom directory. The datasets require 15 GB disk space. 
#' 
#' @author Johannes De Groeve
#' 
#' @return No return value, called for side effects
#'
#' @export
#'
#' @examples
#' 
#' if(interactive()){
#' 
#' # download labs, topo and curve and store in custom or default directory
#' setup()
#' 
#' # reset setup
#' # if you want the install the datasets under a different path
#' options(tabs.datasetPath=NULL)
#' setup()
#' }
#' 
setup <- function(){
if(is.null(options()$tabs.datasetPath)){ # !dir.exists(options()$tabs.datasetDefaultPath) & 
  men <- menu(c(paste0("Default [data stored in ", options()$tabs.datasetDefaultPath,"]"), 
                paste0("User-defined [data stored in a user-defined directory]"))       
              , title="Install data [15 GB] or press 0 to exit without setup")
  if(men==0){ 
    message('run setup() or setup(datadir) to install data')
    setdatasetpath <- options()[paste0('tabs.datasetPath')]
    setdatasetpath[[1]] <- NA
    options(setdatasetpath)
  } else {
    if(men==1) datadir <- options()$tabs.datasetDefaultPath # if 1 set default path 
    if(men==2) datadir <- readline(prompt="Enter root directory: "); # if 2 provide directory
    
    if( dir.exists(datadir)) confirm <- readline(prompt=paste0(datadir,' exists, press enter to confirm or 0 to exit'))
    if(!dir.exists(datadir)) confirm <- readline(prompt=paste0(datadir,' does not exist, press enter to create this directory or 0 to exit'))
    
    if( nchar(datadir)!=0 & nchar(confirm)==0 ){
      lapply(options()$tabs.datasetNames, function(x) {
        download(path=datadir,name=x,autoupdate=F,verbose=F)
      })
    }
  }
} else {
  
  # check for updates
  n <-lapply(options()$tabs.datasetNames, function(x) {
    download(path=dirname(options()$tabs.datasetPath),name=x,autoupdate=F,verbose=F) # run downloads to check updates 
    update_available <- as.vector(options()$tabs.datasetUpdate[x])
    lastupdate <- as.Date(options()$tabs.datasetLastUpdate[x])
    if(update_available){ # only ask for update once and then wait one year 
      if(is.na(options()$tabs.datasetUpdateAsked[x])){
        update_request(name=x)
      } else {
        if(as.numeric(Sys.Date() - as.Date(options()$tabs.datasetUpdateAsked[x])) > 365){
        update_request(name=x)
        }
      }
      
    }
  })
}
update_options_config()

# # check if the datasets are present
# dir_path <- list.files(options()$tabs.datasetPath,full.names=T)
# v <- rep(NA,length(dir_path))
# names(v) <- basename(dir_path)
# for(i in 1:length(dir_path)){
#   if (length(list.files(dir_path[i])) == 0) {
#     v[i] <- FALSE 
#   } else {
#     v[i] <- TRUE
#   }
#   }
# return(v)

}



#' @title update_request
#' 
#' @author Johannes De Groeve
#' @description update request
#'
#' 
#' @return save options 
#'
#' @noRd
#' 
#' @keywords internal
#'
#' 
update_request <- function(name){
  men <- menu(c(paste0("Yes"), 
                paste0("No"))       
              , title=paste("update", name))
  setupdateasked <- options()[paste0('tabs.datasetUpdateAsked')]
  setupdateasked[[1]][name] <- as.character(Sys.Date())
  options(setupdateasked)
  #confirmupdate <- readline(prompt=paste0('Y to update ', x, ' or N to exit'))
  if(men==1){
    download(path=options()$tabs.datasetPath,name=name,autoupdate=T,verbose=F)
  } else {
    message('run setup() or setup(datadir) to update data manually')
  }
}



#' @title update_options_config
#' 
#' @author Johannes De Groeve
#' @description update options config 
#'
#' 
#' @return save options 
#'
#' @noRd
#' 
#' @keywords internal
#'
#' 
update_options_config <- function(){
  op <- options()
  # reset the extent option to FALSE at restart 
  reset <- lapply(op[grepl( 'tabs.current_extent',names(op))], 
             function(x) x <- FALSE)
  # load current options datasets
  my.options <- op[grepl( 'tabs.dataset',names(op))]
  my.options <- c(my.options,reset)
  # sae options to config 

  save(my.options, file=paste0(tools::R_user_dir('config'),"/.Roptions.Rdata"))
}


# atlas <- function(path=NULL, name=options()$tabs.datasetNames, autoupdate=FALSE, verbose=FALSE){
#   
#   
#   # download_gebco 
#   if('bathymetry' %in% name){
#     download(path=path,name='bathymetry',autoupdate=autoupdate,verbose=verbose)
#   }
#   
#   # download_labs
#   if('labs' %in% name){
#     download(path=path,name='labs',autoupdate=autoupdate,verbose=verbose)
#   }
#   
#   # download sea level curve 
#   if('curve' %in% name){
#     download(path=path,name='curve',autoupdate=autoupdate,verbose=verbose)
#   }
#   
# }



# 
# glottoget_remotemeta <- function(name = NULL, url = NULL){
#   rlang::check_installed("jsonlite", reason = "to use `glottoget_remotemeta()`")
#   rlang::check_installed("xml2", reason = "to use `glottoget_remotemeta()`")
#   if(is.null(name) & !is.null(url)){
#     base_url <- url
#   } else if(tolower(name) == "glottolog"){
#     # Newest version is always uploaded here!
#     base_url <- "https://zenodo.org/api/records/3260727"
#   } else if(tolower(name) == "wals"){
#     # Newest version is always uploaded here!
#     base_url <- "https://zenodo.org/api/records/3606197"
#   } else if(name == "dplace" | name == "d-place"){
#     base_url <- "https://zenodo.org/api/records/3935419"
#   } else if(!is.null(name) ){
#     stop("Unable to download data from Zenodo. Unrecognized name argument. ")
#   }
#   
#   remote <- suppressWarnings(jsonlite::stream_in(url(base_url)))
#   
#   version <- remote$metadata$version
#   btime <- utils::timestamp()
#   citation <- xml2::xml_text(xml2::read_html(charToRaw(remote$metadata$description), encoding = "UTF-8"))
#   
#   # v <- paste0(c("Version: ", version), collapse = "" )
#   # c <- paste0(c("Citation: ", gsub(pattern = "\n", replacement = " ", x = citation)), collapse = "" )
#   # b <- paste0(c("Built time: ", btime), collapse = "" )
#   #
#   # paste(c(v,c,b),  sep = "\n")
#   
#   metainfo <- paste0(c("version: ", version, "\n\n\n", citation, "\n\n", btime),  collapse = "")
#   
#   paste0("\\note{", metainfo, "}")
#   
# }
# 
# 


#' #' @title download_gebco
#' #' 
#' #' @author Johannes De Groeve
#' #' @description download bathymetry 
#' #'
#' #' @param path path where to save the bathymetric model 
#' #'
#' #' @return a data frame with the curve data in the correct format
#' #'
#' #' @export
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # download bathymetric model 
#' #' download_gebco()
#' #' }
#' #' 
#' download_gebco <- function(path=tools::R_user_dir("tabs", which = "data")){
#'   
#'   url <- download_link_gebco()
#'   destination <- paste0(path,'/gebco_', as.numeric(gsub("\\D", "", url)),'.zip')
#'   
#'   message(paste0('Downloading zip to ',destination,' - this might take some time'))
#'   download.file(url, destination)
#'   unzipdestination <- gsub('.zip','',destination)
#'   print(paste0('unzip GEBCO 2021 into ', unzipdestination))
#'   unzip(destination, exdir=unzipdestination)
#' } 
#' 
#' download_rsl <- function(path=tools::R_user_dir("tabs", which = "data")){
#'   
#'   url <- download_link_gebco()
#'   destination <- paste0(path,'/gebco_', as.numeric(gsub("\\D", "", url)),'.zip')
#'   
#'   message(paste0('Downloading zip to ',destination,' - this might take some time'))
#'   download.file(url, destination)
#'   unzipdestination <- gsub('.zip','',destination)
#'   print(paste0('unzip GEBCO 2021 into ', unzipdestination))
#'   unzip(destination, exdir=unzipdestination)
#' } 
#' 
#' 
#' 
#' atlas <- function(path=tools::R_user_dir("tabs", which = "data")){
#'   default <- path
#'   
#'   # download_gebco 
#'   download_gebco(path=default)
#'   
#'   # download_labs
#'   download_labs(path=default)
#'   
#'   # download sea level curve 
#'   download_curve(path=default)
#'   
#'   
#' }









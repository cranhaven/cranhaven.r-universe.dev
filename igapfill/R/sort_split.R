#' Console-based application to sort and split spatio-temporal chunks of images
#' 
#' An application of \code{\link[geoTS]{split_replace}} to split/divide/configure
#' images in parts to which a subsequent call of \code{\link[igapfill]{applyGapfill}} 
#' can be easily handled by regular computer systems.
#' 
#' This function asks the user a series of inputs on-the-fly. Should the user allow it, 
#' these inputs will be used as arguments in a subsequent call to \code{split_replace}.
#' 
#' @param               path character with full path name to a directory containing a set
#'                           of files to be split.
#' @param          startYear numeric indicating the starting time-point, on the annual scale,
#'                           of a time series of satellite images.
#' @param            endYear numeric indicating the ending time-point, on the annual scale,
#'                           of a time series of satellite images.
#' @param         nrow_split numeric, in how many equal parts the images' rows must be split? 
#'                           See \bold{Details.}
#' @param         ncol_split numeric, in how many equal parts the images' cols must be split?
#'                           See \bold{Details.}
#'                  
#' @export                  
#'  
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar                
#' @importFrom gtools mixedsort 
#' @importFrom terra rast
#' @importFrom terra ncol
#' @importFrom terra nrow
#' @importFrom terra ncell
#' @importFrom terra nlyr
#' @importFrom terra as.polygons
#' @importFrom terra ext
#' @importFrom terra writeRaster
#' @importFrom terra datatype
#' @importFrom terra add<-
#' @importFrom raster rasterOptions
#' @importFrom utils globalVariables
#' 
#' @details \code{\link[igapfill]{create_dirs}} defines a specific directory
#' structure used by \code{sort_split}, hence, it is highly recommended
#' to use \code{create_dirs} in advance. Also, it is highly recommended to use 
#' \code{sort_split} before using \code{\link[igapfill]{applyGapfill}}.
#' 
#' @note The "sort" part of the function means that in case that the file 
#' list created from \code{path} is not originally ordered, then \code{sort_split}
#' will sort it out internally.
#' 
#' @seealso \code{\link[igapfill]{waysToSplit}}, \code{\link[igapfill]{dimsReport}}, 
#' \code{\link[geoTS]{split_replace}}.
#' 
#' @return When the user decides not to proceed with this function,
#' a \code{NULL} is returned at the console. 
#' Otherwise, the resulting splits (.tif files) will be saved at the sub-directories 
#' defined by \code{paste0(path, "/gapfill/splits")} and a corresponding final _invisible_ 
#' message is displayed at the console.
#' 
sort_split <- function(path, startYear, endYear, nrow_split, ncol_split){
  
  yearsToFill <- startYear:endYear
  
  pathFILES <- mixedsort(list.files(path=path,
                                    pattern = ".tif",
                                    full.names = TRUE))
  
  if( length(pathFILES) == 0 ){
    stop(paste0(path, " must have some images to process"))
  }
  
  if(length(yearsToFill)^2 != length(pathFILES)){
    stop(paste0("Number of files in ", path, 
                " must be equal to length(startYear:endYear)^2"))
  }
  
  stackLIST <- list()
  p <- length(yearsToFill)
  for(k in 1:p){
    stackLIST[[k]] <- rast( pathFILES[1:p + (k-1)*p] )
  }
  
  if( !( missing(nrow_split) & missing(ncol_split) ) ){
    sort_split_core(path = path, startYear = startYear, endYear = endYear, 
                    nrow_split = nrow_split, ncol_split = ncol_split)
    
    output_dir <- paste0(path, "/gapfill/splits")
    output_dir_master <- paste0(path, "/gapfill/master")
    
  } else {
    
    message(colorText("The following files:", 80))
    
    for(i in 1:length(pathFILES)){
      cat(colorText( paste0( "(",i,") ", pathFILES[i] ), 159), "\n")
    }
    
    message( colorText("will be split under the following scheme", 80) )
    
    print_years_array(p=length(yearsToFill), years=yearsToFill)
    
    dimsReport(path = path, mes = "The images in these STACKs have:")
    
    message( colorText("How would you like to proceed?", 220) )
    
    v <- as.numeric(readline(justColorText("Parts to split nROW, v: ", 80) ))
    
    h <- as.numeric(readline(justColorText("Parts to split nCOL, h: ", 80)))
    
    waysToSplit(h=h,v=v, raster=stackLIST[[1]])
    
    response <- readline(justColorText("Would you like to proceed with these parameters? [Y/n]: ", 220))
    
    output <- NULL
    
    if(response == "yes" | response == "Yes" | response == "Y" | response == "y"){
      
      dir_response <- readline(justColorText("Should I use the directories suggested in my documentation to save outputs [Y/n]: ", 220))
      
      if(dir_response == "yes" | dir_response == "Yes" | dir_response == "Y" | dir_response == "y"){
        output_dir <- paste0(path, "/gapfill/splits")
        output_dir_master <- paste0(path, "/gapfill/master")
      } else {
        output_dir <- readline(justColorText("Please type in output directory (splits): ", 220))
        output_dir_master <- readline(justColorText("Please type in output directory (master): ", 220))
      }
      
      sort_split_core(path=path, startYear=startYear, endYear=endYear, nrow_split=v, ncol_split=h)
      
      output <- "message"
      
    }
    
  }
  
  
  if( is.null(output) ){
    out <- output
  } else {
    message(c(colorText("Done, check output at ", 216),
                   colorText(output_dir, 159)))
    
    out <- output
  }
  
  invisible(out)
  
}

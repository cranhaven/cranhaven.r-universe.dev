#' Save a vascr dataset
#'
#' @param data.df The vascr dataset to save
#' @param path The path to save the file to
#'
#' @returns A .vascr file containing a vascr dataset
#' 
#' @export
#'
#' @examples
#' path = tempfile()
#' vascr_save(growth.df, path = path)
#' 
vascr_save = function(data.df , path){
  #dataframe = deparse(substitute(data.df))
  #get(dataframe, envir = .GlobalEnv)
  save(data.df, file = path, compress = "xz")
}
  
#' Load a vascr dataset
#'
#' @returns A vascr dataset
#' 
#' @param path the path to a .vascr file containing the saved dataset
#' 
#' @export
#'
#' @examples
#' 
#' path = system.file("extdata/test.vascr", package = "vascr")
#' vascr_load(path)
#' 
vascr_load = function(path){
  data = load(path, verbose = TRUE)
  return(data)
  }


#' Wrapper for a function to dump errors from C++
#'
#' @param x Data to save
#' @param fname Path/name to save data to
#' 

errDump = function(x, fname=file.path(tempdir(), 'error_samplerState.RData')) { 
  save(x, file=fname) 
}
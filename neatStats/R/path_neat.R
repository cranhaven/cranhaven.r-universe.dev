#' @title Script Path
#'
#' @description Gives, in RStudio, the path to the script file in which it is
#'   executed.
#' @param subdir String, optional. Subdirectory relative to the script's path.
#' @return Script file's path as string. If \code{subdir} is given, it is
#'   appended to the original path.
#' @examples
#'
#' # assuming the given script is at path "C:/script_folder/"
#' path_neat('') # returns "C:/script_folder/"
#' path_neat('my_subdir/misc/') # returns "C:/script_folder/my_subdir/misc/"
#'
#' # Note: the returned string can be used as argument for base::setwd()
#' # e.g. setwd( path_neat() ) # sets working directory to the script's path
#'
#' @export
path_neat = function(subdir = '') {
    if (Sys.getenv("RSTUDIO") == "1") {
        validate_args(match.call(),
                      list(val_arg(subdir, c('char'), 1)))
        the_path = ''
        tryCatch({
            the_path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),
                              '/',
                              subdir)
        }, error = function(e) {
            message(e)
        })
        return(the_path)
    } else {
        message('This function works only in RStudio.')
    }
}


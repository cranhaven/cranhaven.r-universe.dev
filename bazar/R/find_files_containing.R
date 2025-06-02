
#' @importFrom utils file_test
#' 
find_files_containing <- 
function(pattern, 
         path, 
         recursive = FALSE, 
         ...)
{
  ell = list()
  d = dir(path)
  for (file in d) {
    
    f = file.path(path, file)
    w = integer(0L)
    
    ## Test if 'file' is a file and not a directory
    if (utils::file_test("-f", f)) {
      lines = readLines(f)
      w = which(grepl(pattern, lines, ...))
      
    } else if (utils::file_test("-d", f) && recursive) {
      w = Recall(pattern, f, recursive = recursive, ...)
    }
    
    if (length(w) > 0L) {
      ell[[file]] = w
    }
  }
  ell
}

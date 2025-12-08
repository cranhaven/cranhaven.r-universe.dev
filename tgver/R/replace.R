#' Function to replace patterns in given files.
#'
#'@param files character vector of full paths where pattern to be replaced.
#'@param pattern character pattern to replace using `gsub`.
#'@param replacement character to replace pattern with using `gsub`.
file_replace = function(files = NULL, pattern, replacement) {
  if(any(!file.exists(files)))
    stop("a given file path does not exist")
  if(!isSingleString(pattern) | !isSingleString(replacement))
    stop("'pattern' and 'replacement' must be single character values.")
  for(fname in files){
    content = readLines(fname, warn = FALSE)
    # g = grep(pattern, content)
    # if(length(g) == 0) cat(pattern, " not found in ", fname, "\n")
    content = gsub(pattern, replacement, content)
    writeLines(content, fname)
  }
}

#' Function to find what files may contain TGVE
#' API variables for functions like `file_replace`
#' to consume.
#'
#' @param path where TGVE instance is located.
list_api_files = function(path = NULL) {
  if(is.null(path) || !dir.exists(path))
    stop("valid tgve instance path is required.")
  # main.*.chunk.js*
  files = list.files(file.path(path, "static/js"),
                     pattern = "^main.*\\.js$",
                     full.names = TRUE)
  if(length(files) == 0)
    stop("could not find ^main* files in path.")
  files
}


#' Function to explore available API variables, their types and examples of
#' using them in the TGVE
#'
#' @return no object is returned
#'
#' @examples {
#' help()
#' }
#' @export
help = function() {
  message("These are the available list of variables as of version: ",
          tgver::version)
  print(names(apis))
}

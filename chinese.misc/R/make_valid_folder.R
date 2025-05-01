make_valid_folder <- function(x, return_null = TRUE, last_slash = TRUE) {
  if (is.null(x)) {
    if (return_null) {
      return(NULL)
    }
    else {
      stop("Folder name should not be NULL.")
    }
  }
  else {
    if (!is_character_vector(x, len = 1)) 
      stop("You must provide a length 1 character of valid folder name.")
    x <- whetherencode(x)
	x <- gsub("\\\\", "/", x)
    if (!dir.exists(x)) {
      x <- gsub("/+$|\\\\+$", "", x)
      tryCatch(expr = {
        dir.create(x, recursive = TRUE)
        message("Folder ", x, " is created ! !")
      }, error = function(e) {
        stop("Please provide a valid folder name.")
      })
    }
    if (last_slash) 
      x <- gsub("/+$|\\\\+$", "", x)
    return(x)
  }
}

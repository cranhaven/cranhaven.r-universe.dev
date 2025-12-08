#' Setup an instance.
#'
#' This function initializes an instance of TGVE for permanent use compared with
#' what \link[tgver]{tgve_server} which relies on a `tempdir` based instance.
#' It requires a path.
#'
#' @param path Character URI to copy tgve instance in.
#' @param create Boolean to create new directory at path, defaults to `TRUE`.
#'
#' @return no value returned
#'
#' @examples {
#' p = file.path(tempdir(), "tgve")
#' setup(p)
#' }
#' @export
setup = function(path = NULL, create = TRUE) {
  if(!is.character(path) || length(path) != 1) {
    stop("setup takes one character variable.")
  }

  if(dir.exists(path)) {
    d = file.path(path, "tgve")
    if(dir.exists(d)) {
      stop("directory named `tgve` exists at given path.")
    }
    copy_tgve(path)
    message("A TGVE instance has been created at: ", d)
  } else {
    pare.path = dirname(path)
    if(dir.exists(pare.path) && create) {
      dir.create(path)
      setup(path, create = FALSE)
    } else {
      stop("attempting to create destination parent directory failed.")
    }
  }
}

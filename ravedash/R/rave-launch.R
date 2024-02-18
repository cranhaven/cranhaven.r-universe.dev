#' Debug 'RAVE' modules interactively in local project folder
#' @param module_root root of modules, usually the project folder created from
#' \code{'shidashi'} template
#' @param host,port host and port of the application
#' @param jupyter whether to launch \code{'Jupyter'} server; default is false
#' @param ... passed to \code{\link[shidashi]{render}}
#' @return \code{'RStudio'} job ID
#' @export
debug_modules <- function(module_root = rstudioapi::getActiveProject(), host = '127.0.0.1', port = 17283, jupyter = FALSE, ...){

  options("rave.run.if_false" = FALSE)

  if(jupyter && rstudioapi::isAvailable()){
    jupyter_port <- raveio::raveio_getopt("jupyter_port", default = 17284)
    jupyter_wd <- raveio::raveio_getopt('data_dir')
    rpymat::jupyter_check_launch(open_browser = FALSE, workdir = jupyter_wd, port = jupyter_port, host = host, async = TRUE)
  }
  shidashi::render(root_path = module_root, port = port, host = host, ...)
}

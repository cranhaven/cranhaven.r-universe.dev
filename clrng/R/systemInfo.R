#' @title get_system_info
#' @description This function prints operating system, CPU, GPU, and OpenCL version information.
#' @return List of system information.
#' 
#' @useDynLib clrng
#' @export

get_system_info <- function() {
  .Call("_getSystemInfo")
}
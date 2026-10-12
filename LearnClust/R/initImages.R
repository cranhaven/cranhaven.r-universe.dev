#' @title To display an image.
#' @description An auxiliar function to display a picture.
#' @param path is a file path.
#' @export
#' @importFrom magick image_read

initImages <- function(path){
  img <- image_read(path)
  plot(img)
}

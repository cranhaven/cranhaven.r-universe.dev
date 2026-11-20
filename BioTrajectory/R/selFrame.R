#' Image Frame Selector and Viewer
#'
#' This function provides an interactive image frame viewer where the user can navigate through a sequence of images 
#' and select a specific one. The viewer uses buttons for navigation (previous and next) and an option to select an image.
#' The function uses the `rp.control` to create a window for the interface, and `grid` for displaying images.
#'
#' @param image_files A vector of file paths to the images that will be displayed in the viewer.
#'
#' @return The index of the selected image.
#'
#' @details
#' The function displays a sequence of images from the provided file paths, allowing the user to navigate between them 
#' using "Previous" and "Next" buttons. The images are displayed using `grid.raster` from the `grid` package. A "Select" button 
#' allows the user to select the current image, and the function returns the index of the selected image.
#'
#' @examples
#' \donttest{
#' # Not run:
#' path <- system.file('extdata/frames/', package='BioTrajectory')
#' image_files <- list.files(path, pattern = "\\.png$", full.names = TRUE)
#' selFrame(image_files)
#' }
#' 
#' @export

selFrame <- function(image_files) {
  # Function to display an image
  show_image <- function(index) {
    if (index > 0 && index <= length(image_files)) {
      img <- readPNG(image_files[index])
      grid::grid.raster(img)
    }
  }

  update_ui <- function(panel) {
    grid::grid.newpage()  
    show_image(panel$index)  
    panel
  }

  select_image <- function(panel) {
    print(panel$index)  
    panel
  }

  panel <- rp.control(title = "Image Viewer", size = c(400, 400))
  panel$index <- 1 
  
  rp.button(panel, action = function(panel) {
    if (is.null(panel$index)) panel$index <- 1
    panel$index <- max(1, panel$index - 1)  
    update_ui(panel) 
  }, title = "Previous")

  rp.button(panel, action = function(panel) {
    if (is.null(panel$index)) panel$index <- 1
    panel$index <- min(length(image_files), panel$index + 1)  
    update_ui(panel)  
  }, title = "Next")

  update_ui(panel)

  rp.button(panel, action = select_image, title = "Select")

  return(panel$index)
}

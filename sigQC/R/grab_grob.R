# grab_grob.R
#
# This function "grabs" the plots on the R graphics device, and enables them to be saved to an
# external file.
# @keywords grab_grob

grab_grob <- function(){
  # require(gridGraphics)
  # require(grid)
  gridGraphics::grid.echo()
  grid::grid.grab()
}

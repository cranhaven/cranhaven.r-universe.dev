#######################################

# LAPOP Visualization Templates #

#######################################
#' LAPOP Fonts (design)
#'
#' This function loads fonts needed for LAPOP graph formatting.  In contrast to lapop_fonts(),
#' this renders text as text instead of polygons, which allows post-hoc editing.
#'
#'@return No return value, called for side effects
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu}
#'
#'@import sysfonts
#'@import systemfonts
#'@import showtext
#'@export

 lapop_fonts_design <- function() {
   .lapop_register_fonts()
   showtext::showtext_auto(enable = FALSE)
   invisible(TRUE)
}

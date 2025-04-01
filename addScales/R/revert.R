## revert
## Reverts scaledTrellis object to its last unscaled version
revert <- function(obj,...) UseMethod("revert")

revert.scaledTrellis <- function(obj,...)
{
   orig <- obj$addScales$orig
   obj$panel <- orig$panel
   obj$legend <- orig$legend
   obj$addScales <- NULL
   class(obj) <- "trellis"
   obj
}

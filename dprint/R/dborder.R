#' Draw Border
#'
#' Draws a border style arround a section of table
#' 
#' @param cord1 Vector (x,y) indicating position of top left point of rectangle
#' @param cord2 Vector (x,y) indicating position of bottom right point of rectangle
#' @param frmt Format
#' @export
dborder <-
function(cord1,  # Vector (x,y) indicating position of top left point of rectangle
                  cord2,  # Vector (x,y) indicating position of top left point of rectangle
                  frmt    #
                 )
{
  ### Prints a box - suppresse lines based on symbol ###
  if (frmt$bty %in% c("=", "o", "-"))
  {# TOP Line
    grid.lines( x = unit(c(cord1[1], cord2[1]), "inches"),
                y = unit(cord1[2], "inches"),
                gp = gpar(col = frmt$lcol, fontsize = frmt$fontsize, cex = frmt$cex, lwd = frmt$lwd, lty=frmt$lty),
                draw = TRUE)
  }
  if (frmt$bty %in% c("=","o", "_"))
  { # BOTTOM Line
    grid.lines( x = unit(c(cord1[1], cord2[1]), "inches"),
                y = unit(cord2[2], "inches"),
                gp = gpar(col = frmt$lcol, fontsize = frmt$fontsize, cex = frmt$cex, lwd = frmt$lwd, lty=frmt$lty),
                draw = TRUE)
  }

  if (frmt$bty %in% c("o"))
  {# LEFT Line
    grid.lines( x = unit(cord1[1], "inches"),
                y = unit(c(cord1[2], cord2[2]), "inches"),
                gp = gpar(col = frmt$lcol, fontsize = frmt$fontsize, cex = frmt$cex, lwd = frmt$lwd, lty=frmt$lty),
                draw = TRUE)
  }
  if (frmt$bty %in% c("o"))
  {# RIGHT Line
    grid.lines( x = unit(cord2[1], "inches"),
                y = unit(c(cord1[2], cord2[2]), "inches"),
                gp = gpar(col = frmt$lcol, fontsize = frmt$fontsize, cex = frmt$cex, lwd = frmt$lwd, lty=frmt$lty),
                draw = TRUE)
  }
 }


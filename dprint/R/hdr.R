#' Header
#'
#' Header
#'
#' @param txt1 Text 1
#' @param frmt1 Format 1
#' @param txt2 Text 2
#' @param frmt2 Format 2
#' @param pagelayout.obj Layout Object
#' @export
hdr <-
function(txt1,
                frmt1=frmt(fontfamily="", fontface="bold", fontsize=20, col="blue", bty="_", lwd=2, linespace=1),
                txt2=NA, # oPTIONAL, see as being used for indicating CONFIDENTIAL document justified right, in upper right hand corner
                frmt2=frmt(fontfamily="", fontface="bold", fontsize=16, col="red", bty="_", lwd=2, linespace=1),
                pagelayout.obj=pagelayout(dtype="portrait", margins=c(1, .5)) # Header is printed in reference to the margins of this function
                )
{

  txt1.struct <- vector.struct(txt1)
  # Calculate character height and
  ch1  <- char.height("A", frmt=frmt1, cx=1)
  linespace1  <- frmt1$linespace*ch1

  y.rem <- y.loc <- pagelayout.obj$page.height
  for (txt1.dx in 1:txt1.struct$vctr.nrw)
  {
    y.loc <- y.loc -  pagelayout.obj$margins[3] + (linespace1)*txt1.dx
    grid.text(txt1.struct$vctr[txt1.dx],
                    just = "left",
                    gp = gpar(fontfamily = frmt1$fontfamily, fontface = frmt1$fontface, fontsize = frmt1$fontsize, cex = frmt1$cex, col = frmt1$col),
                    x = unit(pagelayout.obj$cord.tl[1], "inches"), y = unit(y.loc, "inches"))
  }
  grid.lines(x = unit(c(pagelayout.obj$margins[2], pagelayout.obj$page.width-pagelayout.obj$margins[4]), "inches"),
             y = unit(pagelayout.obj$page.height-pagelayout.obj$margins[3], "inches"),
             gp = gpar(col = frmt1$lcol, fontsize = frmt1$fontsize, cex = frmt1$cex, lwd = frmt1$lwd, lty=frmt1$lty),
                draw = TRUE)

  if (!is.na(txt2))
  {
    txt2.struct <- vector.struct(txt2)
    ch2         <- char.height("A", frmt=frmt2, cx=1)
    linespace2  <- frmt2$linespace*ch2
    y.loc <- y.rem
    for (txt2.dx in 1:txt2.struct$vctr.nrw)
      {
        y.loc <- y.loc -  pagelayout.obj$margins[3] + (linespace2)*txt2.dx
        grid.text(txt2.struct$vctr[txt2.dx],
                  just = "right",
                  gp = gpar(fontfamily = frmt2$fontfamily, fontface = frmt2$fontface, fontsize = frmt2$fontsize, cex = frmt2$cex, col = frmt2$col),
                  x = unit(pagelayout.obj$page.width-pagelayout.obj$margins[4], "inches"), y = unit(y.loc, "inches"))

      }
  }
}


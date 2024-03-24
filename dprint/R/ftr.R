#' Footer
#'
#' dprint is to be designed so that users can define custom functions to present footer on page.  This is the out of box footer.
#'
#' @param txt1 A vector of text to be placed on bottom right of footer
#' @param frmt1 style sheet data type frmt for text on bottom left
#' @param date  Boolean, should today's date be placed in bottom center of page
#' @param pagelayout.obj Tells footer what type of page dprint is working with.  Has separate margins to allow for extra space between table presentation
#' @param pgtxt2 Text to be appended to page number. To suppress numbering, make NULL
#' @param pagenum Didn't want parameter here, I wanted pagenum to be passed down the calls stack "eval.parent(pagenum, 1)" here is the only way I could get to work ~ carlin
#' @export
#'
#' @examples
#' \donttest{
#' longtable1 <- rdesc(15, 7)
#' longtable2 <- rdesc(7, 4)
#' h <- expression(hdr("Multiple Page Report",
#'                 pagelayout.obj=pagelayout(dtype="landscape", margins=c(.75, .5))))
#' f <- expression(ftr("R Package tabulaR",
#'                 pagelayout.obj=pagelayout(dtype="landscape", margins=c(.75, .5))
#'                 , pagenum=eval.parent(pagenum, 1)
#'                 ))
#'                
#'  pdf("longtable1.pdf", height=8.5, width=11)
#'     dp <- dprint(fmla= group+level~ `This is a Control`:(Mean1 + Variance1) +
#'     Treatment:(Mean2 + Variance2)+p.value,
#'           data=longtable1, showmargins=TRUE, dtype="landscape",
#'           newpage=TRUE, pagenum=1, margins=1,
#'           f.hdr=h, f.ftr=f
#'           )
#'    
#'    dprint(fmla= group+level~ `This is a Control`:(Mean1 + Variance1) + 
#'    Treatment:(Mean2 + Variance2)+p.value,
#'           data=longtable2, showmargins=TRUE, dtype="landscape",
#'           newpage=TRUE, lastcall=dp, # Pick up with page numbering
#'           margins=1,
#'           f.hdr=h, f.ftr=f
#'           )
#'   x <- rnorm(100)
#'   y <- rnorm(100)
#'   f2 <- expression(ftr("R Package tabulaR",
#'                 pagelayout.obj=pagelayout(dtype="landscape", margins=c(.5, .5))
#'                 , pagenum=dp$pagenum+1
#'                 )) }
#'   # par(mai=c(2,3,2,3))
#'   \donttest{plot(x,y, main="Scatter Plot X vs. Y")
#'   eval(f2)
#'   eval(h)}
#'   \donttest{par(mfcol=c(1,2), pty="s", bg="grey", mai=c(1,1,1,1))
#'   plot(density(x), "Distribution of X", xlab="x", bg="blue")
#'   plot(x,y, main="Scatter Plot X vs. Y")
#'   eval(f2)
#'   eval(h)
#'   dev.off()}

ftr <-
function(txt1,
            frmt1=frmt(fontfamily="", fontface="plain", fontsize=8, col="black", linespace=.75),
            date = TRUE,
            pagelayout.obj=pagelayout(dtype="portrait", margins=c(1, .5)), # Header is printed in reference to the margins of this function
            pgtxt2 = "page" #NULL, #"page", # Text appended to page number
            , pagenum=NULL
              )
{

  txt1.struct <- vector.struct(txt1)
  # Defaults
  if (is.null(frmt1$fontfamily)) {frmt1$fontfamily=""}
  if (is.null(frmt1$fontface))   {frmt1$fontface="plain"}
  if (is.null(frmt1$fontsize))   {frmt1$fontsize=8}
  if (is.null(frmt1$col))        {frmt1$col="black"}
  if (is.null(frmt1$linespace))  {frmt1$linespace=.75}
   
  # Calculate character height and
  ch1  <- char.height("A", frmt=frmt1, cx=1)
  linespace1  <- frmt1$linespace*ch1

  y.rem <- y.loc <- pagelayout.obj$margins[1]
  for (txt1.dx in 1:txt1.struct$vctr.nrw)
  {
    y.loc <- y.loc -  (linespace1)*txt1.dx
    grid.text(txt1.struct$vctr[txt1.dx],
                    just = "left",
                    gp = gpar(fontfamily = frmt1$fontfamily, fontface = frmt1$fontface, fontsize = frmt1$fontsize, cex = frmt1$cex, col = frmt1$col),
                    x = unit(pagelayout.obj$cord.tl[1], "inches"), y = unit(y.loc, "inches"))
  }

  y.rem <- y.loc <- pagelayout.obj$margins[1]
  if (date)
  {
    y.loc <- y.rem -  (linespace1)*txt1.dx
    grid.text(format(Sys.Date(), "%m/%d/%Y"),
              just = "right",
              gp = gpar(fontfamily = frmt1$fontfamily, fontface = frmt1$fontface, fontsize = frmt1$fontsize, cex = frmt1$cex, col = frmt1$col),
              x = unit(pagelayout.obj$page.width-pagelayout.obj$margins[4], "inches"), y = unit(y.loc, "inches"))
  }

  y.rem <- y.loc <- pagelayout.obj$margins[1]
  if (!is.null(pgtxt2))
  {
    y.loc <- y.rem -  2*(linespace1)
    grid.text(paste(pgtxt2, "\n", pagenum, sep=""),
              just = "center",
              gp = gpar(fontfamily = frmt1$fontfamily, fontface = frmt1$fontface, fontsize = frmt1$fontsize, cex = frmt1$cex, col = frmt1$col),
              x = unit(pagelayout.obj$page.width/2, "inches"), y = unit(y.loc, "inches"))
  }

}


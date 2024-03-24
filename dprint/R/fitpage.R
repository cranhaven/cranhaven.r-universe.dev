#' Fit Table to Page
#'
#' Calcuates character expansion to apply accross table presentation so that table fits within margins
#'
#' @param size.simp size object
#' @param pagelayout dimensions of page
#' @param fit.width boolean, force to fit to width
#' @param fit.height boolean, force to fit height
#' @param fit boolean, force to fit minimum of height and wdith
#' @param cex.0 default cex
#' @export
fitpage <-
function(size.simp,
                    pagelayout,
                    fit.width=FALSE,
                    fit.height=FALSE,
                    fit=FALSE,
                    cex.0            # default cex
                    )
{
  cex.width  <- (pagelayout$page.width -sum(pagelayout$margins[c(2,4)]))/size.simp$tbl.width
  cex.height <- (pagelayout$page.height-sum(pagelayout$margins[c(1,3)]))/size.simp$tbl.height.mf
  cntrbuf.horz <- (pagelayout$page.width -sum(pagelayout$margins[c(2,4)])-size.simp$tbl.width)/2
  cntrbuf.vert <- (pagelayout$page.height-sum(pagelayout$margins[c(1,3)])-size.simp$tbl.height.mf)/2

  if (fit.width&fit.height|fit) {cex.fit<-min(cex.width, cex.height)}
  else if (fit.width)           {cex.fit<-cex.width}
  else if (fit.height)          {cex.fit<-cex.height}
  else                          {cex.fit<-cex.0}

  return(list(cex.fit=cex.fit,cntrbuf.horz=cntrbuf.horz, cntrbuf.vert=cntrbuf.vert))
}


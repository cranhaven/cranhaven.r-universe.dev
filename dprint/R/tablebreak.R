#' Table Break
#'
#' Given an index, breaks table into multiple tables
#'
#' @param tbl.obj.l list of table objects
#' @param cur.tbl Index to current table
#' @param dx  Row to break table on
#' @export
tablebreak <-
function(tbl.obj.l,  # list of table objects
                       cur.tbl,    # Index to current table
                       dx          # Row to break table on
                       )
{
  tbl.obj1 <- tbl.obj2 <- NULL;
  if (!is.null(dx)) # Handle when no breakneeded here
  {
    # These structures stay the same
    tbl.obj1[[1]]$main <-  tbl.obj2[[1]]$main <-tbl.obj.l[[cur.tbl]]$main
    tbl.obj1[[1]]$main.nrw <- tbl.obj2[[1]]$main.nrw <- tbl.obj.l[[cur.tbl]]$main.nrw
    tbl.obj1[[1]]$cond.txt <- tbl.obj2[[1]]$cond.txt <- tbl.obj.l[[cur.tbl]]$cond.txt
    tbl.obj1[[1]]$ncl <- tbl.obj2[[1]]$ncl <- tbl.obj.l[[cur.tbl]]$ncl
    tbl.obj1[[1]]$colhead <- tbl.obj2[[1]]$colhead <- tbl.obj.l[[cur.tbl]]$colhead
    tbl.obj1[[1]]$nrw.colhead <- tbl.obj2[[1]]$nrw.colhead <- tbl.obj.l[[cur.tbl]]$nrw.colhead
    tbl.obj1[[1]]$footnote <- tbl.obj2[[1]]$footnote <- tbl.obj.l[[cur.tbl]]$footnote
    tbl.obj1[[1]]$footnote.nrw <- tbl.obj2[[1]]$footnote.nrw <- tbl.obj.l[[cur.tbl]]$footnote.nrw
  
    # These row strucutres need to be adjusted
    dx1 <- 1:dx
    dx2 <- (dx+1):(tbl.obj.l[[cur.tbl]]$nrw)
  
    tbl.obj1[[1]]$nrw <- dx
    tbl.obj2[[1]]$nrw <- tbl.obj.l[[cur.tbl]]$nrw - dx
  
    if (!is.null(tbl.obj.l[[cur.tbl]]$group[1]))
      {
        tbl.obj1[[1]]$group       <-  tbl.obj.l[[cur.tbl]]$group[dx1]
        tbl.obj2[[1]]$group       <-  tbl.obj.l[[cur.tbl]]$group[dx2]
        tbl.obj1[[1]]$newgrp      <-  tbl.obj.l[[cur.tbl]]$newgrp[dx1]
        tbl.obj2[[1]]$newgrp      <-  tbl.obj.l[[cur.tbl]]$newgrp[dx2]
        tbl.obj1[[1]]$lastofgroup <-  tbl.obj.l[[cur.tbl]]$lastofgroup[dx1]
        tbl.obj2[[1]]$lastofgroup <-  tbl.obj.l[[cur.tbl]]$lastofgroup[dx2]
        tbl.obj1[[1]]$newgrp.dx   <-  tbl.obj.l[[cur.tbl]]$newgrp.dx[tbl.obj.l[[cur.tbl]]$newgrp.dx <= dx]
        tbl.obj2[[1]]$newgrp.dx   <-  tbl.obj.l[[cur.tbl]]$newgrp.dx[tbl.obj.l[[cur.tbl]]$newgrp.dx > dx] - dx
        tbl.obj1[[1]]$newgrp.qdx  <-  tbl.obj.l[[cur.tbl]]$newgrp.qdx[tbl.obj.l[[cur.tbl]]$newgrp.dx <= dx]
        tbl.obj2[[1]]$newgrp.qdx  <-  tbl.obj.l[[cur.tbl]]$newgrp.qdx[tbl.obj.l[[cur.tbl]]$newgrp.dx > dx] - max(tbl.obj1[[1]]$newgrp.dx)
      }
    else
      {
        tbl.obj1[[1]]$group <-  tbl.obj2[[1]]$group <- NULL;
        tbl.obj1[[1]]$newgrp <- tbl.obj2[[1]]$newgrp <- NULL;
        tbl.obj1[[1]]$lastofgroup <- tbl.obj2[[1]]$lastofgroup <- NULL;
        tbl.obj1[[1]]$newgrp.dx <- tbl.obj2[[1]]$newgrp.dx <-  NULL;
        tbl.obj1[[1]]$newgrp.qdx <-  tbl.obj2[[1]]$newgrp.qdx <-  NULL;
      }
    if (!is.null(tbl.obj.l[[cur.tbl]]$label[1]))
      {
        tbl.obj1[[1]]$label <-  tbl.obj.l[[cur.tbl]]$label[dx1]
        tbl.obj2[[1]]$label <-  tbl.obj.l[[cur.tbl]]$label[dx2]
      }
    else {tbl.obj1[[1]]$label <- tbl.obj2[[1]]$label <- NULL}
  
    if (length(tbl.obj.l[[cur.tbl]]$lblescp.dx))
      { DD <- 'A'
        # [[!!!insert code here!!!]]
      }
    tbl.obj1[[1]]$bdy <-  tbl.obj.l[[cur.tbl]]$bdy[dx1, ,drop=FALSE]
    tbl.obj2[[1]]$bdy <-  tbl.obj.l[[cur.tbl]]$bdy[dx2, ,drop=FALSE]
    if (!is.null(tbl.obj.l[[cur.tbl]]$row.hl$dx) )
      {
         tbl.obj1[[1]]$row.hl$dx <-  tbl.obj.l[[cur.tbl]]$row.hl$dx[tbl.obj.l[[cur.tbl]]$row.hl$dx <= dx]
         tbl.obj2[[1]]$row.hl$dx <-  tbl.obj.l[[cur.tbl]]$row.hl$dx[tbl.obj.l[[cur.tbl]]$row.hl$dx > dx] - dx
         tbl.obj1[[1]]$row.hl$col <- tbl.obj2[[1]]$row.hl$col <- tbl.obj.l[[cur.tbl]]$row.hl$col
      }
    else {tbl.obj1[[1]]$row.hl$dx <- tbl.obj2[[1]]$row.hl$dx <- tbl.obj1[[1]]$row.hl$col <- tbl.obj2[[1]]$row.hl$col <- NULL;}
  
    ## Reconstruct List of Table objects ##
    if (length(tbl.obj.l)==1)              {tbl.obj.l <- c(tbl.obj1, tbl.obj2)}
    else if (cur.tbl == 1)                 {tbl.obj.l <- c(tbl.obj1, tbl.obj2, tbl.obj.l[2:length(tbl.obj.l)])}
    else if (cur.tbl == length(tbl.obj.l)) {tbl.obj.l <- c(tbl.obj.l[1:(cur.tbl-1)], tbl.obj1, tbl.obj2)}          # 1/06/2011 changed from cut.tbl to cur.tbl ???
    else                                   {tbl.obj.l <- c(tbl.obj.l[1:(cur.tbl-1)], tbl.obj1, tbl.obj2, tbl.obj.l[(cur.tbl+1):(length(tbl.obj.l))])}
  }
  return(tbl.obj.l)
}


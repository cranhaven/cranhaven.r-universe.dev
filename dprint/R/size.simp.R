#' Size of Simple Table
#'
#' Measures size of properties of the simple table
#'
#' @param tbl.obj Table Object
#' @param char.dim.obj Size of character given formats
#' @param pagelayout Page dimensions
#' @param loc.y Y-coordinate reference
#' @export
size.simp <-
function(tbl.obj, char.dim.obj, pagelayout, loc.y)
{
  # see if label and group were defined for number of columns to allocate: want to be 0 or 1
  nlbl  <- min(1, length(tbl.obj$label))
  ngrp  <- min(1, length(tbl.obj$group))
  nmain <- max(c(0, tbl.obj$main.nrw), na.rm=T)
  nfoot <- max(c(0, tbl.obj$footnote.nrw+.5), na.rm=T)
  main.height     <- nmain*char.dim.obj$linespace.main
  footnote.height <- nfoot*char.dim.obj$linespace.ftn

  # To calculate the height of the current table Number of rows for each section times character height
  tbl.height    <- tbl.obj$nrw.colhead*char.dim.obj$linespace.col + tbl.obj$nrw*char.dim.obj$linespace.bdy
  tbl.height.m  <- main.height +  tbl.obj$nrw.colhead*char.dim.obj$linespace.col + tbl.obj$nrw*char.dim.obj$linespace.bdy
  tbl.height.mf <- main.height +  tbl.obj$nrw.colhead*char.dim.obj$linespace.col + tbl.obj$nrw*char.dim.obj$linespace.bdy + footnote.height
  tbl.width     <- char.dim.obj$cw.row.u + char.dim.obj$bdy.xloc[length(char.dim.obj$bdy.xloc)]

  line.height     <- c(rep(char.dim.obj$linespace.main, nmain), rep(char.dim.obj$linespace.col,tbl.obj$nrw.colhead), rep(char.dim.obj$linespace.bdy, tbl.obj$nrw) + rep(char.dim.obj$linespace.footnote, nfoot))
  cumheight.bdy <- cumsum(rep(char.dim.obj$linespace.bdy, tbl.obj$nrw)) + sum(c(rep(char.dim.obj$linespace.main, nmain), rep(char.dim.obj$linespace.col,tbl.obj$nrw.colhead), rep(char.dim.obj$linespace.footnote, nfoot)))

  fitspage <- cumheight.bdy <= (loc.y - pagelayout$margins[3])
  newpage=FALSE;
  tablebreak.dx = NULL;
  nothingfits <- FALSE

  if (sum(fitspage) < tbl.obj$nrw) # table will not fit on page
    {
      if (sum(fitspage)==0) {nothingfits <- TRUE}
      else if (!is.null(tbl.obj$group[1]))  # If grouping was defined keep groups together when possible
        {
          fitspage.grp.dx <- which(fitspage & tbl.obj$lastofgroup)
          if      (length(fitspage.grp.dx) > 0)
            {
             tablebreak.dx <- fitspage.grp.dx[length(fitspage.grp.dx)];
             newpage=TRUE
            }
          #else if (call2) {tablebreak.dx <- which(fitspage)[1]; newpage=TRUE}  
          #  After page is resetM, IF group is too, large break rule
          else  {tablebreak.dx <- which(fitspage)[length(which(fitspage))]
                 # THese two options have different behaviors, not sure which should be the default, mayber antohe parameter
                 #nothingfits <- TRUE
                 newpage <- TRUE
                 } #  Reset page and see if group works
        }
      else # Assume no group and fit rows to page
        {
          tablebreak.dx <- which(fitspage)[length(which(fitspage))]
          newpage <- TRUE
        }
      }

  return(list(tbl.height=tbl.height,           # table height (EXlcuding main/footnote)
              tbl.height.m=tbl.height.m,       # table height Including footnote
              tbl.height.mf=tbl.height.mf,     # table height Including main and footnote
              tbl.width=tbl.width,
              line.height=line.height,         # The height of each line in a vector,
              cumheight.bdy=cumheight.bdy,     # The height of each line in a vector
              nothingfits=nothingfits,
              tablebreak.dx=tablebreak.dx      # Rownumber for table to be split upon
              ))
}


#' Print Table to Graphics Device (dprint)
#'
#' drpint "simple table" class (to be implemented)
#' 
#' @param tbl.obj Simple Table object
#' @param init Starting Position of Table
#' @param style style object
#' @param char.dim.obj character dimension object
#' @param size.simp.obj size of text object passed to grid
#' @export
dprint_simp <-
function(tbl.obj,          # Simple Table object
                        init = c(0, 6.9), # Starting Position of Table
                        style,
                        char.dim.obj,
                        size.simp.obj
                        )
{
  # For sections Table Title, Colum heading, body, footnote, I want to update a global parameter that tells where the last section left off. So updates are contaiend it one variable
  y.loc <- y.rem <- init[2]
  # If missing  make 0
  main.nrw <- ifelse(is.na(tbl.obj$main.nrw), 0, tbl.obj$main.nrw)

  # Calculate how big the table is going to be
  #size.simp.obj <- size.simp(tbl.obj, char.dim)

  # --------------------------- #
  ###    Print Table Title    ###
  # --------------------------- #
  if (!is.na(tbl.obj$main.nrw))
    { # If there is conditinal text, append it to the last position of main vector
      if (!is.null(tbl.obj$cond.txt)) {tbl.obj$main[tbl.obj$main.nrw] <- paste(tbl.obj$main[tbl.obj$main.nrw], tbl.obj$cond.txt, sep="")}
      for (main.dx in 1:tbl.obj$main.nrw)
        {#                                                              a little extra space for lines background
          y.loc <- y.rem - char.dim.obj$linespace.main*(main.dx-.5) + .25*char.dim.obj$linespace.col
          grid.text(tbl.obj$main[main.dx],
                    just = "left",
                    gp = gpar(fontfamily = style$frmt.main$fontfamily, fontface = style$frmt.main$fontface,  fontsize = style$frmt.main$fontsize, cex = style$cex, col = style$frmt.main$col),
                    x = unit(init[1], "inches"), y = unit(y.loc, "inches"))

        }
      y.rem <- y.rem - char.dim.obj$linespace.main*(main.dx-.5) # update overall y location, without column addition
    }
  else {y.rem <- y.rem + .5*char.dim.obj$linespace.col}  # When no title, column headings will start excatly on top margin

  ### Conditional row highlight ###
  # Must be before any text or borders for correct effect
  for (y.dx in 1:tbl.obj$nrw)
    { #
      #y.loc <-  y.rem - char.dim.obj$linespace.col*tbl.obj$nrw.colhead - char.dim.obj$linespace.bdy*y.dx
      y.loc <-  y.rem - char.dim.obj$linespace.col*(tbl.obj$nrw.colhead + .25) - char.dim.obj$linespace.bdy*y.dx
      if (!is.null(row.hl))
        {  if (y.dx %in% tbl.obj$row.hl$dx)
            { grid.rect(x = unit(init[1],"inches"), y = unit(y.loc, "inches"),
                        height = unit(char.dim.obj$linespace.bdy, "inches"), width = unit(size.simp.obj$tbl.width, "inches"),
                        just = "left", gp = gpar(col = tbl.obj$row.hl$col, fill = tbl.obj$row.hl$col, fontsize = style$frmt.bdy$fontsize, cex = style$cex))
            }
        }
    }
  # --------------------------- #
  ###  Prints Column Headings ###
  # --------------------------- #
  ### Column Heading Background ###
  # X, From begining left of table with width of table
  # Y, Centered around middle of column heading with height as the number of of columnheading rows ***.125
  grid.rect(x = unit(init[1],"inches"), y = unit(init[2]-(main.nrw)*char.dim.obj$linespace.main-.5*char.dim.obj$linespace.col*(tbl.obj$nrw.colhead-.25), "inches"),
            height = unit(char.dim.obj$linespace.col*(tbl.obj$nrw.colhead+.125), "inches"), width = unit(size.simp.obj$tbl.width, "inches"),
            just = "left", gp = gpar(col = style$frmt.col$bg, fill = style$frmt.col$bg, fontsize = style$frmt.col$fontsize, cex = style$cex))
  for (col.dx in 1:nrow(tbl.obj$colhead))
    {
      col.y.loc <- y.rem - char.dim.obj$linespace.col*tbl.obj$colhead[col.dx, "row"]
      # X location and justification is different for column hiearchies than for rest of table
      if (tbl.obj$colhead[col.dx, "col.logical.row"]==1) # Lowest column name descriptor
        { # This uses the bdy.xloc
          col.x.loc <- init[1] + char.dim.obj$cw.row.u +
                        char.dim.obj$bdy.xloc[tbl.obj$colhead[col.dx, "span.beg"]] +
                        .5*(char.dim.obj$bdy.xloc[tbl.obj$colhead[col.dx, "span.end"]]-char.dim.obj$bdy.xloc[tbl.obj$colhead[col.dx, "span.beg"]])
          j <- style$justify
        }
      else # Column Hiearchy printing and formatting
        { # This uses the colhead.xloc
          col.x.loc <- init[1] + char.dim.obj$cw.row.u +
                        char.dim.obj$colhead.xloc[tbl.obj$colhead[col.dx, "span.beg"]] +
                        .5*(char.dim.obj$colhead.xloc[tbl.obj$colhead[col.dx, "span.end"]]-char.dim.obj$colhead.xloc[tbl.obj$colhead[col.dx, "span.beg"]])
         j <- "center"
        ### Column Hiearchy Border ###
        if (style$frmt.colh$bty%in%c("o")) # border entire column
          {
            dborder(c(init[1]+char.dim.obj$cw.row.u+char.dim.obj$colhead.xloc[tbl.obj$colhead[col.dx, "span.beg"]]-.5*char.dim.obj$cw.bdy.u[tbl.obj$colhead[col.dx, "span.beg"]],
                        col.y.loc+.625*char.dim.obj$linespace.col),
                    c(init[1]+char.dim.obj$cw.row.u+char.dim.obj$colhead.xloc[tbl.obj$colhead[col.dx, "span.end"]]+.5*char.dim.obj$cw.bdy.u[tbl.obj$colhead[col.dx, "span.end"]],
                       init[2]-size.simp.obj$tbl.height.m-.25*char.dim.obj$linespace.col), frmt=style$frmt.colh)
          }
        if (style$frmt.colh$bty=="_") # underline column hiearchy name
          {
            dborder(c(init[1]+char.dim.obj$cw.row.u+char.dim.obj$colhead.xloc[tbl.obj$colhead[col.dx, "span.beg"]],#-.5*char.dim.obj$cw.bdy.u[tbl.obj$colhead[col.dx, "span.beg"]],
                        col.y.loc+.5*char.dim.obj$linespace.col),
                    c(init[1]+char.dim.obj$cw.row.u+char.dim.obj$colhead.xloc[tbl.obj$colhead[col.dx, "span.end"]],#+.5*char.dim.obj$cw.bdy.u[tbl.obj$colhead[col.dx, "span.end"]],
                      col.y.loc-.5*char.dim.obj$linespace.col), frmt=style$frmt.colh)
          }

        }
        ### PRINT COLUMN NAMES ###
        grid.text(tbl.obj$colhead[col.dx, "cname"],
            just = j,
            gp = gpar(fontfamily = style$frmt.col$fontfamily, fontface = style$frmt.col$fontface,  fontsize = style$frmt.col$fontsize, cex = style$cex, col = style$frmt.col$col),
            x = unit(col.x.loc, "inches"), y = unit(col.y.loc, "inches"))
    }
  # Update Global y.loc variable                                  ***.25
  y.rem <- y.rem - char.dim.obj$linespace.col*(tbl.obj$nrw.colhead + .25)
  ### Column Heading Border ###
  dborder(c(init[1], init[2]-main.nrw*char.dim.obj$linespace.main + .25*char.dim.obj$linespace.col),
          c(init[1]+size.simp.obj$tbl.width, y.rem-.375*char.dim.obj$linespace.col), frmt=style$frmt.col)

  ### Prints Table and Row Attributes ###
  for (y.dx in 1:tbl.obj$nrw)
    { #
      y.loc <- y.rem - char.dim.obj$linespace.bdy*y.dx

      ### Line Separator ###
      if (y.dx != tbl.obj$nrw)
        {  dborder(c(init[1], y.loc-.5*char.dim.obj$linespace.bdy),
                   c(init[1]+size.simp.obj$tbl.width, y.loc-.5*char.dim.obj$linespace.bdy), frmt=style$frmt.bdy)}
          if (!is.null(tbl.obj$group[1]))  # If group is defined
            {
              if (!is.na(tbl.obj$group[y.dx]))  # If element is NOT NA
              { ### GROUP ###
                grid.text(tbl.obj$group[y.dx],
                          just = "left",
                          gp = gpar(fontfamily = style$frmt.grp$fontfamily, fontface = style$frmt.grp$fontface, fontsize = style$frmt.grp$fontsize, cex = style$cex, col = style$frmt.grp$col),
                          x = unit(init[1], "inches"), y = unit(y.loc, "inches"))
              }
              if (tbl.obj$lastofgroup[y.dx] & (y.dx != tbl.obj$nrw))  # If last row of group, and not row of table (so doesn't conflict with table border)
              {
                ### Group Separator ###
                dborder(c(init[1], y.loc-.5*char.dim.obj$linespace.bdy),
                        c(init[1]+size.simp.obj$tbl.width, y.loc-.5*char.dim.obj$linespace.bdy), frmt=style$frmt.grp)
              }

            }
          if (!is.null(tbl.obj$label[1]))  # If label is defined
            {
              if (!is.na(tbl.obj$label[y.dx])) # If element is NOT NA
                { ### LABEL ###
                  grid.text(tbl.obj$label[y.dx],
                            just = "left",
                            gp = gpar(fontfamily = style$frmt.bdy$fontfamily, fontface = style$frmt.bdy$fontface, fontsize = style$frmt.bdy$fontsize, cex = style$cex, col = style$frmt.bdy$col),
                            x = unit(init[1]+char.dim.obj$lbl.indent, "inches"), y = unit(y.loc, "inches"))
                }
            }
      for (x.dx in 1:tbl.obj$ncl)
        { #        Initial Location + Label&Group Length + Column Width
          x.loc <- init[1] + char.dim.obj$cw.row.u + char.dim.obj$bdy.xloc[x.dx]
          ## Prints all elements in table, Suppressing NAs ##
          if (!is.na(tbl.obj$bdy[y.dx , x.dx]))
            {
              grid.text(tbl.obj$bdy[y.dx , x.dx],
                        just = style$justify,
                        gp = gpar(fontfamily = style$frmt.bdy$fontfamily, fontface = style$frmt.bdy$fontface, fontsize = style$frmt.bdy$fontsize, cex = style$cex, col = style$frmt.bdy$col),
                        x = unit(x.loc, "inches"), y = unit(y.loc, "inches"))
            }
        }
    }
  y.rem <- y.loc

  ### Entire Border ###
  # CORD1: left margin, and top margin and correcting for table title, adjust for extra column space in col header
  # CORD2: left margin+table width, and top margin - table height, adjust for extra column space in col header
  dborder(c(init[1], init[2]-main.nrw*char.dim.obj$linespace.main+.25*char.dim.obj$linespace.col), c(init[1]+size.simp.obj$tbl.width, init[2]-size.simp.obj$tbl.height.m-.25*char.dim.obj$linespace.col), frmt=style$frmt.tbl)

  ### Print Table Footnote ###
  if (!is.na(tbl.obj$footnote.nrw))
    {
      for (footnote.dx in 1:tbl.obj$footnote.nrw)
        {
          y.loc <- y.rem - char.dim.obj$linespace.ftn*.25 - char.dim.obj$linespace.ftn*footnote.dx
          grid.text(tbl.obj$footnote[footnote.dx],
                    just = "left",
                    gp = gpar(fontfamily = style$frmt.ftn$fontfamily, fontface = style$frmt.ftn$fontface,  fontsize = style$frmt.ftn$fontsize, cex = style$cex, col = style$frmt.ftn$col),
                    x = unit(init[1], "inches"), y = unit(y.loc, "inches"))

        }
      y.rem <- y.loc # update overall y location.
    }

   # Update Global y.loc variable
   y.rem <- y.loc
   return(
     list(cord1=c(init[1], init[2]),
          cord2=c(init[1] + char.dim.obj$cw.row.u + char.dim.obj$bdy.xloc[length(char.dim.obj$bdy.xloc)], y.rem-.5*char.dim.obj$linespace.bdy)
          ))
}


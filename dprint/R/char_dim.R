#' Character Width
#'
#' Returns the max width in units, given gpar settings
#' 
#' @param vctr vector
#' @param frmt format object
#' @param cx shrinkage parameter
#' @export
char.width1 <-
function(vctr, frmt, cx=1)
{

   if (is.factor(vctr)) {vctr <- as.character(vctr)}

   n.char   <- nchar(vctr)
   cwidth.n <- max(n.char, na.rm=T)
   max.dx   <- which.max(cwidth.n)

   cwidth.u <- max(unlist(lapply(vctr,FUN=function(x) {convertUnit(grobWidth(textGrob(x, gp=gpar(fontfamily = frmt$fontfamily,
                                                                     fontsize = frmt$fontsize,
                                                                     fontface = frmt$fontface,
                                                                     cex = cx
                                                                      ))), "inches", valueOnly =TRUE)})), na.rm=TRUE)

   return(list(cwidth.n=cwidth.n, # Width in number of characters
               cwidth.u=cwidth.u  # Width in units
               ))
}

#' Character Width
#'
#' Returns the max width of all the elements in a vector in units, handles data types.
#' 
#' @param obj Table Object
#' @param frmt format object
#' @param cx shinkage parameter
#' @export
char.width <-
function(obj, frmt, cx=1)
{
   cwidth.n=NULL; cwidth.u=NULL;
   if (is.data.frame(obj))  # If data.frame return a vector of widths
     {
       for (j in 1:ncol(obj))
          { cw <- char.width1(obj[,j], frmt=frmt, cx=cx)
            cwidth.n <- c(cwidth.n, cw$cwidth.n); cwidth.u <- c(cwidth.u, cw$cwidth.u);
          }
     }
   else # Otherwise simple call
     {
       cw <- char.width1(obj, frmt=frmt, cx=cx)
       cwidth.n <- cw$cwidth.n; cwidth.u <- cw$cwidth.u;
     }

   return(list(cwidth.n=cwidth.n, # Width in number of characters
               cwidth.u=cwidth.u  # Width in units
               ))
}

#' Character Height
#'
#' Returns height in units(inches) based on a gpar setting
#' 
#' @param charact Table Object
#' @param frmt format object
#' @param cx shinkage parameter
#' @export
char.height <-
function(charact = "A", frmt, cx=1)
{

  cheight.u <- convertUnit(grobHeight(textGrob(charact, gp=gpar(fontfamily = frmt$fontfamily,
                                                                  fontsize = frmt$fontsize,
                                                                  fontface = frmt$fontface,
                                                                  cex = cx
                                                                  ))), "inches", valueOnly =TRUE)
   return(cheight.u)
}


#' Character Dimensions of Table
#'
#' Given the table object and style object, will return the maximum width of columns, the height of characters in the units specified by user
#' 
#' @param obj Table Object
#' @param style style 
#' @param cx charcter expansion
#' @export
char.dim <-
function(obj,     # Table Object
                     style,
                     cx=1
                     )
{
  cw.grp.u <- 0; cw.grp.n <- 0;
  cw.lbl.u <- 0; cw.lbl.n <- 0;
  cw.row.u <- NULL; cw.row.n <- NULL;
  cw.bdy.u <- NULL; cw.bdy.n <- NULL;
  ch.bdy   <- NULL; ch.grp   <- NULL;  ch.lbl <- NULL; ch.main<- NULL; ch.ftn<-NULL;

  ### Character Width for each section of Table ###
  for (l.i in 1:length(obj))
    {
      if (!is.null(obj[[l.i]]$group))  # Group
        {
         cw.grp   <- char.width(obj[[l.i]]$group, frmt=style$frmt.grp, cx=cx)
         cw.grp.u <- c(cw.grp.u, cw.grp$cwidth.u)
         cw.grp.n <- c(cw.grp.n, cw.grp$cwidth.n)
        }
      if (!is.null(obj[[l.i]]$label))  # Label
        {
         cw.lbl   <- char.width(obj[[l.i]]$label, frmt=style$frmt.lbl, cx=cx)
         cw.lbl.u <- c(cw.lbl.u, cw.lbl$cwidth.u)
         cw.lbl.n <- c(cw.lbl.n, cw.lbl$cwidth.n)
        }
      # Body
      cw.bdy   <- char.width(obj[[l.i]]$bdy, frmt=style$frmt.bdy, cx=cx)
      cw.bdy.u <- rbind(cw.bdy.u, cw.bdy$cwidth.u)
      cw.bdy.n <- rbind(cw.bdy.n, cw.bdy$cwidth.n)
    }
  # Define the width of one character so constants may be added for white space buffering between columns
  cw.bdy1 <- char.width("A", frmt=style$frmt.bdy, cx=cx)
  cw.grp1 <- char.width("A", frmt=style$frmt.grp, cx=cx)
  cw.lbl1 <- char.width("A", frmt=style$frmt.lbl, cx=cx)


  # Recycle Handling for defining buffer as a vector
  if (length(style$frmt.bdy$buf) < ncol(obj[[1]]$bdy)) {style$frmt.bdy$buf <- rep(style$frmt.bdy$buf, ncol(obj[[1]]$bdy))}

  # Column Names (First Logical Row)
  # Take the Column Name and span.beg, which indicates which column it is in, transpose and feed back to char width
  cw.col   <- char.width(as.data.frame(t(obj[[1]]$colhead[obj[[1]]$colhead$col.logical.row==1, c("cname", "span.beg")])), frmt=style$frmt.col, cx=cx)
  # For multiple lines
  cw.col$cwidth.u <- tapply(cw.col$cwidth.u, obj[[1]]$colhead[obj[[1]]$colhead$col.logical.row==1, "span.beg"], max)
  cw.col$cwidth.n <- tapply(cw.col$cwidth.n, obj[[1]]$colhead[obj[[1]]$colhead$col.logical.row==1, "span.beg"], max)
  # Take max accross all simple tables, and column names, so that all printed tables will have the same column width
  cw.bdy.u <- apply(rbind(cw.col$cwidth.u, cw.bdy.u), 2, max) + style$frmt.bdy$buf*cw.bdy1$cwidth.u # Correction to account for user specified buffer, it is a constant
  cw.bdy.n <- apply(rbind(cw.col$cwidth.n, cw.bdy.n), 2, max) + style$frmt.bdy$buf*cw.bdy1$cwidth.n

  a <- cw.bdy.u
  colhead.xloc <- cumsum(c(.5*a, 0) + c(0, .5*a))

  # Adjust starting location for columns based on justify
  if (style$justify == "right")
    {
      a <- c(cw.bdy.u, .5*style$frmt.bdy$buf[1]*cw.bdy1$cwidth.u)
      a[1] <- a[1]-.5*style$frmt.bdy$buf[1]*cw.bdy1$cwidth.u
    }
  if (style$justify == "center")
    {
      a <- cw.bdy.u
      a <- c(.5*a, 0) + c(0, .5*a)
    }
  if (style$justify == "left")
    {
     a <- c(.5*style$frmt.bdy$buf[1]*cw.bdy1$cwidth.u,cw.bdy.u)
     a[length(a)] <- a[length(a)]-.5*style$frmt.bdy$buf[1]*cw.bdy1$cwidth.u
    }
#    bdy.xloc <- cumsum(bdy.xloc)
    bdy.xloc <- cumsum(a)
  if (!is.null(obj[[l.i]]$group))
    {
      cw.grp.u=max(cw.grp.u) + style$frmt.grp$buf*cw.grp1$cwidth.u;
      cw.grp.n=max(cw.grp.n) + style$frmt.grp$buf*cw.grp1$cwidth.n;
    };
  if (!is.null(obj[[l.i]]$label))
    { #        Max Characters +  Buffer                           +  Indention Under Group
      cw.lbl.u=max(cw.lbl.u) + style$frmt.lbl$buf*cw.lbl1$cwidth.u + style$indent*cw.lbl1$cwidth.u;
      cw.lbl.n=max(cw.lbl.n) + style$frmt.lbl$buf*cw.lbl1$cwidth.n + style$indent*cw.lbl1$cwidth.n;
    }
  # Take the max of group and label
   cw.row.u=max(cw.grp.u, cw.lbl.u); cw.row.n=max(cw.grp.n, cw.lbl.n);


  ### Character Height for each section of Table ###
  ch.bdy  <- char.height("A", frmt=style$frmt.bdy, cx=cx)
  ch.grp  <- char.height("A", frmt=style$frmt.grp, cx=cx)
  ch.lbl  <- char.height("A", frmt=style$frmt.lbl, cx=cx)
  ch.col  <- char.height("A", frmt=style$frmt.col, cx=cx)
  ch.main <- char.height("A", frmt=style$frmt.main, cx=cx)
  ch.ftn  <- char.height("A", frmt=style$frmt.ftn, cx=cx)
  ch.row  <- max(ch.bdy, ch.grp, ch.lbl); # Probably what is going to be used, the max character height
  ### Linespace in Relative Units ###
  linespace.bdy  <- ch.row*style$frmt.bdy$linespace   # Number of characters for lines space specified by user, multiplied by the max unit for character height
  linespace.col  <- ch.col*style$frmt.col$linespace
  linespace.main <- ch.main*style$frmt.main$linespace
  linespace.ftn  <- ch.ftn*style$frmt.ftn$linespace

  lbl.indent <- style$indent*cw.lbl1$cwidth.u;

  return(list(cw.grp.u=cw.grp.u, cw.grp.n=cw.grp.n, # Max Column width for group, label, max(group, label) as row, and body
              cw.lbl.u=cw.lbl.u, cw.lbl.n=cw.lbl.n,
              cw.row.u=cw.row.u, cw.row.n=cw.row.n,
              cw.bdy.u=cw.bdy.u, cw.bdy.n=cw.bdy.n,
              ch.bdy = ch.bdy, ch.grp = ch.grp, ch.lbl = ch.lbl, ch.main = ch.main, ch.row=ch.row, ch.col=ch.col, ch.ftn=ch.ftn,# Column Height for each section of table
              bdy.xloc = bdy.xloc, # Location for Columns
              colhead.xloc=colhead.xloc, # Location for column hiearchy
              lbl.indent=lbl.indent,
              linespace.bdy=linespace.bdy, # Lines space between rows in units
              linespace.col=linespace.col,
              linespace.main=linespace.main,
              linespace.ftn=linespace.ftn
              ))
}


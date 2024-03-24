#' Column Names Row
#'
#' Adjust index (reference number of rows above table for colnames) to account for line breaks
#' 
#' @param colnames.obj column names object
#' @export
colnames.row <-
function(colnames.obj)
  {
     max.row <- tapply(colnames.obj$col.row, list(colnames.obj$col.logical.row), function(x) max(x, na.rm=T))
     d.max.row <- data.frame(max.row)
     d.max.row$col.logical.row <- rownames(d.max.row)
     d.max.row$lag.max.row.adj <- c(0, d.max.row$max.row[-nrow(d.max.row)]-1)
     colnames.obj <- merge(colnames.obj, d.max.row, by ="col.logical.row")
     colnames.obj$row <- colnames.obj$col.logical.row + colnames.obj$col.row -1 + colnames.obj$lag.max.row.adj
     colnames.obj$row <- max(colnames.obj$row) - (colnames.obj$row-1)
     colnames.obj
  }


#' Column Names Line Break
#'
#' Inserts a line break in column names when the escape character [backslash] n is found.
#' 
#' @param colnames.obj column names object
#' @export
colnames.linebreak <-
function(colnames.obj  # Column Names Object creatd by COLNAMES.STRUCT
                              )
{
  grep.linebreak <- grep("\n",colnames.obj$cname)
  if (length(grep.linebreak > 0))
    {  # This only accounts for one record at a time - which many contain multiple line breaks
       strsplit.linebreak  <- strsplit(colnames.obj$cname[grep.linebreak[1]], "\n")  # Break on line escape character
       n.linebreak         <- length(strsplit.linebreak[[1]]) # store how many line breaks
       d.temp         <- colnames.obj[rep(grep.linebreak[1], n.linebreak), ] # Expand for each linebreak in this particular name
       d.temp$cname   <- strsplit.linebreak[[1]] # insert section of string that was delimitted by the string break character
       d.temp$col.row <- n.linebreak:1
       if (grep.linebreak[1] == 1)
        {  # Insert new expanded rows into orignal object (if row occurs at begining of data frame)
          d.after   <- colnames.obj[(grep.linebreak[1]+1):nrow(colnames.obj) , ] # After
          colnames.obj <- rbind( d.temp, d.after)
        }
       else if (grep.linebreak[1] == nrow(colnames.obj))
        {  # Insert new expanded rows into orignal object (if row occurs at end of data frame)
          d.before  <- colnames.obj[1:(grep.linebreak[1]-1) , ]
          colnames.obj <- rbind(d.before,  d.temp)
        }
       else
        {  # Insert new expanded rows into orignal object (if row occurs in middle of data frame)
          d.before  <- colnames.obj[1:(grep.linebreak[1]-1) , ]
          d.after   <- colnames.obj[(grep.linebreak[1]+1):nrow(colnames.obj) , ] # After
          colnames.obj <- rbind(d.before,  d.temp, d.after)
        }
        grep.linebreak <- grep("\n",colnames.obj$cname)
        # Recursive Calls
        if (length(grep.linebreak) > 0) {colnames.obj <- colnames.linebreak(colnames.obj)}
    }
  colnames.obj
}


#' Column Names Structure
#'
#' Creates a structure for printing column names and their hierachies
#' 
#' @param col.names vector of strings (result of colnames(data.frame))
#' @param linebreak boolean
#' @export
colnames.struct <-
function(col.names, linebreak=TRUE)
{
  # look for escape character indicating that a grouping scheme exists for column names
  col.grp.dx      <- grep(":", col.names)
  col.grp.split   <- strsplit(col.names, ":")
  orig.names      <- unlist(lapply(col.grp.split, FUN=function(x) {x[length(x)]}))
  lst.names       <- lapply(col.grp.split, FUN=function(x) {x[-length(x)]}) # group names (if any) after removing origninal column names
  lnames          <- length(orig.names)
  # Logical row keeps column hiercahies together, col.row indicates whether line breaks have been performed
  column.heading  <- data.frame(cname = orig.names, col.logical.row = rep(1, lnames), col.row=1, span.beg=1:lnames, span.end=1:lnames, stringsAsFactors =F)
  r.i             <- nrow(column.heading)
  cname.hierc.df  <- list.to.df(lst.names)

  if(!is.null(cname.hierc.df))
    {
      for (hier.i in ncol(cname.hierc.df):1)
        {
          d.i <- consect.struct(cname.hierc.df[, hier.i])
          n.d <- nrow(d.i$consec.begend)
          column.heading[r.i+ (1:n.d), "col.logical.row"] <- ncol(cname.hierc.df)-hier.i+2
          column.heading[r.i+ (1:n.d), "col.row"] <-1
          column.heading[r.i+(1:n.d), c("cname", "span.beg", "span.end")] <- d.i$consec.begend
          r.i <- nrow(column.heading)
        }
    }
  column.heading$cname  <- kill.multiregx(column.heading$cname, "`") #  Remove backhashes
  if (linebreak) # Can be called in two different places with different assumptions whether the line break has been added yet
    {
      column.heading        <- colnames.linebreak(column.heading)        #  Account for line breaks in column names
      column.heading        <- colnames.row(column.heading)              #  Adjust index (reference # of rows above table for colnames) that accounts for line breaks
    }
  column.heading
}


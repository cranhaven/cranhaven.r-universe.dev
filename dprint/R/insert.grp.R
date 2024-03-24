#' Insert Values
#'
#' Inserts a value into record in tbl.struct objects to control for when empty rows should be inserted into the data structure.  NA's will be suppressed during presentation of the table.
#'
#' @param obj vector or data frame
#' @param dx Index where NA is to be inserted
#' @param lblescp Label Escape, boolean
#' @param group indicates that this object is related to the grouping vector and the insetions must go after
#' @param val Values to be inserted
#' @param dx.up Bolean, If true adjust an index
#' @export
insert.grp1 <-
function(obj,      # vector or data frame
          dx,         # Index where NA is to be inserted
          lblescp,    # Label Escape, boolean
          group=FALSE,  # indicates that this object is related to the grouping vector and the insetions must go after
          val=NA,      
          dx.up =FALSE # If true adjust an index
)
{
  # This section handles data frames
  if (is.data.frame(obj))
    {
      for (i in 1:length(dx))
        {
          if (!lblescp[i])
            { if(i==1)  {obj <- rbind(rep(val, ncol(obj)), obj)}
              else
                { # Exception to handle object changing class when subsetting
                  data1 <- obj[1:(dx[i]-1), ]
                  data2 <- obj[(dx[i]):nrow(obj), ]
                  if (!is.data.frame(data1)) {data1 <- as.data.frame(data1); colnames(data1) <- "a1";}
                  if (!is.data.frame(data2)) {data2 <- as.data.frame(data2); colnames(data2) <- "a1";}
                   obj <- rbind(data1, rep(val, ncol(obj)), data2)
                }
              dx <- dx + 1
            }
        }
    }
  else if(dx.up)
    { obj2 <- obj
      for (dx.i in 1:length(dx))
        {
          if (!lblescp[dx.i])
            {          obj2[obj>=dx[dx.i]] <- obj2[obj>=dx[dx.i]] +1}
        }
      obj<-obj2
    }
  # This section handles vectors
  else
    { # This section handles vectors with the exception of groups
      if (!group)
        {
          for (i in 1:length(dx))
            { 
              if (!lblescp[i])
               {
                  if(i==1) {obj <- c(val, obj)}
                  else {obj <- c(obj[1:(dx[i]-1)], val, obj[(dx[i]):length(obj)])}
                  dx <- dx + 1
                }
            }
        }
      else # This section handles the group vector, index is slightly altered to put NA after the group name
        {
          for (i in 1:length(dx))
            {
              if (!lblescp[i])
               {
                  if(i==length(dx)) {obj <- c(obj, val)}
                  else {obj <- c(obj[1:(dx[i+1]-1)], val, obj[(dx[i+1]):length(obj)])}
                  dx <- dx + 1
               }
            }
        }
    }
  return(obj)
}

#' Insert Values Table Structure
#'
#' Dispatcher for insert.grp1. Inserts NA positions for tbl, label, and group structure from tbl.obj.
#'
#' @param tbl.obj Table Object
#' @export
insert.grp <-
function(tbl.obj)
{
  if (!is.null(tbl.obj$group))
   {
      tbl.obj$bdy         <- insert.grp1(tbl.obj$bdy, tbl.obj$newgrp.dx, tbl.obj$lblescp)
      if (!is.null(tbl.obj$label))
        {tbl.obj$label  <- insert.grp1(tbl.obj$label, tbl.obj$newgrp.dx, tbl.obj$lblescp)}
      tbl.obj$group       <- insert.grp1(tbl.obj$group, tbl.obj$newgrp.dx, tbl.obj$lblescp, group=T)
      tbl.obj$newgroup    <- insert.grp1(tbl.obj$newgrp, tbl.obj$newgrp.dx, tbl.obj$lblescp, group=T, val=FALSE)
      tbl.obj$lastofgroup <- insert.grp1(tbl.obj$lastofgroup, tbl.obj$newgrp.dx, tbl.obj$lblescp, val=FALSE)
      tbl.obj$nrw         <- tbl.obj$nrw + length(tbl.obj$newgrp.dx) - sum(tbl.obj$lblescp)
      if(!is.null(tbl.obj$row.hl$dx)) # Row Highlight
        {tbl.obj$row.hl$dx <- insert.grp1(tbl.obj$row.hl$dx, tbl.obj$newgrp.dx, tbl.obj$lblescp, FALSE,val=1, dx.up=TRUE)}
      # This must go last
      {tbl.obj$newgrp.dx <- which(tbl.obj$newgroup)}
   }
 return(tbl.obj)
}


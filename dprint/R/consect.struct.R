#' Consecutive Structure
#'
#' Given a vector returns several objects describing how(if) consecutive elements appear.  
#' This is a helper function for tbl.struct and is important in defining the column hierarchy.
#' 
#' @param vct vector
#' @export
consect.struct <-
function(vct  # vector
                          )
{
  temp.vct <- vct
  lvct <- length(vct) #length of vector
  # Handle NAs
  temp.vct[is.na(vct)] <- paste("NA.", 10000+(1:length(which(is.na(vct)))), sep="") # NA's should be treated as distinct
  next.vct  <- c(temp.vct[2:lvct], NA) # the next position
  # Indicate when the next consective value begins
  new.vct       <- temp.vct != next.vct
  new.vct       <- c(TRUE, new.vct[-lvct])
  new.vct.dx    <- which(new.vct)
  new.vct.gdx   <- cumsum(new.vct)
  consec.info   <- data.frame(vct, temp.vct, next.vct, new.vct, new.vct.gdx, stringsAsFactors =F)
  if (lvct>1) # Exception handler for when just vector is of length 1
  {
    min.dx <- tapply(1:lvct, list(new.vct.gdx), function(x) {min(x)})
    max.dx <- tapply(1:lvct, list(new.vct.gdx), function(x) {max(x)})
  }
  else {min.dx=1; max.dx=1}
  consec.begend <- data.frame(uniq.val=vct[new.vct], mn=min.dx, mx=max.dx, stringsAsFactors =F)
  return(list(consec.info=consec.info, consec.begend=consec.begend[!is.na(consec.begend$uniq.val), ]))
}


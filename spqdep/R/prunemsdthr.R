# Function to prune observations with distances over thresholds
# Used in m.surround.R, m_surr_cbl.R and m_surr_cdt.R
# Check for threshold in absolute distances dtmaxabs
# or percentage distance dtmaxpc
prunemsdthr <- function(dtmaxabs = 0, dtmaxpc = 0, mdtfull, ms,
                        mdtms, mscbl = NULL)
{
  if (!(dtmaxabs > 0) && !(dtmaxpc > 0)) dthr <- 0
  if (dtmaxpc > 0) {
    # Set percentage threshold to absolute threshold
    if (dtmaxpc <= 1)
      dthr <- as.numeric(dtmaxpc*max(mdtfull)) else
        stop("The value of dtmaxpc must be in [0,1]")
    }
    # Check if external absolute threshold is lower...
    if (dtmaxabs > 0) {
      if (dtmaxpc > 0) {
        if (dtmaxabs < dthr) dthr <- dtmaxabs
      } else dthr <- dtmaxabs
    }
  if (dthr > 0) {
    excl <- mdtms > dthr
    indexcl <- which(excl == TRUE, arr.ind = TRUE)
    rowexcl <- unique(indexcl[, "row"])
    cat("\n Threshold distance: ", dthr)
    if (length(rowexcl) > 0) {
      cat("\n Number of m-surroundings excluded for exceeding
        the threshold distance: ",length(rowexcl),"\n")
      cat("\n Index of spatial observations excluded: ", rowexcl,"\n")
      ms <- ms[-rowexcl,]
      mdtms <- mdtms[-rowexcl,]
      if (!is.null(mscbl)) mscbl <- mscbl[-rowexcl,]
    } else cat("\n None m-surrounding excluded for exceeding
        the threshold distance","\n")
  }
  lms <- list(ms = matrix(as.integer(ms), nrow=nrow(ms), ncol = ncol(ms)),
              R = nrow(ms), mdtms = mdtms)
  if (exists("rowexcl")) lms$rowexcl <- rowexcl
  return(lms)
}


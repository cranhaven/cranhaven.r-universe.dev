#' Restrict the DTW warping window for snow profiles alignment
#'
#' Given a matrix, this function sets all elements of the matrix that are outside the so-called warping
#' window to `NA`. The warping window is a slanted band of constant width around the main diagonal
#' (i.e., *Sakoe-Chiba*-band), and it's size can be controlled with function arguments.
#'
#'
#' @param iw matrix of integers indicating their row number (cf., `?row`)
#' @param jw matrix of integers indicating their column number (cf., `?col`)
#' @param iheight matrix of query height filled into the columns of the matrix
#' @param jheight matrix of ref height filled into the rows of the matrix
#' @param iddate same as iheight, but containing deposition date information (i.e., POSIXct data converted to numeric through matrix call!)
#' @param jddate same as jheight, but containing deposition date information (i.e., POSIXct data converted to numeric through matrix call!)
#' @param profile.size number of layers in the longer one of the two profiles (scalar)
#' @param profile.height snow height of the deeper one of the two profiles (scalar)
#' @param window.size percentage of profile.size or profile.height defining the size of the warping window
#' (i.e., the most restrictive of the two will be applied)
#' @param window.size.abs Instead of a `window.size` percentage, an absolute value (in *cm*!) can be provided
#' @param ddate.window.size number of days that exclude layers from the warping window if their deposition dates
#' differ by more than these days
#' @param ... unused---but important to be able to provide other warping functions to [distanceSPlayers]
#'
#' @seealso [dtw::dtwWindowingFunctions]
#'
#' @export
'warpWindowSP' <- function(iw, jw,
                           iheight, jheight,
                           iddate, jddate,
                           profile.size, profile.height,
                           window.size = 0.3, window.size.abs = NA,
                           ddate.window.size = Inf, ...) {

  ## initialize boolean matrix to TRUE
  bmat <- matrix(TRUE, max(iw), max(jw))

  if (is.na(window.size.abs)){
    ## set elements outside of warp window to FALSE..
    ## based on index (i.e. layer #):
    bmat[abs(iw - jw) > profile.size * window.size] <- FALSE

    ## based on layer height:
    bmat[abs(iheight - jheight) > profile.height * window.size] <- FALSE

  } else {
    ## based on an absolute window wrt layer height:
    bmat[abs(iheight - jheight) > window.size.abs] <- FALSE
  }

  ## additionally constrain window based on ddate:
  if (!all(is.na(iddate))) {
    bmat[abs(iddate - jddate) > ddate.window.size *24*3600] <- FALSE  # numeric POSIXct values represent units of seconds; --> adjust ddate.window units from days to seconds!
  }

  return(bmat)
}

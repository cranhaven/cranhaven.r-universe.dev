#' Selecting variables for the SDA model
#'
#' @description Function for selecting variables for
#' the second deminsion of spatial association (SDA) model
#'
#' @usage selectvarsda(y, xlist)
#'
#' @param y A vector of a response variable
#' @param xlist A list of the SDA explanatory variables
#'
#' @return A list of the selected the second dimension variables
#'
#' @examples
#' data("obs")
#' data("sample_vars_sda")
#' obs$Cr_ppm <- log(obs$Cr_ppm)
#' krm <- rmvoutlier(obs$Cr_ppm)
#' y <- obs$Cr_ppm[-krm]
#' x <- list(sample_vars_sda[[1]][-krm, 1:11])
#' system.time({ # ~0.01s
#'   sx <- selectvarsda(y, xlist = x)
#' })
#' @export
#'
selectvarsda <- function(y, xlist){
  nx <- length(xlist)
  selected.vars <- list()
  if (nx > 1){
    for (i in 1:nx){
      dx <- xlist[[i]]
      names(dx) <- paste(paste("v", i, sep = ""), names(dx), sep = "")
      selected.dx <- selectvarlm(y = y, x = dx)

      selected.vars[[i]] <- selected.dx
    }
    vars <- do.call(cbind, selected.vars)

    selected.dx.all <- selectvarlm(y = y, x = vars)
  } else {
    dx <- xlist[[1]]
    selected.dx.all <- selectvarlm(y = y, x = dx)
  }
  return(selected.dx.all)
}




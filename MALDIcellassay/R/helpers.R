#' Current time
#'
#' @return current time
#' @noRd
timeNow <- function() {
  format(Sys.time(), "%H:%M")
}

#' Convert concentration to log10 and replace zero's
#'
#' @param conc numeric, concentrations.
#'
#' @return
#' numeric, log10 transformed concentrations
#' @export
#' @examples
#' transformConc2Log(c(0.1, 0.01,0.001))
transformConc2Log <- function(conc) {
  concLog <- log10(conc)
  if (any(concLog == -Inf)) {
    concLog[which(concLog == -Inf)] <- (min(concLog[which(!concLog == -Inf)]) - 1)
  }
  return(concLog)
}

#' go up x folders
#'
#' @param path Character, Path
#' @param x    Numeric
#'
#' @return     new path
#' @noRd
goUpXFolders <- function(path, x) {
  for (i in 1:x) {
    resPath <- dirname(path)
    path <- resPath
  }
  return(resPath)
}

#' Convert spectrum to data.frame
#'
#' @param specs MALDIquant::MassSpectrum or list there of
#'
#' @return      data.frame of spectra
#' @noRd
spec2df <- function(specs) {
  df_l <- lapply(1:length(specs), function(i) {
    mz <- mass(specs[[i]])
    int <- intensity(specs[[i]])
    name <- names(specs)[i]
    return(tibble(
      name = name,
      mz = mz,
      int = int,
      plotIdx = i
    ))
  })
  return(bind_rows(df_l))
}

#' Check if object if of class MALDIassay
#'
#' @param object object to text
#'
#' @return
#' logical, TRUE if object is of class MALDIassay
#' @export
#' @examples 
#' x <- 1
#' # FALSE
#' isMALDIassay(x)
#' # TRUE
#' isMALDIassay(Blank2022res)
isMALDIassay <- function(object) {
  if (!is(object, "MALDIassay")) {
    return(FALSE)
  }
  return(TRUE)
}

#' Stop and throw an error if object is not of class MALDIassay
#'
#' @param object object
#' @noRd
stopIfNotIsMALDIassay <- function(object) {
  if (!isMALDIassay(object)) {
    stop("object needs to be of class MALDIassay.")
  }
}


#' Calculate the fold-change
#'
#' @param model      nplr model object
#'
#' @return
#' Numeric, fold-change
#'
#' @importFrom tibble is_tibble
#' @noRd
calculateFC <- function(model) {
  par <- getPar(model)$params

  if(par[["top"]] <= 0 | par[["bottom"]] <= 0) {
    return(NA)
  }

  if(par[["scal"]] < 0) {
    FC <- par[["bottom"]]/par[["top"]]
    return(FC)
  }

  FC <- par[["top"]]/par[["bottom"]]

  return(FC)
}

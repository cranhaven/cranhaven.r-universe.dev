#to_cambial_age-----------------------------------------------------------------
#' @title to_cambial_age
#' @description This function aligns tree ring series to match their cambial
#' ages, taking pith offset into account if provided.
#' @param rwl a data frame/rwl object.
#' @param po optional, a data frame containing series names in the first and po
#'   data as nr. of years in the second column.
#'
#' @return A data.frame with aligned series
#'
#' @examples
#' library("dplR")
#' data("gp.rwl")
#' data("gp.po")
#' gp.po$series <- as.character(gp.po$series)
#' iadf:::to_cambial_age(gp.rwl, gp.po)
to_cambial_age <- function(rwl, po = NULL) {
  #check arguments

  if (!is.data.frame(rwl)) {
    stop("rwl must be of class data.frame")
  }

  if (!(is.data.frame(po) || is.null(po))) {
    stop("po must be of class data.frame or NULL")
  }

  if (is.null(po)) {
    po <- data.frame(series = names(rwl), po = 1, stringsAsFactors = FALSE)
  }


  if (ncol(po) > 2 || !is.numeric(po[, 2])) {
    stop("please provide a po object with two columns and pith offset as numeric
         values (cambial age of innermost ring) in the second column")
  }

  if (!all(po[, 2] > 0)) {
    stop("please provide po as cambial age of the
         first ring - this has to be > 0")
  }

  if (!setequal(po[, 1], names(rwl))) {
    stop("series names in po are not the same as provided in rwl")
  }

  #execute function
  po.ordered <- po[po[, 1] %in% names(rwl), ]

  lengths <- (series_length(rwl) + po.ordered[, 2]) - 1
  rows <- max(lengths)
  out <- data.frame(matrix(NA, ncol = length(rwl), nrow = rows))
  for (i in seq_along(rwl)){
    start <- (po.ordered[i, 2])
    out[start:lengths[i], i] <- na.omit(rwl[, i])
  }
  names(out) <- names(rwl)
  return(out)
}


#series_length------------------------------------------------------------------
#' @title series length
#' @description returns the series length of the series within a data.frame/rwl
#'   object.
#' @param x a data.frame/rwl object
#'
#' @return a numeric vector
series_length <- function(x) {
  sapply(x, FUN = function(y) length(na.omit(y)))
}

#sort_by_index------------------------------------------------------------------
#' @title sort_by_index
#' @description internal function such as sortByIndex as in package dplR, shifts
#'   series to start with index 1, maintaining the same vector length by
#'   adding NA values to the end.
#' @param x a numeric vector, representing an individual rwl series,
#'   potentially containing NA values.
#' @return a numeric vector with the same length as x.
#' @examples
#' x <- c(NA,NA,NA,1,2,3,4,5, NA, NA)
#' iadf:::sort_by_index(x)
#' #[1]  1  2  3  4  5 NA NA NA NA NA
sort_by_index <- function(x) {
  lowerBound <- which.min(is.na(x))
  c(x[lowerBound:length(x)], rep(NA, lowerBound - 1))
}

#detrend_given_rc---------------------------------------------------------------
detrend_given_rc <- function(rwl, rc, po, method = c("quotient", "difference")) {

  if (!method %in% c("quotient", "difference")) {
    stop("please choose either 'quotient' or 'difference' as method")
  }

  if (!is.data.frame(rwl)) {
    stop("rwl must be of class data.frame")
  }

  if (!(is.data.frame(po))) {
    stop("po must be of class data.frame")
  }

  if (!setequal(po[, 1], names(rwl))) {
    stop("series names in po are not the same as provided in rwl")
  }

  if (!is.numeric(rc)){
    stop("rc must be a numeric vector")
  }

  if (length(na.omit(rc)) < max(series_length(rwl))) {
    greater <- series_length(rwl) > length(na.omit(rc))
    warning(paste0("rc is shorter than series: ", paste0(names(rwl)[greater],
                                                         collapse = ", ")))
  }

  n.col <- ncol(rwl)
  col.names <- names(rwl)
  seq.cols <- seq_len(n.col)
  rwl2 <- rwl
  rownames(rwl2) <- rownames(rwl2)
  rwl.ord <- apply(rwl2, 2, sort_by_index)
  rwca <- matrix(NA, ncol = n.col, nrow = sum(nrow(rwl.ord) + max(po[, 2])))
  nrow.m1 <- nrow(rwl.ord) - 1
  for (i in seq.cols) {
    yrs2pith <- po[po[, 1] %in% col.names[i], 2]
    rwca[yrs2pith:(yrs2pith + nrow.m1), i] <- rwl.ord[, i]
  }

  if (method == "quotient"){
    rwica <- rwca / rc[seq_len(nrow(rwca))]
  } else if (method == "difference") {
    rwica <- rwca - rc[seq_len(nrow(rwca))]
  }

  rwi <- rwl2
  yrs <- as.numeric(row.names(rwl2))
  for (i in seq.cols) {
    series.yrs <- as.numeric(range(rownames(rwl2)[!is.na(rwl2[[i]])]))
    first <- series.yrs[1]
    last <- series.yrs[2]
    tmp <- na.omit(rwica[, i])
    if (first + length(tmp) != last + 1) {
      warning(paste("indexing problem when restoring to cal years:
                    first+length(tmp) != last+1", "problem in interation", i,
                    "rc curve probably shorter than series", sep = " "))
    }
    rwi[[i]][yrs %in% first:last] <- tmp
  }
  return(rwi)
}

#' Join two tables based on overlapping genomic intervals: both a
#'
#' This is an extension of \code{\link{interval_join}} specific to genomic intervals.
#' Genomic intervals include both a chromosome ID and an interval: items are only
#' considered matching if the chromosome ID matches and the interval overlaps.
#' Note that there must be three arguments to by, and that they must be in the order
#' c("chromosome", "start", "end").
#'
#' @param x A tbl
#' @param y A tbl
#' @param by Names of columns to join on, in order c("chromosome", "start", "end").
#' A match will be counted only if the chromosomes are equal and the start/end pairs
#' overlap.
#' @param mode One of "inner", "left", "right", "full" "semi", or "anti"
#' @param ... Extra arguments passed on to \code{\link[IRanges]{findOverlaps}}
#'
#' @details All the extra arguments to \code{\link{interval_join}}, which are
#' passed on to \code{\link[IRanges]{findOverlaps}}, work for \code{genome_join}
#' as well. These include \code{maxgap} and \code{minoverlap}.
#'
#' @examples
#'
#' library(dplyr)
#'
#' x1 <- tibble(id1 = 1:4,
#'              chromosome = c("chr1", "chr1", "chr2", "chr2"),
#'              start = c(100, 200, 300, 400),
#'              end = c(150, 250, 350, 450))
#'
#' x2 <- tibble(id2 = 1:4,
#'              chromosome = c("chr1", "chr2", "chr2", "chr1"),
#'              start = c(140, 210, 400, 300),
#'              end = c(160, 240, 415, 320))
#'
#' if (requireNamespace("IRanges", quietly = TRUE)) {
#'   # note that the the third and fourth items don't join (even though
#'   # 300-350 and 300-320 overlap) since the chromosomes are different:
#'   genome_inner_join(x1, x2, by = c("chromosome", "start", "end"))
#'
#'   # other functions:
#'   genome_full_join(x1, x2, by = c("chromosome", "start", "end"))
#'   genome_left_join(x1, x2, by = c("chromosome", "start", "end"))
#'   genome_right_join(x1, x2, by = c("chromosome", "start", "end"))
#'   genome_semi_join(x1, x2, by = c("chromosome", "start", "end"))
#'   genome_anti_join(x1, x2, by = c("chromosome", "start", "end"))
#' }
#'
#' @export
genome_join <- function(x, y, by = NULL, mode = "inner", ...) {
  if (!requireNamespace("IRanges", quietly = TRUE)) {
    stop("genome_join requires the IRanges package: ",
         "https://bioconductor.org/packages/release/bioc/html/IRanges.html")
  }

  by <- common_by(by, x, y)
  if (length(by$x) != 3) {
    stop("genome_join must join on exactly three columns")
  }

  f <- function(x, y) {
    # nest around the chromosome column
    x$..index <- seq_len(nrow(x))
    y$..index <- seq_len(nrow(y))
    nested_x <- tidyr::nest(dplyr::group_by_at(x, .vars = 1))
    nested_y <- tidyr::nest(dplyr::group_by_at(y, .vars = 1))
    by <- c(colnames(nested_y)[1])
    names(by) <- colnames(nested_x)[1]

    joined <- dplyr::inner_join(nested_x, nested_y, by = by)

    # find matching ranges in each
    find_overlaps <- function(xd, yd) {
      r1 <- IRanges::IRanges(xd[[1]], xd[[2]])
      r2 <- IRanges::IRanges(yd[[1]], yd[[2]])
      o <- as.data.frame(IRanges::findOverlaps(r1, r2, ...))
      data.frame(x = xd$..index[o$queryHits], y = yd$..index[o$subjectHits])
    }

   ret <- purrr::map2_df(joined$data.x, joined$data.y, find_overlaps)
   ret
  }

  fuzzy_join(x, y, mode = mode, index_match_fun = f, multi_by = by)
}


#' @rdname genome_join
#' @export
genome_inner_join <- function(x, y, by = NULL, ...) {
  genome_join(x, y,  by, mode = "inner", ...)
}


#' @rdname genome_join
#' @export
genome_left_join <- function(x, y, by = NULL, ...) {
  genome_join(x, y,  by, mode = "left", ...)
}


#' @rdname genome_join
#' @export
genome_right_join <- function(x, y, by = NULL, ...) {
  genome_join(x, y,  by, mode = "right", ...)
}


#' @rdname genome_join
#' @export
genome_full_join <- function(x, y, by = NULL, ...) {
  genome_join(x, y,  by, mode = "full", ...)
}


#' @rdname genome_join
#' @export
genome_semi_join <- function(x, y, by = NULL, ...) {
  genome_join(x, y,  by, mode = "semi", ...)
}


#' @rdname genome_join
#' @export
genome_anti_join <- function(x, y, by = NULL, ...) {
  genome_join(x, y,  by, mode = "anti", ...)
}

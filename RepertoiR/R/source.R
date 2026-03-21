#' Visualized for CR Sources
#'
#' @description Visualization of Two clones for their convergent recombination
#' (CR) sources. Each sequence (NT) is represented as a colored bar (red for A,
#' yellow for G, blue for T and green for C) linked to its translated amino
#' acid sequence by a colored line, red for the first clone and blue for the
#' second.
#'
#' @param clone1 First vector of sequences, string-length is the same for each
#' nucleotide sequence ('A', 'G', 'T', 'C').
#' @param clone2 Second vector of sequences, same string-length as for the
#' first vector.
#' @param ... Any other arguments.
#'
#' @export
#'
#' @return No return value.
#'
#' @examples
#' nt <- c("A", "G", "C", "T")
#' seq_len <- 15
#' seq_n <- c(12, 7)
#'
#' # Create data
#' c1 <- replicate(seq_n[1],
#'                 paste(sample(nt, seq_len, replace = TRUE), collapse = ''))
#' c2 <- replicate(seq_n[2],
#'                 paste(sample(nt, seq_len, replace = TRUE), collapse = ''))
#'
#' cr_source(c1, c2)
#'
cr_source <- function(clone1, clone2,...) {
  UseMethod("cr_source")
}



#' Visualized for CR Sources
#'
#' @description Visualization of Two clones for their convergent recombination
#' (CR) sources. Each sequence (NT) is represented as a colored bar (red for A,
#' yellow for G, blue for T and green for C) linked to its translated amino
#' acid sequence by a colored line, red for the first clone and blue for the
#' second.
#'
#' @param clone1  First vector of sequences, string-length is the same for each
#' nucleotide sequence ('A', 'G', 'T', 'C').
#' @param clone2  Second vector of sequences, same string-length as for the
#' first vector.
#' @param ... Any other arguments.
#'
#' @importFrom circlize circos.initialize circos.track circos.par circos.clear
#' circos.barplot circos.link get.cell.meta.data mm_h circos.rect
#' @importFrom stringr str_replace_all
#' @importFrom grDevices adjustcolor
#'
#' @export
#'
#' @return No return value.
#'
#' @examples
#' nt <- c("A", "G", "C", "T")
#' seq_len <- 15
#' seq_n <- c(12, 7)
#'
#' # Create data
#' c1 <- replicate(seq_n[1],
#'                 paste(sample(nt, seq_len, replace = TRUE), collapse = ''))
#' c2 <- replicate(seq_n[2],
#'                 paste(sample(nt, seq_len, replace = TRUE), collapse = ''))
#'
#' cr_source(c1, c2)
#'
cr_source.default <- function(clone1, clone2,...) {
  ntseq <- c(clone1, clone2)
  n <- length(ntseq)
  len <- nchar(max(ntseq))

  aa_name <- "aa"
  sourse_name <- c("A", "B")

  names(ntseq) <- c(paste0(sourse_name[1], seq(length(clone1))),
                    paste0(sourse_name[2], seq(length(clone2))))
  sectors <- c(names(ntseq), aa_name)
  # Create x-axis range for each sector, 0-1 for each
  axes <- matrix(rep(c(0, 1), each = n), ncol = 2)
  axes <- rbind(axes, c(0, n))
  rownames(axes) <- sectors
  # Set circos parameters
  circos.clear()
  circos.par$cell.padding <- c(0, 0, 0, 0)
  circos.par$track.margin <- c(0.005, 0.005)
  circos.par$start.degree <- 0
  circos.par$gap.degree <- c(rep(2, n-1), 5, 5)
  circos.par$start.degree <- 90
  circos.par$clock.wise <- FALSE
  circos.par$points.overflow.warning <- FALSE
  # Initiate circos graph
  circos.initialize(sectors, xlim = axes, sector.width = c(rep(1, n), n/2))
  circos.track(sectors=names(ntseq), x=n:1, ylim=c(0, len), bg.border = NA,
               track.height = 0.7, panel.fun = function(x, y) {
    # Bars track
    i <- get.cell.meta.data("sector.index")
    nt_col <- as.character(ntseq[i])
    nt_col <- rev(unlist(strsplit(nt_col, split = "")))
    nt_col <- str_replace_all(nt_col, c("A" = "tomato",
                                        "G" = "khaki1",
                                        "T" = "dodgerblue",
                                        "C" = "palegreen"))
    circos.barplot(value = len:1, pos = rep(0.5, len), bar_width = .5,
                   col = nt_col, border = adjustcolor("black", alpha.f=.3))
  })

  circos.track(sectors=sectors, x=n:0, ylim=c(0, 1), bg.border = NA,
               track.height = mm_h(1), panel.fun = function(x, y) {
    # Links track
    i <- get.cell.meta.data("sector.index")
    if (i!=aa_name) {
      to <- ifelse(x <= length(clone1), x + length(clone2), x - length(clone1))
      to <- to * max(axes[aa_name,]) / n
      col1 <- adjustcolor("red", alpha.f=.6)
      col2 <- adjustcolor("blue", alpha.f=.6)
      link_col <- ifelse(x <= length(clone1), col1, col2)
      circos.link(i, c(.15, .85), aa_name, c(to-1, to), col = link_col,
                  border = adjustcolor("black", alpha.f=.3))
    }
    circos.rect(get.cell.meta.data("xlim")[1], 0, get.cell.meta.data("xlim")[2],
                1, col="azure3", border=adjustcolor("black", alpha.f=.5))
  })

  circos.clear()
}

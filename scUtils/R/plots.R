



# internal functions ----------------------------------------------------------



#' Check if number(s) is/are integers. In contrast to is.integer, is_wholenumber
#' does not check the class but accepts all numbers that are integers with reasonable
#' precision.
#' @param x Number to test
#' @param tol tolerance for testing
#' @NoRd
is_wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol




# useful functions --------------------------------------------------------




#' @title Feature Plot
#' @description Highlight gene expression data in a 2D-embedding (UMAP, tSNE, etc.).
#' @param embedding A matrix/data.frame/tibble/... with exactly two columns.
#' If colnames are missing, the axis will be named "Dim1" and "Dim2".
#' Other classes than matrix/data.frame/tibble are possible, as long as
#' \code{data.frame(embedding)}) produces a numeric data.frame.
#' @param expression Numeric vector with expression values of the gene of
#' interest. Order has to correspond to the row order in \code{embedding}.
#' Typically, \code{expression} is normalized gene expression and we recommend
#' \code{k/s/mean(1/s)}, where \code{k} are UMI counts for the gene of interest
#' and \code{s} are totalUMI of the cell (aka library size).
#' @param legend_name Text displayed above the legend. Most commonly the name
#' of the displayed gene.
#' @return A \code{ggplot2} object storing a colored scatter plot.
#' @details This function discourages customization on purpose, because it
#' bundles geoms, themes and settings that I found important for
#' visualizing gene expression in scRNAseq data:
#'
#' \itemize{
#'  \item coord_fixed, to avoid distortion of embeddings
#'  \item geom_point with size=.4, to ameliorate overplotting
#'  \item No background grid, because distances and axis units
#'  in embeddings do not
#'  carry meaning for most dimensionality reduction techniques.
#'  \item Intensity-coded color scales (viridis) displayed with
#'  log2-transformation. Makes visualization independent of colorblindness
#'  and appropriate for gene expression data (which is usually Log Normal
#'  distributed).
#'  \item Color scale breaks are displayed as 'closed interval', i.e.
#'  \code{max(expression)} and \code{min(expression)} are the most extreme
#'  breaks. Rounding makes them human-readable. This functionality is provided
#'  by \link[scUtils]{closed_breaks_log2} and \link[scUtils]{closed_labels}.
#'       }
#'
#' If you insist on customizing, think of this function as a great starting point, you can simply
#' copy-paste the code after typing \code{feat} into your
#' console.
#' @examples
#'  # expression goes from 0 to 22:
#'  set.seed(100)
#'  feat(matrix(rnorm(2000, c(.1, 3)), ncol=2), rpois(1000, c(.1, 11)))
#'  # expression goes from 2 to 52:
#'  set.seed(100)
#'  feat(matrix(rnorm(2000, c(.1, 3)), ncol=2), rpois(1000, c(10, 31)))
#' @seealso
#'  \code{\link[ggplot2]{ggplot}},
#'  \code{\link[scUtils]{closed_labels}},
#'  \code{\link[scUtils]{closed_breaks_log2}}
#' @rdname feat
#' @export
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_cols
#' @importFrom ggplot2 aes
feat <- function(embedding, expression, legend_name="Expression") {
  assertthat::assert_that(is.vector(expression), is.numeric(expression))
  assertthat::assert_that(is.numeric(embedding[,1, drop=TRUE]),
                          is.numeric(embedding[,2, drop=TRUE]),
                          ncol(embedding)==2)
  assertthat::assert_that(nrow(embedding) == length(expression))
  assertthat::assert_that(assertthat::is.string(legend_name))
  assertthat::assert_that(all(expression >= 0))
  # save axis_names for later:
  axis_names <- if (!is.null(colnames(embedding))){colnames(embedding)}else{
    c("Dim1", "Dim2")}
  colnames(embedding) <- c("u1", "u2")
  u1 <- u2 <- NULL # avoids "no visible binding" warning when building package
  # convert to data.frame (e.g. to handle matrices, etc.)
  embedding <- data.frame(embedding)

  # avoid zeros as it causes errors with log2 breaks. Replacing with a 10th of
  # expression's non-zero minimum seems reasonable:
  has_zeros <- any(expression == 0)
  expression[expression==0] <- min(expression[expression!=0])/10

  ggplot2::ggplot(data = dplyr::bind_cols(embedding, expression=expression),
                  mapping = ggplot2::aes(u1, u2, col=expression)) +
      ggplot2::geom_point(size=.4) + ggplot2::coord_fixed() +
    ggplot2::xlab(axis_names[1]) + ggplot2::ylab(axis_names[2])+
    viridis::scale_color_viridis(
      trans="log2",
      breaks = closed_breaks_log2,
      labels=function(br) closed_labels(br, min_is_zero = has_zeros),
      na.value=viridisLite::viridis(1),
      name = legend_name) +
    ggplot2::theme(
      # axis.text =  ggplot2::element_text(color="grey"),
      # axis.ticks = ggplot2::element_line(color="grey"),
      # grey background without grid:
      panel.background = ggplot2::element_rect(fill="grey90"),
      panel.grid = ggplot2::element_blank() )

}













#' @title Closed breaks for log scale
#' @description Finds breaks that are powers of 2,
#' and forces inclusion of upper and lower limits
#' (displaying the closed interval).
#' Including limits specifically is particularly useful for ggplot2's color/fill,
#' as it
#' emphasizes the meaning of maximal/minimal color intensities (see examples).
#' @param lims Vector with lower and upper limits (in that order) of the data
#' that you want breaks for.
#' @return Numeric vector with breaks.
#' @details The \code{feat} function uses \code{closed_breaks_log2} to color by
#' gene expression,
#' where the maximal expression gives valuable
#' intuition for a gene's overall expression strength.
#' For x- or y-axis (\code{scale_*_log10}),
#' I still recommend \code{breaks_log} from the scales package.
#' @examples
#' # closed breaks include maximum, breaks_log do not:
#' closed_breaks_log2(lims = c(.01, 977.1))
#' scales::breaks_log()(c(.01, 977.1))
#' @seealso
#'  \code{\link[scUtils]{closed_labels}}
#' @rdname closed_breaks_log2
#' @export
#' @importFrom assertthat assert_that
closed_breaks_log2 <- function(lims) {
   lim1 <- lims[1]; lim2 <- lims[2]
  # get powers of two, but keep original lims:
  breaks <- 2^round(seq(log2(lim1), log2(lim2), length.out = 5))
  breaks[1] <- lim1
  breaks[length(breaks)] <- lim2
  # remove rounding above artifacts: non-uniqueness and breaks outside of lims
  breaks <- breaks[breaks >= lim1 & breaks <= lim2]
  breaks <- unique(breaks)
  # only two breaks looks poor; we'll place one more exactly in the middle:
  if(length(breaks) < 3){
    breaks <- c(lims[1], 2^mean(log2(lims)), lims[2])
  }
  return(breaks)
}






#' @title Human-readable labels for closed breaks
#' @description Complements the closed_breaks_log2 function.
#' @param x Vector of breaks for which to produce labels.
#' Typically, this is the output of \code{closed_breaks_log2}.
#' @param min_is_zero Should the smallest break be
#' displayed as zero (TRUE) or as the actual value (FALSE). Default: FALSE
#' @return Character vector with labels, used by \code{feat} function.
#' @details This is a helper for the \code{feat} function.
#' \code{feat} replaces numeric zeros with the next-smallest expression value
#' to avoid taking the logarithm of zero. \code{min_is_zero} can be used to
#' display the lowest break of the color scale as zero in these cases.
#' @examples
#'  # human readable output:
#'  closed_labels(c(.001111,.122, 0.5, 10, 100, 1800))
#' @seealso
#'  \code{\link[scales]{label_scientific}}
#'  \code{\link[scales]{label_number_auto}}
#' @rdname closed_labels
#' @importFrom dplyr case_when
#' @importFrom scales scientific
#' @export
closed_labels <- function(x, min_is_zero = FALSE) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr must be installed for this functionality.")
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    stop("scales must be installed for this functionality.")
  }
  x <- dplyr::case_when(
    x == min(x) & min_is_zero ~ as.character(0), # for feat function
    x == 0 ~ as.character(0),
    # I want maximum to be labelled more precicely than others:
    x == max(x) & base::abs(x) >= 1000 ~ scales::scientific(x,  digits = 2),
    base::abs(x) < .01 | base::abs(x) >= 1000 ~ scales::scientific(x,  digits = 0),
    x > 2 ~ as.character(round(x, digits=1)),  #
    TRUE ~ as.character(round(x, 2)))
  # remove leading zero because it looks nicer:
  return(base::gsub("^0.", ".", x))
}







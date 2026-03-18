#' Plot Phylogenetic Trees With Species Delimitation Partitions
#'
#' @description
#' `delim_autoplot()` returns a phylogenetic tree plotted using `ggtree` alongside
#' with a customized tile plot using [geom_tile][ggplot2::geom_tile] combined by 
#' [wrap_plots][patchwork::wrap_plots].
#'
#' @param delim Output from [delim_join].
#' @param tr A [treedata][tidytree::treedata-class] object. Both phylogram and
#' ultrametric trees are supported.
#' @param consensus Logical. Should the majority-vote consensus to be estimated?
#' @param n_match An Integer. If `consensus = TRUE`, threshold for majority-vote
#' calculations. See [delim_consensus] for details.
#' @param delim_order A character vector of species delimitation names ordered by user.
#' Default to NULL.
#' @param tbl_labs A [tbl_df][tibble::tbl_df] of customized labels for tree plotting. The
#' first column must match tip labels of the `tr` object, while the second column
#' should have customized labels.
#' @param col_vec A color vector for species delimitation partitions. See
#' [delim_brewer] for customized color palette options.
#' @param widths A numeric vector containing the relative widths of the tree and
#' species delimitation bars. See [wrap_plots][patchwork::wrap_plots] for details.
#' Defaults to `c(0.5, 0.2)`.
#' @param hexpand Numeric. Expand xlim of tree by a ratio of x axis range. Useful if
#' tiplabels become truncated when plotting. Default to `0.1`.
#'
#' @details
#' `delim_autoplot()` is a wrapper for tree plotting with associated data implemented
#' using `ggtree`, `ggplot2`, and `patchwork`. If `consensus = TRUE`,
#' a consensus bar will be plotted next to the species delimitation plot,
#' summarizing partitions across samples. If no consensus is reached, an "X" will be plotted instead.
#'
#' @return
#' A `patchwork` object.
#'
#' @author
#' Pedro S. Bittencourt, Rupert A. Collins.
#'
#' @examples
#' # view partitions using an ultrametric tree
#' p <- delim_autoplot(geophagus_delims, geophagus_beast)
#' p
#'
#' # view partitions using a phylogram
#' p1 <- delim_autoplot(geophagus_delims, geophagus_raxml)
#'
#' @export
delim_autoplot <- function(delim, tr, consensus = TRUE, n_match = NULL,
                           delim_order = NULL, tbl_labs = NULL, col_vec = NULL,
                           hexpand = 0.1, widths = c(0.5, 0.2)) {
  
  # check if `patchwork` is installed
  rlang::check_installed("patchwork", reason = "to run `delim_autoplot` properly.")
  
  if (!methods::is(tr, "treedata")) {
    cli::cli_abort(c("Tree file must be from class {.cls treedata}.",
      "i" = "You've supplied a tree file of class {.cls {class(tr)}}",
      "i" = "You may convert your tree file by using {.fun tidytree::as.treedata}"
    ))
  }

  if (is.null(tbl_labs)) {
    tbl_labs <- tibble::tibble(
      label = tidytree::tip.label(tr),
      labs = .data$label
    )

    cli::cli_warn("{cli::col_yellow({cli::symbol$warning})} Argument {.arg tbl_labs} not provided. Using tiplabels instead.")
  }

  if (ape::is.ultrametric(tr@phylo)) {
    p <- ggtree::ggtree(tr, ladderize = TRUE, color = "grey50", size = 1) %<+% tbl_labs

    pp <- p +
      ggtree::hexpand(ratio = hexpand) +
      ggtree::geom_tiplab(ggplot2::aes(label = get(colnames(tbl_labs)[2])), size = 3.5) +
      ggtree::geom_point2(ggplot2::aes(subset = !is.na(.data$posterior) & .data$posterior >= 0.95))
  } else {
    p <- ggtree::ggtree(tr, ladderize = TRUE, color = "grey50", size = 1) %<+% tbl_labs

    pp <- p +
      ggtree::hexpand(ratio = hexpand) +
      ggtree::geom_tiplab(ggplot2::aes(label = get(colnames(tbl_labs)[2])), size = 3.5, align = TRUE) +
      ggtree::geom_point2(ggplot2::aes(subset = !.data$isTip & .data$support >= 75))
  }

  if (is.null(delim_order)) {
    delim_order <- colnames(delim)[-1]

    cli::cli_warn("{cli::col_yellow({cli::symbol$warning})} Argument {.arg delim_order} not provided.
                  Using default order from {.arg {deparse(substitute(delim))}}.")
  }

  if (is.null(col_vec)) {
    cli::cli_warn(c("{cli::col_yellow({cli::symbol$warning})} Argument {.arg col_vec} not provided.
                  Customizing one using {.fn delim_brewer}.",
      "i" = "Please use {.fn delimtools::delim_brewer} to create even better colour palettes!"
    ))

    col_vec <- suppressWarnings(delimtools::delim_brewer(delim, package = NULL, palette = NULL, seed = NULL))
  }

  if (consensus == TRUE) {
    # reorder and turn into long format
    delim_long <- delim |>
      dplyr::arrange(match(labels, ggtree::get_taxa_name(pp))) |>
      delimtools::delim_consensus(n_match = n_match) |>
      dplyr::relocate(tidyselect::all_of(delim_order), .after = labels) |>
      tidyr::pivot_longer(
        cols = -labels,
        names_to = "method",
        values_to = "spp",
        cols_vary = "fastest"
      ) |>
      dplyr::mutate(
        method = as.factor(.data$method) |> forcats::fct_inorder(),
        spp = as.factor(.data$spp) |> forcats::fct_inorder(),
        label = labels,
        labels = as.factor(.data$labels) |> forcats::fct_inorder() |> forcats::fct_rev()
      )

    delim_tile <- ggplot2::ggplot(delim_long, ggplot2::aes(x = .data$method, y = .data$labels, color = .data$spp, fill = .data$spp)) +
      ggplot2::geom_tile(data = ~ subset(., !is.na(spp)), width = 0.5, show.legend = FALSE) +
      ggplot2::geom_point(data = ~ subset(., is.na(spp)), shape = 4, color = "black", show.legend = FALSE) +
      ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = c(0, 0))) +
      ggplot2::scale_x_discrete(position = "top", expand = ggplot2::expansion(add = c(0, 0.5))) +
      ggplot2::scale_fill_manual(values = col_vec, na.value = "transparent") +
      ggplot2::scale_color_manual(values = col_vec, na.value = "transparent") +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(color = "black", size = 10),
        axis.ticks.x = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )

    pp2 <- pp +
      ggplot2::coord_cartesian(xlim = ggfun::xrange(pp), ylim = ggfun::yrange(delim_tile), expand = FALSE) +
      ggtree::geom_treescale(color = "grey50", linesize = 1, fontsize = 3, y = ggfun::yrange(delim_tile)[1])
  } else {
    delim_long <- delim |>
      dplyr::arrange(match(labels, ggtree::get_taxa_name(pp))) |>
      dplyr::relocate(tidyselect::all_of(delim_order), .after = labels) |>
      tidyr::pivot_longer(
        cols = -labels,
        names_to = "method",
        values_to = "spp",
        cols_vary = "fastest"
      ) |>
      dplyr::mutate(
        method = as.factor(.data$method) |> forcats::fct_inorder(),
        spp = as.factor(.data$spp) |> forcats::fct_inorder(),
        label = labels,
        labels = as.factor(.data$labels) |> forcats::fct_inorder() |> forcats::fct_rev()
      )

    delim_tile <- ggplot2::ggplot(delim_long, ggplot2::aes(x = .data$method, y = .data$labels, color = .data$spp, fill = .data$spp)) +
      ggplot2::geom_tile(data = ~ subset(., !is.na(spp)), width = 0.5, show.legend = FALSE) +
      ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = c(0, 0))) +
      ggplot2::scale_x_discrete(position = "top", expand = ggplot2::expansion(add = c(0, 0.5))) +
      ggplot2::scale_fill_manual(values = col_vec, na.value = "transparent") +
      ggplot2::scale_color_manual(values = col_vec, na.value = "transparent") +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(color = "black", size = 10),
        axis.ticks.x = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )

    pp2 <- pp +
      ggplot2::coord_cartesian(xlim = ggfun::xrange(pp), ylim = ggfun::yrange(delim_tile), expand = FALSE) +
      ggtree::geom_treescale(color = "grey50", linesize = 1, fontsize = 3, y = ggfun::yrange(delim_tile)[1])
  }

  x <- patchwork::wrap_plots(pp2, delim_tile, widths = widths)

  return(x)
}

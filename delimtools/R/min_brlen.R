#' A function to report the smallest tip-to-tip distances in a phylogenetic tree
#'
#' @description
#' `min_brlen()` returns a table of smallest tip-to-tip distances in a phylogenetic tree.
#'
#' @param tree A path to tree file in Newick format, or a phylogenetic tree object of class [phylo][ape::phylo].
#' @param n Number of distances to report (default = 5).
#' @param verbose Logical of whether to print the result to screen (default = TRUE).
#'
#' @details
#' `min_brlen()` tabulates the smallest tip-to-tip distances in a phylogenetic tree
#' using [cophenetic.phylo][ape::cophenetic.phylo] and prints a table to screen.
#' This is useful when excluding identical or near-identical haplotypes
#' using the '--minbr' parameter in mPTP.
#'
#' @return
#' an object of class [tbl_df][tibble::tbl_df]
#'
#' @author
#' Rupert A. Collins
#'
#' @examples
#'
#' # estimate minimum branch length from raxml tree
#' min_brlen(ape::as.phylo(geophagus_raxml), n = 5)
#'
#' @export
min_brlen <- function(tree, n = 5, verbose = TRUE) {
  # checks

  if (methods::is(tree, "phylo")) {
    tr <- tree
  } else if (file.exists(tree)) {
    tr <- ape::read.tree(tree)
  } else {
    writeLines("\n")

    cli::cli_abort("Error. Please provide a phylogenetic tree object or a path to a Newick file that can be read by `ape`.")

    writeLines("\n")
  }

  # fun

  tr.dist <- ape::cophenetic.phylo(tr)

  pairs.tab <- tr.dist |>
    tibble::as_tibble(rownames = "tip1") |>
    tidyr::pivot_longer(-1, names_to = "tip2", values_to = "dist") |>
    dplyr::filter(.data$tip1 != .data$tip2)

  pairs.tab.cut <- pairs.tab |>
    dplyr::arrange(.data$dist) |>
    dplyr::count(.data$dist) |>
    dplyr::slice_head(n = n)

  # print

  if(verbose == TRUE) {
    writeLines("\n")

    cli::cli_alert_info("Printing {n} smallest tip-to-tip distances in a tree with {length(tr$tip.label)} tips ...")

    pairs.tab.cut |>
      knitr::kable(align = "lr") |>
      print()
  }

  invisible(pairs.tab.cut)
}

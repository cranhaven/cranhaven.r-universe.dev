#' Convert attached FASTA data to a tidy long table
#'
#' @param plan A data frame returned by [tol_attach_fasta()] or a FASTA plan
#'   with local files.
#' @param fasta_column Name of the FASTA list-column. If it is not present,
#'   [tol_attach_fasta()] is called.
#'
#' @return A tibble with one row per sequence.
#' @export
tol_fasta_long <- function(plan, fasta_column = "fasta") {
  if (!fasta_column %in% names(plan)) {
    plan <- tol_attach_fasta(plan, column = fasta_column)
  }

  rows <- lapply(seq_len(nrow(plan)), function(i) {
    fasta <- plan[[fasta_column]][[i]]
    if (!nrow(fasta)) {
      return(tibble::tibble())
    }

    tibble::tibble(
      sequence_id = plan$sequence_id[i],
      scientific_name = plan$scientific_name[i],
      order = plan$order[i],
      family = plan$family[i],
      genus = plan$genus[i],
      specific_epithet = plan$specific_epithet[i],
      fasta_file_name = plan$fasta_file_name[i],
      header = fasta$header,
      gene_id = extract_gene_id(fasta$header),
      sequence = fasta$sequence,
      width = fasta$width
    )
  })

  dplyr::bind_rows(rows)
}

#' Summarise FASTA content by species record
#'
#' @param plan A data frame returned by [tol_attach_fasta()] or a FASTA plan
#'   with local files.
#'
#' @return A tibble with one row per species/specimen FASTA.
#' @export
tol_fasta_summary <- function(plan) {
  tol_fasta_long(plan) |>
    dplyr::group_by(.data$sequence_id, .data$scientific_name) |>
    dplyr::summarise(
      n_sequences = dplyr::n(),
      n_genes = dplyr::n_distinct(.data$gene_id),
      total_bp = sum(.data$width, na.rm = TRUE),
      mean_bp = mean(.data$width, na.rm = TRUE),
      median_bp = stats::median(.data$width, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Choose a gene shared by multiple downloaded FASTA files
#'
#' @param plan A data frame returned by [tol_attach_fasta()] or a FASTA plan
#'   with local files.
#' @param min_records Minimum number of records that must contain the gene.
#'
#' @return A tibble ranked by number of records and median width.
#' @export
tol_common_genes <- function(plan, min_records = 3) {
  tol_fasta_long(plan) |>
    dplyr::filter(!is.na(.data$gene_id), .data$gene_id != "") |>
    dplyr::group_by(.data$gene_id) |>
    dplyr::summarise(
      n_records = dplyr::n_distinct(.data$sequence_id),
      median_width = stats::median(.data$width, na.rm = TRUE),
      min_width = min(.data$width, na.rm = TRUE),
      max_width = max(.data$width, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$n_records >= min_records) |>
    dplyr::arrange(dplyr::desc(.data$n_records), dplyr::desc(.data$median_width))
}

#' Build an illustrative tree from one shared gene
#'
#' This helper is intended for exploration. It extracts one homologous gene from
#' each selected FASTA, trims sequences to their common minimum length, computes
#' raw DNA distances with `ape`, and returns an `hclust` tree plus the distance
#' matrix. Rigorous phylogenetics should use explicit alignment and model
#' selection outside this helper.
#'
#' @param plan A data frame returned by [tol_attach_fasta()] or a FASTA plan
#'   with local files.
#' @param gene_id Optional gene id. If `NULL`, the most shared gene is selected.
#' @param min_records Minimum records required to build the tree.
#'
#' @return A list with `tree`, `distances`, `gene_id`, `sequences`, and `note`.
#' @export
tol_build_gene_tree <- function(plan, gene_id = NULL, min_records = 3) {
  if (!requireNamespace("ape", quietly = TRUE)) {
    stop("Package `ape` is required to build trees.", call. = FALSE)
  }

  long <- tol_fasta_long(plan)

  if (is.null(gene_id)) {
    genes <- tol_common_genes(plan, min_records = min_records)
    if (!nrow(genes)) {
      stop("No shared gene found for at least ", min_records, " records.", call. = FALSE)
    }
    gene_id <- genes$gene_id[1]
  }

  selected <- long |>
    dplyr::filter(.data$gene_id == gene_id) |>
    dplyr::group_by(.data$sequence_id, .data$scientific_name) |>
    dplyr::slice_max(.data$width, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()

  if (nrow(selected) < min_records) {
    stop(
      "Gene `", gene_id, "` is present in only ", nrow(selected),
      " records; at least ", min_records, " are required.",
      call. = FALSE
    )
  }

  min_width <- min(nchar(selected$sequence), na.rm = TRUE)
  selected <- selected |>
    dplyr::mutate(
      tree_label = make.names(.data$scientific_name, unique = TRUE),
      sequence_trimmed = substr(.data$sequence, 1, min_width)
    )

  seqs <- stats::setNames(selected$sequence_trimmed, selected$tree_label)
  dna <- ape::as.DNAbin(strsplit(seqs, ""))
  distances <- ape::dist.dna(dna, model = "raw", pairwise.deletion = TRUE)
  tree <- stats::hclust(distances, method = "average")

  list(
    tree = tree,
    distances = distances,
    gene_id = gene_id,
    sequences = selected,
    note = paste(
      "Exploratory UPGMA tree from one shared gene trimmed to",
      min_width,
      "bp. Use an explicit alignment workflow for publication-grade phylogenies."
    )
  )
}

#' Plot recovered gene counts with ggplot2
#'
#' @param records Species records returned by [tol_species_index()] or
#'   [tol_search_species()].
#'
#' @return A ggplot object.
#' @export
tol_plot_gene_recovery <- function(records) {
  ggplot2::ggplot(
    records,
    ggplot2::aes(
      x = stats::reorder(.data$scientific_name, .data$no_of_genes_recovered),
      y = .data$no_of_genes_recovered
    )
  ) +
    ggplot2::geom_col(fill = "#2f6f73") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Genes recuperados",
      title = "Genes recuperados por especimen"
    ) +
    ggplot2::theme_minimal()
}

#' Plot FASTA summary with ggplot2
#'
#' @param summary A summary returned by [tol_fasta_summary()].
#'
#' @return A ggplot object.
#' @export
tol_plot_fasta_summary <- function(summary) {
  ggplot2::ggplot(
    summary,
    ggplot2::aes(
      x = stats::reorder(.data$scientific_name, .data$total_bp),
      y = .data$total_bp
    )
  ) +
    ggplot2::geom_col(fill = "#7a9e3f") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Pares de bases en FASTA",
      title = "Tamano total de FASTA por especimen"
    ) +
    ggplot2::theme_minimal()
}

#' Plot an hclust tree with ggplot2
#'
#' @param tree_result A result returned by [tol_build_gene_tree()] or an
#'   `hclust` object.
#' @param label_offset Numeric offset used to place species labels beyond the
#'   end of each terminal branch. If `NULL`, an offset is computed from the tree
#'   height.
#' @param label_size Label text size.
#'
#' @return A ggplot object.
#' @export
tol_plot_tree <- function(tree_result, label_offset = NULL, label_size = 3) {
  tree <- if (inherits(tree_result, "hclust")) tree_result else tree_result$tree
  dendro <- stats::as.dendrogram(tree)
  segments <- dendrogram_segments(dendro)
  labels <- dendrogram_labels(dendro)
  max_distance <- max(c(segments$y, segments$yend), na.rm = TRUE)
  if (is.null(label_offset)) {
    label_offset <- max(max_distance * 0.03, .Machine$double.eps)
  }
  labels$label_y <- -label_offset

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = segments,
      ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
      linewidth = 0.4
    ) +
    ggplot2::geom_text(
      data = labels,
      ggplot2::aes(x = .data$x, y = .data$label_y, label = .data$label),
      hjust = 0,
      size = label_size
    ) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_y_reverse(
      limits = c(max_distance, -label_offset),
      expand = ggplot2::expansion(mult = c(0.05, 0.02))
    ) +
    ggplot2::labs(x = NULL, y = "Distancia", title = "Arbol exploratorio") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(5.5, 90, 5.5, 5.5)
    )
}

extract_gene_id <- function(header) {
  sub("\\s+.*$", "", header)
}

dendrogram_segments <- function(dendro) {
  rows <- list()

  walk <- function(node) {
    x <- attr(node, "x")
    y <- attr(node, "height")

    if (stats::is.leaf(node)) {
      return(invisible(NULL))
    }

    for (child in node) {
      child_x <- attr(child, "x")
      child_y <- attr(child, "height")
      rows[[length(rows) + 1L]] <<- tibble::tibble(x = x, y = y, xend = child_x, yend = y)
      rows[[length(rows) + 1L]] <<- tibble::tibble(x = child_x, y = y, xend = child_x, yend = child_y)
      walk(child)
    }

    invisible(NULL)
  }

  walk(assign_dendrogram_x(dendro))
  dplyr::bind_rows(rows)
}

dendrogram_labels <- function(dendro) {
  labels <- list()

  walk <- function(node) {
    if (stats::is.leaf(node)) {
      labels[[length(labels) + 1L]] <<- tibble::tibble(
        x = attr(node, "x"),
        y = 0,
        label = attr(node, "label")
      )
      return(invisible(NULL))
    }

    for (child in node) {
      walk(child)
    }
    invisible(NULL)
  }

  walk(assign_dendrogram_x(dendro))
  dplyr::bind_rows(labels)
}

assign_dendrogram_x <- function(dendro) {
  i <- 0

  assign_x <- function(node) {
    if (stats::is.leaf(node)) {
      i <<- i + 1
      attr(node, "x") <- i
      return(node)
    }

    children <- lapply(node, assign_x)
    for (j in seq_along(children)) {
      node[[j]] <- children[[j]]
    }
    attr(node, "x") <- mean(vapply(children, function(x) attr(x, "x"), numeric(1)))
    node
  }

  assign_x(dendro)
}

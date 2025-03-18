#' Visualise registration results
#'
#' @param x Input object.
#'  - For [plot.res_greatR()]: registration results, output of the [register()] registration process.
#'  - For [plot.summary.res_greatR()]: registration results summary, output of [summary()].
#'  - For [plot.dist_greatR()]: pairwise distances between reference and query time points, output of [calculate_distance()].
#' @param type Type of plot.
#'  - For both [plot.res_greatR()] and  [plot.dist_greatR()]: whether to use registration "result" (default) or "original" time points.
#'  - For [plot.summary.res_greatR()]: whether to show "all" genes (default) or only "registered" ones.
#' @param type_dist Type of marginal distribution. Can be either "histogram" (default), or "density".
#' @param genes_list Optional vector indicating the \code{gene_id} values to be plotted.
#' @param show_rep_mean Whether to show \code{replicate} mean values.
#' @param match_timepoints If \code{TRUE}, will match query time points to reference time points.
#' @param ncol Number of columns in the plot grid. By default this is calculated automatically.
#' @param bins Number of bins to use when \code{type_dist} = "histogram". By default, 30.
#' @param alpha Optional opacity of the points in the scatterplot.
#' @param scatterplot_size Vector \code{c(width, height)} specifying the ratio of width and height of the scatterplot with respect to stretch and shift distribution plots.
#' @param title Optional plot title.
#' @param ... Arguments to be passed to methods (ignored).
#'
#' @name plot
#' @return
#'  - For [plot.res_greatR()]: plot of genes of interest after registration process (\code{type = "result"}) or showing original time points (\code{type = "original"}).
#'  - For [plot.dist_greatR()]: distance heatmap of gene expression profiles over time between reference and query.
#'  - For [plot.summary.res_greatR()]: TODO.
NULL
# > NULL

#' @rdname plot
#' @export
plot.res_greatR <- function(x,
                            type = c("result", "original"),
                            genes_list = NULL,
                            show_rep_mean = FALSE,
                            ncol = NULL,
                            title = NULL,
                            ...) {
  # Suppress "no visible binding for global variable" note
  gene_id <- NULL
  accession <- NULL
  timepoint_reg <- NULL
  expression_value <- NULL

  # Validate parameters
  type <- match.arg(type)

  # Parse results
  data <- x$data
  model_comparison <- x$model_comparison
  reference <- attr(data, "ref")
  query <- attr(data, "query")
  scaling_method <- x$fun_args$scaling_method

  # Select genes to be plotted
  genes <- unique(data[, gene_id])

  if (any(!is.null(genes_list))) {
    if (!inherits(genes_list, "character")) {
      stop(
        cli::format_error(c(
          "{.var genes_list} must be a {.cls character} vector.",
          "x" = "You supplied vectors with {.cls {class(genes_list)}} values."
        )),
        call. = FALSE
      )
    }

    data <- data[data$gene_id %in% genes_list]
    model_comparison <- model_comparison[model_comparison$gene_id %in% genes_list]
  } else if (length(genes) > 50) {
    cli::cli_alert_info("The first 25 genes will be shown. To override this, use the {.var genes_list} parameter.")
    model_comparison <- model_comparison[model_comparison$gene_id %in% genes[1:25]]
  }

  # Parse model_comparison object
  gene_facets <- parse_gene_facets(model_comparison, type)
  data <- data[gene_facets, on = "gene_id"]

  # Plot labels
  if (type == "result") {
    timepoint_var <- "timepoint_reg"
    x_lab <- "Registered time"
  } else {
    timepoint_var <- "timepoint"
    x_lab <- "Time point"
  }

  # Construct plot
  gg_registered <- ggplot2::ggplot(data) +
    ggplot2::aes(
      x = !!ggplot2::sym(timepoint_var),
      y = expression_value,
      color = accession
    ) +
    ggplot2::geom_point() +
    {
      if (show_rep_mean) ggplot2::stat_summary(fun = mean, geom = "line")
    } +
    ggplot2::facet_wrap(~gene_facet, scales = "free", ncol = ncol) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    theme_greatR() +
    ggplot2::labs(
      title = title,
      x = x_lab,
      y = ifelse(scaling_method == "none", "Expression", "Scaled expression"),
      color = NULL
    )

  # Add model curve layers
  if (type == "result") {
    # Count registered and non-registered genes
    registered_count <- length(model_comparison[model_comparison$registered, gene_id])
    non_registered_count <- length(model_comparison[!model_comparison$registered, gene_id])

    # Get curves for H1
    if (registered_count > 0) {
      preds_H1 <- get_H1_model_curves(data, model_comparison)
      preds_H1 <- merge(preds_H1, gene_facets, by = "gene_id")
    }
    # Get curves for H2
    if (non_registered_count > 0) {
      preds_H2 <- get_H2_model_curves(data, model_comparison, reference, query)
      preds_H2 <- merge(preds_H2, gene_facets, by = "gene_id")
    }

    # Bind predictions
    if (non_registered_count == 0) {
      preds <- preds_H1
    } else if (registered_count == 0) {
      preds <- preds_H2
    } else {
      preds <- rbind(preds_H1, preds_H2)
    }

    gg_registered <- gg_registered +
      ggplot2::geom_line(
        mapping = ggplot2::aes(
          x = timepoint_reg,
          y = expression_value,
          group = interaction(accession, gene_id)
        ),
        data = preds,
        linetype = "dashed",
        linewidth = 0.5
      )
  }

  # Fix legend order
  gg_registered <- gg_registered +
    ggplot2::scale_color_manual(
      breaks = c(reference, query),
      values = greatR_palettes$disc
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(linetype = NULL))
    )

  return(gg_registered)
}

#' Parse \code{gene_facet} faceting variable for plotting
#'
#' @noRd
parse_gene_facets <- function(model_comparison, type) {
  # Suppress "no visible binding for global variable" note
  gene_id <- NULL
  registered <- NULL
  stretch <- NULL
  shift <- NULL
  BIC_diff <- NULL

  if (type == "result") {
    gene_facets <- model_comparison[, .(
      gene_id,
      gene_facet = paste0(
        gene_id, " - ", ifelse(registered, "REG", "NON-REG"),
        ifelse(
          registered,
          paste0("\n", "BIC diff: ", round(BIC_diff, 2), ", stretch: ", round(stretch, 2), ", shift: ", round(shift, 2)),
          ""
        )
      )
    )]
  } else {
    gene_facets <- model_comparison[, .(
      gene_id,
      gene_facet = gene_id
    )]
  }

  return(gene_facets)
}

#' Get curves for Hypothesis H1
#'
#' @noRd
get_H1_model_curves <- function(data, model_comparison) {
  # Suppress "no visible binding for global variable" note
  gene_id <- NULL
  accession <- NULL
  timepoint_reg <- NULL

  # Get registered genes only
  genes <- unique(model_comparison[model_comparison$registered, gene_id])

  # Fit using cubic splines with K+3 params for each gene
  fits <- lapply(
    genes,
    function(gene) {
      fit_spline_model(
        data[data$gene_id == gene],
        x = "timepoint_reg"
      )
    }
  )
  names(fits) <- genes

  # Predict query expression values
  preds <- data.table::rbindlist(
    lapply(
      genes,
      function(gene) {
        data <- unique(data[data$gene_id == gene][, .(gene_id, timepoint_reg)])
        data <- data.table::data.table(
          gene_id = gene,
          timepoint_reg = seq(min(data$timepoint_reg), max(data$timepoint_reg), length.out = 25)
        )
        data[, .(gene_id, timepoint_reg, expression_value = stats::predict(fits[gene][[1]], newdata = data))]
      }
    )
  )

  preds[, accession := character()][]

  return(preds)
}

#' Get curves for Hypothesis H2
#'
#' @noRd
get_H2_model_curves <- function(data, model_comparison, reference, query) {
  # Suppress "no visible binding for global variable" note
  gene_id <- NULL
  accession <- NULL
  timepoint_reg <- NULL

  # Get non-registered genes only
  genes <- unique(model_comparison[!model_comparison$registered, gene_id])

  # Get reference and query data
  data_ref <- data[data$accession == reference]
  data_query <- data[data$accession == query]

  # Fit using cubic splines with K+3 params for each gene
  fits_ref <- lapply(
    genes,
    function(gene) {
      fit_spline_model(
        data_ref[data_ref$gene_id == gene],
        x = "timepoint_reg"
      )
    }
  )
  fits_query <- lapply(
    genes,
    function(gene) {
      fit_spline_model(
        data_query[data_query$gene_id == gene],
        x = "timepoint_reg"
      )
    }
  )
  names(fits_ref) <- genes
  names(fits_query) <- genes

  # Predict query expression values
  preds_ref <- data.table::rbindlist(
    lapply(
      genes,
      function(gene) {
        data <- unique(data_ref[data_ref$gene_id == gene][, .(gene_id, timepoint_reg)])
        data <- data.table::data.table(
          gene_id = gene,
          timepoint_reg = seq(min(data$timepoint_reg), max(data$timepoint_reg), length.out = 25)
        )
        data[, .(gene_id, timepoint_reg, expression_value = stats::predict(fits_ref[gene][[1]], newdata = data))]
      }
    )
  )

  preds_query <- data.table::rbindlist(
    lapply(
      genes,
      function(gene) {
        data <- unique(data_query[data_query$gene_id == gene][, .(gene_id, timepoint_reg)])
        data <- data.table::data.table(
          gene_id = gene,
          timepoint_reg = seq(min(data$timepoint_reg), max(data$timepoint_reg), length.out = 25)
        )
        data[, .(gene_id, timepoint_reg, expression_value = stats::predict(fits_query[gene][[1]], newdata = data))]
      }
    )
  )

  # Combine reference and query curves
  preds <- rbind(
    preds_ref[, accession := reference],
    preds_query[, accession := query]
  )[]

  return(preds)
}

#' @rdname plot
#' @export
plot.dist_greatR <- function(x,
                             type = c("result", "original"),
                             match_timepoints = TRUE,
                             title = NULL,
                             ...) {
  # Suppress "no visible binding for global variable" note
  timepoint_ref <- NULL
  timepoint_query <- NULL
  distance <- NULL

  # Validate parameters
  type <- match.arg(type)
  if (type == "result") {
    data <- x$result
  } else {
    data <- x$original
  }

  # Retrieve accession values from results
  reference <- attr(data, "ref")
  query <- attr(data, "query")

  # Synchronise time points
  if (all(match_timepoints, type == "result")) {
    equal_timepoints <- intersect(data$timepoint_ref, data$timepoint_query)
    data <- data[timepoint_query %in% equal_timepoints & timepoint_ref %in% equal_timepoints]
  }

  # Construct plot
  gg_distance <- ggplot2::ggplot(data) +
    ggplot2::aes(
      x = as.factor(timepoint_query),
      y = as.factor(timepoint_ref),
      fill = log(distance)
    ) +
    ggplot2::geom_tile() +
    theme_greatR() +
    ggplot2::theme(
      legend.position = "top",
      legend.justification = "right",
      legend.margin = ggplot2::margin(0, 0, -5, 0),
      legend.title = ggplot2::element_text(size = 10),
      legend.key.height = ggplot2::unit(5, "pt")
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(label.position = "top")
    ) +
    ggplot2::scale_fill_gradientn(colors = greatR_palettes$cont) +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::labs(
      title = title,
      x = query,
      y = reference
    )

  # Synchronise time points
  if (all(match_timepoints, type == "result")) {
    gg_distance <- gg_distance + ggplot2::coord_fixed()
  }

  return(gg_distance)
}

#' @rdname plot
#' @export
plot.summary.res_greatR <- function(x,
                                    type = c("all", "registered"),
                                    type_dist = c("histogram", "density"),
                                    genes_list = NULL,
                                    bins = 30,
                                    alpha = NA,
                                    scatterplot_size = c(4, 3),
                                    title = NULL,
                                    ...) {
  # Suppress "no visible binding for global variable" note
  stretch <- NULL
  shift <- NULL
  registered <- NULL

  # Validate parameters
  type <- match.arg(type)
  type_dist <- match.arg(type_dist)

  # Parse data
  x <- x$reg_params
  if (type == "registered") {
    x <- x[x$registered, ]
  }
  x$registered <- factor(x$registered, levels = c(TRUE, FALSE), labels = c("REG", "NON-REG"))

  # Select genes to be plotted
  if (any(!is.null(genes_list))) {
    if (!inherits(genes_list, "character")) {
      stop(
        cli::format_error(c(
          "{.var genes_list} must be a {.cls character} vector.",
          "x" = "You supplied vectors with {.cls {class(genes_list)}} values."
        )),
        call. = FALSE
      )
    }

    x <- x[x$gene_id %in% genes_list]
  }

  # Scatterplot of stretch and shift (center)
  point_shape <- ifelse(nrow(x) <= 1000, 19, 21)
  if (is.na(alpha)) {
    point_alpha <- ifelse(nrow(x) <= 1000, 1, 0.5)
  } else {
    point_alpha <- alpha
  }

  plot_center <- ggplot2::ggplot(x) +
    ggplot2::aes(
      x = stretch,
      y = shift,
      color = registered
    ) +
    ggplot2::geom_point(alpha = point_alpha, shape = point_shape) +
    ggplot2::scale_color_manual(
      breaks = c("REG", "NON-REG"),
      values = greatR_palettes$hist
    ) +
    ggplot2::labs(x = "Stretch", y = "Shift", color = NULL) +
    theme_greatR() +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(shape = 19, alpha = 1))
    )

  # Marginal density of stretch (top)
  plot_top <- ggplot2::ggplot(x) +
    ggplot2::aes(
      x = stretch,
      color = registered
    ) +
    ggplot2::scale_y_continuous(n.breaks = 4) +
    ggplot2::scale_color_manual(
      breaks = c("REG", "NON-REG"),
      values = greatR_palettes$hist
    ) +
    theme_greatR() +
    ggplot2::theme(
      legend.position = "none",
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::labs(title = title)

  # Marginal density of shift (right)
  plot_right <- ggplot2::ggplot(x) +
    ggplot2::aes(
      x = shift,
      color = registered
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(n.breaks = 4) +
    ggplot2::scale_color_manual(values = greatR_palettes$hist) +
    theme_greatR() +
    ggplot2::theme(
      legend.position = "none",
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )

  # Select geom for marginals
  marginal_hist <- list(
    ggplot2::geom_histogram(fill = "transparent", position = "identity", bins = bins),
    ggplot2::labs(y = "Count")
  )

  marginal_dens <- list(
    ggplot2::geom_density(),
    ggplot2::labs(y = "Density")
  )

  if (type_dist == "histogram") {
    plot_top <- plot_top + marginal_hist
    plot_right <- plot_right + marginal_hist
  } else if (type_dist == "density") {
    plot_top <- plot_top + marginal_dens
    plot_right <- plot_right + marginal_dens
  }

  # Construct patchwork
  plots_list <- list(
    plot_top,
    patchwork::guide_area(),
    plot_center,
    plot_right
  )

  patch <- patchwork::wrap_plots(
    plots_list,
    guides = "collect",
    ncol = 2,
    heights = c(1, scatterplot_size[2]),
    widths = c(scatterplot_size[1], 1)
  )

  return(patch)
}

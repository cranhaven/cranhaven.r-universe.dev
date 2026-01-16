#' Get the default ggplot color palette or a color palette based on the ggplot
#' palette, but with sub-colors that differ in their luminance
#'
#' This is an adapted version of https://stackoverflow.com/a/8197703
#'
#' @param n The number of colors in the color palette. If `n` is a vector,
#'          get a color palette, that has `length(n)` different base colors.
#'          For each item in n, the actual colors are equally spaced on
#'          in the luminance range `l` between the upper and lower value.
#' @param h The hue range.
#' @param l A vector of length 2 that describes the luminance range
#'
#' @return A vector of `sum(n)` colors strings
ggplot_colors <- ggplot_colours <- function(n = 6,
                                            h = c(0, 360) + 15,
                                            l = c(65, 65)) {
  if (length(n) == 1) {
    if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360 / n
    grDevices::hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
  } else {
    if ((diff(h) %% 360) < 1) {
      h[2] <- h[2] - 360 / length(n)
    }
    base_h <- seq(h[1], h[2], length = length(n))
    unlist(lapply(seq_len(length(n)), function(i) {
      h <- base_h[i]
      if (n[i] == 1) {
        ls <- (l[1] + l[2]) / 2
      } else {
        ls <- seq(l[1], l[2], length.out = n[i])
      }

      grDevices::hcl(h = h, c = 100, l = ls)
    }))
  }
}

#' Plot the relationship of integration sites as a graph.
#'
#' Integration sites will be represented as nodes in the graph, while their
#' mutual similarity is indicated by the line size and opaqueness of the lines
#' between them.
#'
#' @param readouts The integration site readouts that this spring model is
#'                 based on.
#' @param mapping The reconstructed mapping from clones to integration sites.
#'                This is represented as a matrix with two columns "IS" and
#'                "Clone".
#' @param gt The ground truth mapping from clones to integration sites, if
#'           available. Same structure as `mapping`.
#' @param sim The similarity matrix holding the similarities between all
#'            integration sites.
#' @param rec_pal A named vector color palette holding colors for each
#'                integration site. Will be used as the fill color for the
#'                nodes.
#' @param clone_pal A named vector color palette holding colors for each
#'                  integration site. Will be used as the line color for the
#'                  nodes.
#' @param line_color The line color to use for the edges of the graph.
#' @param seed A seed that will be set using `set.seed()` to ensure
#'             consistent behaviour with the layout that is provided by
#'             `igraph`.
#'
#' @return A ggplot object that contains the generated graph.
weighted_spring_model <- function(readouts, mapping, gt,
                                  sim = get_similarity_matrix(
                                    readouts,
                                    self = NA,
                                    upper = FALSE,
                                    parallel = FALSE),
                                  rec_pal = NULL,
                                  clone_pal = NULL,
                                  line_color = "#009900FF",
                                  seed = 4711L) {
  net <- igraph::graph_from_adjacency_matrix(
    adjmatrix = sim,
    mode = "undirected",
    diag = FALSE,
    weighted = TRUE
  )
  set.seed(seed)
  layout <- igraph::layout_with_fr(net, grid = "nogrid")

  v <- data.frame(
    x = layout[, 1],
    y = layout[, 2],
    IS = colnames(sim)
  )

  if (!missing(mapping) && !is.null(mapping)) {
    v[, "Rec"] <- plyr::mapvalues(
      v$IS,
      mapping[, "IS"],
      mapping[, "Clone"],
      warn_missing = FALSE
    )
    v[, "Rec"] <- as.factor(v$Rec)
    if (is.null(rec_pal)) {
      rec_pal <- ggplot_colors(n = length(levels(v$Rec)))
      names(rec_pal) <- levels(v$Rec)
    }
  } else {
    v[, "Rec"] <- as.factor(0)
    rec_pal <- c("0" = "grey50")
  }

  if (!missing(gt) && !is.null(gt)) {
    v[, "Clone"] <- plyr::mapvalues(
      v$IS,
      gt[, "IS"],
      gt[, "Clone"]
    )
    v[, "Clone"] <- as.factor(v$Clone)
    if (is.null(clone_pal)) {
      clone_pal <- ggplot_colors(n = length(levels(v$Clone)))
      names(clone_pal) <- levels(v$Clone)
    }
  } else {
    v[, "Clone"] <- as.factor(0)
    clone_pal <- c("0" = "grey25")
  }

  # TODO: This apparently gives the same order...
  # find a way to make this more robust
  el <- igraph::get.edgelist(graph = net, names = FALSE)
  ew <- igraph::get.edge.attribute(net, "weight")

  e <- data.frame(
    from = el[, 1],
    to = el[, 2],
    weight = ew**4,
    Width = 1
  )
  e$weight <- (e$weight - min(e$weight)) / (max(e$weight) - min(e$weight))

  e$from_x <- v$x[e$from]
  e$from_y <- v$y[e$from]
  e$to_x <- v$x[e$to]
  e$to_y <- v$y[e$to]

  x_min <- min(c(e$from_x, e$to_x))
  x_max <- max(c(e$from_x, e$to_x))
  y_min <- min(c(e$from_y, e$to_y))
  y_max <- max(c(e$from_y, e$to_y))

  p <- ggplot2::ggplot(v,
                       ggplot2::aes_string(
                         x = "x",
                         y = "y",
                         fill = "Rec",
                         col = "Clone")) +
    ggplot2::geom_segment(
      data = e,
      ggplot2::aes_string(
        x = "from_x", y = "from_y", xend = "to_x",
        yend = "to_y", size = "3", alpha = "weight"
      ),
      color = line_color, inherit.aes = FALSE
    ) +
    ggplot2::geom_point(size = 12, shape = 21, stroke = 3) +
    ggplot2::geom_text(ggplot2::aes_string(label = "IS"), color = "#000000FF") +
    ggplot2::scale_fill_manual(values = rec_pal) +
    ggplot2::scale_color_manual(values = clone_pal) +
    ggplot2::scale_size_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::theme_void() +
    ggplot2::expand_limits(
      x = c(x_min - 0.12, x_max + 0.12),
      y = c(y_min - 0.12, y_max + 0.12))

  return(p)
}

#' Create a stacked area plot that represents the abundance of integration
#' sites over time.
#'
#' @param readouts The readouts of the integration sites over time.
#'
#' @param aes An additional `ggplot2::aes` object, that will be used as the
#'            plots main aesthetic. Note, that the `geom_area` object
#'            overwrites some of these aesthetics. Useful if you later on
#'            want to add additional elements to the plot.
#' @param col A color palette for integration sites that should be colored. Any
#'            integration site not in this named vector will be colored
#'            `gray50`. This takes precedence over `only` and `rec`.
#' @param only A list of integration sites that should be colored with the
#'             default ggplot2 color palette. Any other integration site is
#'             colored `gray50`. Takes precedence over `rec`.
#' @param rec A matrix containing the columns "IS" and "Clone". Integration
#'            sites will be colored by the clone they belong to. The colors for
#'            the clones are the default ggplot2 ones.
#' @param time A function that extracts the time component from the measurement
#'             (i.e. column)-names. Will be applied to the measurements.
#' @param facet A function that extracts a value from the measurement names and
#'              splits the plot into different facets by that values. Useful,
#'              for example if you have measurements that are sorted for the
#'              cell type and you want to split these into facets.
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
bushmanplot <- function(readouts,
                        aes = NULL,
                        col = NULL,
                        only = NULL,
                        rec = NULL,
                        time = NULL,
                        facet = NULL) {
  readouts[is.na(readouts)] <- 0
  readouts <- t(t(readouts) / colSums(readouts))

  if (!is.null(rec)) {
    rn <- rownames(readouts)
    others <- rn[!(rn %in% unique(rec[, "IS"]))]
    others <- colSums(readouts[others, ])
    readouts <- rbind(readouts[unique(rec[, "IS"]), ], others)
  }

  m <- reshape2::melt(readouts[rowSums(readouts, na.rm = TRUE) != 0, ])
  colnames(m) <- c("IS", "Measurement", "Contribution")
  m[, "IS"] <- factor(m[, "IS"], ordered = FALSE, levels = unique(m$IS))

  if (is.null(time)) {
    m[, "Time"] <- factor(m$Measurement, ordered = TRUE)
  } else if (is.function(time)) {
    m[, "Time"] <- time(m$Measurement)
    x_breaks <- unique(m$Time)
    x_labels <- as.character(x_breaks)
  } else {
    stop("time needs to be either a function or NULL")
  }

  if (!is.null(facet)) {
    if (is.function(facet)) {
      m[, "Facet"] <- facet(m[, "Measurement"])
      # Optimization: in each facet, remove ISs that have 0 reads and thus
      # would not be drawn
      m <- as.data.frame(m %>%
        dplyr::group_by(.data$IS, .data$Facet) %>%
        dplyr::filter(sum(.data$Contribution) > 0) %>%
        dplyr::ungroup())

      for (f in unique(m$Facet)) {
        if (nrow(unique(m[m$Facet == f, "Time", drop = FALSE])) < 2) {
          m <- m[m$Facet != f, ]
        }
      }

      m[, "Facet"] <- factor(m[, "Facet"], ordered = FALSE)
    } else {
      stop("facet needs to be either a function or NULL")
    }
  }

  if (is.null(aes)) {
    p <- ggplot2::ggplot(m)
  } else {
    p <- ggplot2::ggplot(m, aes)
  }

  p <- p + ggplot2::geom_area(
    ggplot2::aes_string(
      group = "IS",
      fill = "IS",
      x = "Time",
      y = "Contribution"))

  if (!is.null(col)) {
    if (length(col) == nrow(readouts)) {
      cols <- col
    } else {
      cols <- rep("gray50", nrow(readouts))
      names(cols) <- rownames(readouts)
      cols[names(col)] <- col
    }

    p <- p + ggplot2::scale_fill_manual(values = cols)
  } else if (!is.null(only)) {
    cols <- rep("gray50", nrow(readouts))
    cols_fc <- ggplot_colors(n = nrow(readouts))
    repl <- rownames(readouts) %in% only
    cols[repl] <- cols_fc[repl]
    names(cols) <- rownames(readouts)

    p <- p + ggplot2::scale_fill_manual(values = cols)
  } else if (!is.null(rec)) {
    cols <- rep("gray50", nrow(readouts))
    names(cols) <- rownames(readouts)

    clones <- rec[, "Clone"]
    cols[rec[, "IS"]] <- clones

    cols <- plyr::mapvalues(cols,
      unique(clones),
      ggplot_colors(n = length(unique(clones))),
      warn_missing = FALSE
    )

    p <- p + ggplot2::scale_fill_manual(values = cols)
  }

  if (is.integer(m$Time)) {
    p <- p + ggplot2::scale_x_continuous(expand = c(0, 0),
                                         breaks = x_breaks,
                                         labels = x_labels) +
      ggplot2::geom_vline(ggplot2::aes_string(xintercept = "Time"))
  } else if (is.factor(m$Time)) {
    p <- p + ggplot2::scale_x_discrete(expand = c(0, 0))
  }
  p <- p + ggplot2::scale_y_continuous(expand = c(0, 0))

  if (!is.null(m$Facet)) {
    p <- p + ggplot2::facet_wrap(~Facet, ncol = 1, strip.position = "right")
  }

  return(p)
}

.time_fn <- function(x) {
  as.integer(unlist(lapply(strsplit(as.character(x), "_"), utils::head, 1)))
}
.comp_fn <- function(x) {
  unlist(lapply(strsplit(as.character(x), "_"), function(y) {
    paste(utils::tail(y, -1), collapse = "_")
  }))
}

bushmanplot_split_compartment <- function(time_fn = .time_fn,
                                        comp_fn = .comp_fn,
                                        ...) {
  return(bushmanplot(time = time_fn, facet = comp_fn, ...))
}

#' Show line plots of all integration sites over time, split into facets by
#' their respective clone.
#'
#' @param bd The readouts of the integration sites over time.
#'
#' @param rec A matrix with columns "IS" and "Clone", that describes for each
#'            integration site, which clone it belongs to.
#' @param order Integration site names will be converted to a factor. This
#'              allows to give the order for this factor, as it influences the
#'              order in which the lines are drawn.
#' @param mapping A ggplot2 aesthetics mapping that will be merged with the
#'                aesthetics used by this plot.
#' @param sim A similarity matrix giving the similarities for each pair of
#'            integration sites. Used if `silhouette_values` is `TRUE` to
#'            calculate the silhouette score.
#' @param silhouette_values A boolean value that determines whether the
#'                          silhouette values for each clone should be
#'                          calculated and added to the facet labels.
#'                          Requires `sim` to be present.
#' @param singletons Whether to show clones that only have a single integration
#'                   site.
#' @param zero_values How to handle values that are zero. If `TRUE`, they remain
#'                    zero and subsequently, a the measurement the line drops to
#'                    zero. If `FALSE`, the values are removed and a gap in the
#'                    line is shown.
#'
#' @importFrom rlang .data
lineplot_split_clone <- function(bd,
                               rec,
                               order = NULL,
                               mapping = NULL,
                               sim = NULL,
                               silhouette_values = !is.null(sim),
                               singletons = TRUE,
                               zero_values = TRUE) {
  if (!is.matrix(bd)) {
    bd <- as.matrix(bd)
  }

  m <- reshape2::melt(bd)
  colnames(m) <- c("IS", "Time", "Count")
  m[, "Clone"] <- plyr::mapvalues(
    m[, "IS"],
    rec[, "IS"],
    as.integer(rec[, "Clone"])
  )

  if (!is.null(order)) {
    m[, "IS"] <- factor(x = m[, "IS"], levels = order)
  } else {
    m[, "IS"] <- factor(x = m[, "IS"])
  }

  m[, "Time"] <- as.factor(m[, "Time"])
  m[, "Clone"] <- as.factor(m[, "Clone"])

  if (silhouette_values) {
    sil <- cluster::silhouette(as.numeric(rec[, "Clone"]), max(sim) - sim)
    ssil <- summary(sil)
    from <- names(ssil$clus.avg.widths)
    clone_size <- table(rec[, "Clone"])
    clone_size <- clone_size[as.character(sort(as.integer(names(clone_size))))]
    clone_size <- as.vector(clone_size)
    to <- sprintf("%s: %.3f [%d IS]",
                  names(ssil$clus.avg.widths),
                  ssil$clus.avg.widths,
                  clone_size)
    m[, "Clone"] <- plyr::mapvalues(
      x = m[, "Clone"],
      from = from,
      to = to
    )
  }

  if (!singletons) {
    m <- m[!endsWith(as.character(m$Clone), "[1 IS]"), , drop = FALSE]
  }

  if (!zero_values) {
    m[m$Count == 0, "Count"] <- NA
  }

  am <- c(mapping, ggplot2::aes_string(x = "Time",
                                       y = "Count",
                                       color = "Clone",
                                       group = "IS"))
  class(am) <- "uneval"
  p <- ggplot2::ggplot(m, mapping = am) +
    ggplot2::geom_path() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ .data$Clone)

  return(p)
}

#' Plots time series data, which consists of multiple measurements over
#' time / place (cols) of different clones / integration sites (rows).
#'
#' @export
#' @param x The data to plot.
#' @param ... Further arguments are ignored.
#' @return A ggplot object, which can be used to further individualize or to
#'         plot directly.
plot.timeseries <- function(x, ...) {
  return(bushmanplot(x) + ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.title.x = ggplot2::element_blank(),
      text = ggplot2::element_text(size = 16),
      plot.margin = ggplot2::margin(.5, .5, .5, .5, "cm")
    ))
}

#' Plots R^2 of two integration sites
#'
#' @export
#' @param dat The matrix that holds the values
#' @param is1 The name of the first row
#' @param is2 The name of the second row
#' @return A ggplot object, which can be used to further individualize or to
#'         plot directly.
plot_rsquare <- function(dat, is1, is2) {
  return(ggplot2::ggplot(
    as.data.frame(t(as.data.frame(dat[c(is1, is2), ],
                                  row.names = c("Count1", "Count2")))),
    ggplot2::aes_string(x = "Count1", y = "Count2")
  ) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ 0 + x, se = FALSE) +
    ggplot2::geom_point() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 75, hjust = 1),
      legend.position = "none",
      axis.title.x = ggplot2::element_blank(),
      text = ggplot2::element_text(size = 16),
      plot.margin = ggplot2::margin(.5, .5, .5, .5, "cm")
    ) +
    ggplot2::xlab(bquote("Abundance (" ~ 1^st ~ " IS)")) +
    ggplot2::ylab(bquote("Abundance (" ~ 2^nd ~ " IS)")))
}

#' Plots the similarity of integration sites
#'
#' @export
#' @param x The matrix that holds the similarity values
#' @param na.rm whether NA values should be deleted beforehand
#' @param ... Further arguments are ignored.
#' @return A ggplot object, which can be used to further individualize or to
#'         plot directly.
plot.ISSimilarity <- function(x, na.rm = TRUE, ...) {
  mdat <- reshape2::melt(x)
  colnames(mdat) <- c("IS1", "IS2", "Similarity")

  if (na.rm) mdat <- mdat[!is.na(mdat$Similarity), ]

  mdat[, "IS1"] <- as.factor(as.numeric(mdat$IS1))
  mdat[, "IS2"] <- as.factor(as.numeric(mdat$IS2))

  return(ggplot2::ggplot(
    mdat,
    ggplot2::aes_string(x = "IS1", y = "IS2", fill = "Similarity")
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradientn(
      colours = grDevices::colorRampPalette(
        rev(RColorBrewer::brewer.pal(11, "Spectral")), space = "Lab")(100)) +
    #    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    #    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 75, hjust = 1),
      text = ggplot2::element_text(size = 16),
      axis.text = ggplot2::element_text(size = 7)
    ))
}

#' Plots the clustering based on a clustering object
#'
#' @export
#' @param x The clustering object.
#' @param ... Further arguments are ignored.
#' @return A ggplot object, which can be used to further individualize or to
#'         plot directly.
plot.clusterObj <- function(x, ...) {
  return(weighted_spring_model(
    readouts = x$readouts,
    mapping = x$mapping,
    gt = x$mapping,
    sim = x$sim
  ) +
    ggplot2::guides(col = FALSE) +
    ggplot2::theme(legend.position = "none"))
}

stacked_barplot <- function(readouts, comp_fn = NULL, rec = NULL) {
  readouts[is.na(readouts)] <- 0

  if (!is.null(rec)) {
    bc_dat_cl <- do.call(
      rbind,
      lapply(unique(rec[, "Clone"]), function(x) {
        colSums(readouts[rec[rec[, "Clone"] == x, "IS"], , drop = FALSE])
      })
    )
    bc_dat_ot <- colSums(
      readouts[!(rownames(readouts) %in% rec[, "IS"]), ])
    readouts <- rbind(bc_dat_cl, bc_dat_ot)
    rownames(readouts) <- c(unique(rec[, "Clone"]), "others")
  }

  readouts <- t(t(readouts) / colSums(readouts))

  mdat <- reshape2::melt(as.matrix(readouts))
  colnames(mdat) <- c("IS", "Measurement", "Value")

  if (!is.null(comp_fn)) {
    mdat[, "Facet"] <- comp_fn(mdat[, "Measurement"])
    ggplot2::ggplot(mdat,
                    ggplot2::aes_string(
                      x = "Measurement",
                      y = "Value",
                      fill = "IS")) +
      ggplot2::geom_bar(position = "stack", stat = "identity") +
      ggplot2::facet_grid(~Facet, scales = "free_x") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 75, hjust = 1)) +
      ggplot2::ylab("Relative abundance")
  } else {
    ggplot2::ggplot(mdat,
                    ggplot2::aes_string(
                      x = "Measurement",
                      y = "Value",
                      fill = "IS")) +
      ggplot2::geom_bar(position = "stack", stat = "identity") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 75, hjust = 1)) +
      ggplot2::ylab("Relative abundance")
  }
}

### * TODO

# Clean-up this file

### * All functions in this file are exported

### * plot.networkModel()

#' Plot observations/trajectories/predictions from a network model
#'
#' @param x A \code{networkModel} object.
#' @param ... Passed to \code{plot_nm}.
#'
#' @return Called for side effect (plotting).
#' 
#' @method plot networkModel
#' 
#' @export

plot.networkModel <- function(x, ...) {
    plot_nm(x, ...)
}

### * plot.ready_for_unit_plot()

#' Plot output from \code{split_to_unit_plot}
#'
#' @param x A \code{ready_for_unit_plot} object.
#' @param ... Passed to \code{plot_nm}.
#'
#' @return Called for side effect (plotting).
#' 
#' @method plot ready_for_unit_plot
#'
#' @export

plot.ready_for_unit_plot <- function(x, ...) {
    plot_nm(x, ...)
}

### * plot.networkModelStanfit()

#' @method plot networkModelStanfit
#'
#' @export

plot.networkModelStanfit <- function(x, ...) {
    plot_traces(x, ...)
}

### * plot.tidy_flows_mcmc.list()

#' @method plot tidy_flows_mcmc.list
#'
#' @export

plot.tidy_flows_mcmc.list <- function(x, ...) {
    plot_traces(x, ...)
}

### * plot.derived.mcmc.list()

#' @method plot derived.mcmc.list
#'
#' @export

plot.derived.mcmc.list <- function(x, ...) {
    plot_traces(x, ...)
}

### * traceplot()

#' Plot mcmc.list objects
#'
#' @param x A \code{coda::mcmc.list} object.
#' @param ... Passed to \code{plot_traces}.
#'
#' @return Called for side effect (plotting).
#' 
#' @export

traceplot <- function(x, ...) {
    plot_traces(x, ...)
}

### * mcmc_heatmap()

#' Draw a heatmap based on the correlations between parameters
#'
#' Note that the colors represent the strength of the correlations (from 0 to
#' 1), but do not inform about their sign. The method used to calculate
#' correlation coefficients is Spearman's rho.
#'
#' @param x A \code{coda::mcmc.list} object.
#' @param col Optional, vectors of colors defining the color ramp. Default uses
#'     the divergent palette "Blue-Red 2" from the colorspace package.
#' @param ... Passed to \code{\link{heatmap}}.
#'
#' @return Called for side effect (plotting).
#' 
#' @importFrom grDevices heat.colors
#' @importFrom grDevices colorRampPalette
#' @importFrom stats dist
#' @importFrom stats cor
#' @importFrom stats hclust
#' @importFrom stats heatmap
#' 
#' @export

mcmc_heatmap = function(x, col = NULL, ...) {
    chain <- x
    f <- 0.45 # Multiplicative factor to adjust the label margin
    # Prepare color palette (generated with the colorspace Berlin palette)
    if (is.null(col)) {
        col <- rev(c("#4A6FE3", "#7086E1", "#8F9DE1", "#ABB4E2", "#C7CBE3", "#E2E2E2", 
                     "#E6C4C9", "#E5A5B1", "#E28699", "#DB6581", "#D33F6A"))
    }
    cols <- colorRampPalette(col)(1025)
    # Draw heatmap
    data <- cor(do.call(rbind, chain), method = "spearman")
    col_low_index <- 513 + floor(min(data) * 512)
    col_high_index <- 513 + ceiling(max(data) * 512)
    cols <- cols[col_low_index:col_high_index]
    labels <- colnames(data)
    labels <- gsub("^lossRate_", "__", labels)
    labels <- gsub("^portion_", "__", labels)
    labels <- gsub("^uptakeRate_from_", "__", labels)
    maxLabels <- max(sapply(labels, nchar))
    labels <- sapply(colnames(data), varnameToExp)
    margin <- maxLabels * f
    heatmap(data, labCol = labels, labRow = labels,
            distfun = function(x) dist(abs(x)), 
            hclustfun = function(x) hclust(abs(x), method = "ward.D2"),
            scale = "none", col = cols, margins = rep(margin, 2), ...)
}

### * sankey()

#' Draw a Sankey plot for a network and estimated flows
#'
#' @param topo A topology.
#' @param nodes Optional, a tibble containing the properties of the nodes. It
#'     should have a `comp` column with the same entries as the topology. It
#'     cannot have `x` and `y` entries. If it has a `label` entry, it will
#'     replace the `comp` values for node labels.
#' @param flows A tibble containing the values of the flows in the topology. If
#'     NULL (the default), all flows have same width in the plot.
#' @param layout String, node-placing algorithm to use from the ggraph package
#'     (e.g. "stress"). The ggraph package itself uses some algoritms from the
#'     igraph package. See the Details in the help of
#'     \code{\link[ggraph]{layout_tbl_graph_igraph}} for available
#'     algorithms. The ggraph package must be installed for this argument to be
#'     taken into account. Currently, only the "left2right" and "stress" layout
#'     are implemented in detail, and any other layout will use rough defaults
#'     for the aesthetic adjustments. Other layouts which are kind of working
#'     are "kk", "lgl", "fr", "dh", "mds". Some of those produce
#'     non-reproducible node locations (at least I haven't managed to reproduce
#'     them even by setting the RNG seed before calling the function).
#' @param new Boolean, create a new page for the plot?
#' @param debug Boolean, if TRUE then draw a lot of shapes to help with
#'     debugging.
#' @param node_f,edge_f Multiplicative factor to adjust node and edge size.
#' @param node_s String defining how node size is calculated. The effect of the
#'     string also depends on the chosen layout.
#' @param edge_n Integer, number of interpolation points along each edge.
#' @param cex_lab,cex.lab Expansion factor for label size (both arguments are
#'     synonyms).
#' @param fit Boolean, if TRUE try to fit all the graphical elements inside the
#'     canvas.
#'
#' @return Mostly called for its side effect (plotting), but also returns
#'     invisible the scene object describing the Sankey plot. Note that the
#'     structure of this object is experimental and might change in the future!
#' 
#' @examples
#' library(magrittr)
#' 
#' topo <- topo(trini_mod)
#' sankey(topo, debug = TRUE)
#' sankey(topo, layout = "stress")
#' sankey(topo(aquarium_mod), layout = "stress", edge_f = 0.5)
#'
#' m <- new_networkModel() %>%
#'     set_topo(c("subs -> NH3 -> subs",
#'                "NH3 -> Q, E", "E -> Q -> E",
#'                "E -> D, M")) %>%
#'     set_steady("subs") %>%
#'     set_prop_family("normal_sd")
#' ggtopo(m)
#' sankey(topo(m), layout = "stress")
#'
#' # Debug visualization
#'
#' ## Helper functions
#' flows_from_topo <- function(x) {
#'     x <- unclass(x) # Remove the "topo" class to treat it as a matrix
#'     n_comps <- ncol(x)
#'     links <- which(x > 0)
#'     from <- links %/% n_comps + 1
#'     to <- links %% n_comps
#'     links <- tibble::tibble(from = from, to = to)
#'     for (i in seq_len(nrow(links))) {
#'         if (links$to[i] == 0) {
#'             links$from[i] <- links$from[i] - 1
#'             links$to[i] <- n_comps
#'         }
#'         stopifnot(x[links$to[i], links$from[i]] > 0)
#'     }
#'     flows <- tibble::tibble(from = colnames(x)[links$from],
#'                             to = rownames(x)[links$to])
#'     return(flows)
#' }
#' nodes_from_topo <- function(x) {
#'     nodes <- tibble::tibble(comp = colnames(x),
#'                             label = colnames(x))
#'     return(nodes)
#' }
#'
#' t <- topo(trini_mod)
#' nodes <- nodes_from_topo(t)
#' nodes$label <- as.list(nodes$label)
#' nodes$label[[2]] <- latex2exp::TeX("$\\beta$")
#' nodes$size <- runif(nrow(nodes), 1, 2)
#' flows <- flows_from_topo(t)
#' flows$width <- runif(nrow(flows), 0.2, 2)
#' z <- sankey(t, nodes = nodes, flows = flows, layout = "left2right",
#'             debug = TRUE, node_f = 1, edge_f = 0.9, edge_n = 32,
#'             cex_lab = 1.5)
#'
#' # Stress layout
#' y <- new_networkModel() %>%
#'         set_topo(c("subs -> NH3 -> subs",
#'                    "NH3 -> Q, E", "E -> Q -> E",
#'                    "E -> D, M")) %>%
#'         set_steady("subs") %>%
#'             set_prop_family("normal_sd")
#' y <- topo(y)
#' nodes <- nodes_from_topo(y)
#' nodes$size <- runif(nrow(nodes), 1, 10)
#' ggtopo(y, edge = "fan")
#' flows <- flows_from_topo(y)
#' flows$width <- runif(nrow(flows), 0.2, 5)
#' z <- sankey(y, nodes = nodes, flows = flows, debug = FALSE, edge_n = 32,
#'             edge_f = 0.4, node_s = "prop")
#'
#' # Another example
#' r <- new_networkModel() %>%
#'     set_topo("infusion -> plasma -> body -> plasma") %>%
#'     set_steady(c("infusion", "body"))
#' r <- topo(r)
#' ggtopo(r, edge = "fan")
#' sankey(r, debug = TRUE, edge_f = 0.2)
#' 
#' @export
#'

sankey <- function(topo, nodes = NULL, flows = NULL, layout = NULL, new = TRUE,
                   debug = FALSE, node_f = 1, edge_f = 1, node_s = "auto",
                   edge_n = 32, cex_lab = NULL, cex.lab = NULL, fit = TRUE) {
    # Process arguments
    if (!is.null(cex_lab) & !is.null(cex.lab)) {
        message("Both `cex_lab` and `cex.lab` provided. Using `cex_lab`.")
    }
    if (is.null(cex_lab) & !is.null(cex.lab)) {
        cex_lab <- cex.lab
    }
    if (is.null(cex_lab)) {
        cex_lab <- 1
    }
    # Create canvas
    if (new) { grid::grid.newpage() }
    if (!debug) {
        dev.hold()
        on.exit(dev.flush())
    }
    top_vp <- grid::viewport()
    grid::pushViewport(top_vp)
    canvas_vp <- make_and_push_ortho_vp(width = 0.9, height = 0.9,
                                        debug = debug)
    # Get default layout if needed
    layout <- sankey_get_layout(topo, layout)
    # Build node tibble if needed
    if (is.null(nodes)) {
        nodes <- tibble::tibble(comp = colnames(topo))
    }
    if (!"size" %in% colnames(nodes)) {
        nodes <- tibble::add_column(nodes, size = 1)
    }
    # Get unit flows if needed
    if (is.null(flows)) {
        flows <- flows_from_topo(topo)
    }
    if (!"width" %in% colnames(flows)) {
        flows <- tibble::add_column(flows, width = 1)
    }
    # Adjust flow widths
    total_width <- 1
    flows$width <- flows$width / sum(flows$width) * total_width * edge_f
    # 1) place_nodes
    scene <- sankey_place_nodes(topo, nodes, flows, layout,
                                xlim = canvas_vp$xscale,
                                ylim = canvas_vp$yscale)
    # 2) place_edge_sockets_on_nodes
    scene <- sankey_place_edge_sockets_on_nodes(scene, topo, nodes, flows, layout)
    # 3) calc_node_shape
    scene <- sankey_calc_node_shape(scene, topo, nodes, flows, layout, node_f = node_f,
                                    xlim = canvas_vp$xscale, node_s = node_s)
    # 4) adjust_node_locations
    scene <- sankey_adjust_node_locations(scene, topo, nodes, flows, layout)
    # 5) adjust_edge_sockets
    scene <- sankey_adjust_edge_sockets(scene, topo, nodes, flows, layout)
    # 6) calc_edge_socket_coordinates
    scene <- sankey_calc_edge_socket_coordinates(scene, topo, nodes, flows, layout)
    # 7) place_edge_backbones
    scene <- sankey_place_edge_backbones(scene, topo, nodes, flows, layout, n = edge_n)
    # 8) place_labels
    scene <- sankey_place_labels(scene, topo, nodes, flows, layout, cex_lab = cex_lab)
    # Draw elements
    elements_limits <- sankey_get_elements_lims(scene)
    ratio_x <- diff(elements_limits[["xlim"]]) / diff(canvas_vp$xscale)
    ratio_y <- diff(elements_limits[["ylim"]]) / diff(canvas_vp$yscale)
    ratio <- max(ratio_x, ratio_y)
    xscale_fit <- (canvas_vp$xscale - mean(canvas_vp$xscale)) * ratio + mean(elements_limits[["xlim"]])
    yscale_fit <- (canvas_vp$yscale - mean(canvas_vp$yscale)) * ratio + mean(elements_limits[["ylim"]])
    if (fit) {
        # Create a new canvas vp
        canvas_fit_vp <- grid::dataViewport(xscale = xscale_fit,
                                            yscale = yscale_fit,
                                            name = "ortho_canvas_fit")
        grid::pushViewport(canvas_fit_vp)
    }
    sankey_draw_edges(scene[["edges"]], debug = debug)
    sankey_draw_nodes(scene[["nodes"]], debug = debug, node_s = node_s)
    sankey_draw_labels(scene[["labels"]], debug = debug)
    if (fit) {
        # Pop extra canvas viewport
        grid::popViewport(1)
    }
    # Pop viewports
    grid::popViewport(3)
    # Return elements
    return(invisible(scene))
}

### * quick_sankey()

#' Draw a Sankey plot with basic defaults
#' 
#' @param flows A tibble containing flows (output from
#'     \code{\link{tidy_flows}}). For now it should have an "average_flow"
#'     column in the tibbles of the "flows" list column.
#' @param ... Passed to \code{\link{sankey}}.
#'
#' @return Mostly called for its side effect (plotting), but also returns
#'     invisible the scene object describing the Sankey plot. Note that the
#'     structure of this object is experimental and might change in the future!
#' 
#' @export
#' 

quick_sankey <- function(flows, ...) {
    `!!` <- rlang::`!!`
    flows <- dplyr::bind_rows(lapply(flows$flows, as.data.frame))
    stopifnot(all(c("from", "to", "average_flow") %in% colnames(flows)))
    flows <- na.omit(flows[, c("from", "to", "average_flow")])
    flows <- dplyr::group_by(flows, `!!`(rlang::sym("from")),
                             `!!`(rlang::sym("to")))
    flows <- dplyr::summarize(flows, width = mean(`!!`(rlang::sym("average_flow"))),
                              .groups = "drop")
    # Build topo from flows table
    topo <- make_topology(flows, from = "from", to = "to")
    sankey(topo = topo, nodes = nodes_from_topo(topo), flows = flows, ...)
}


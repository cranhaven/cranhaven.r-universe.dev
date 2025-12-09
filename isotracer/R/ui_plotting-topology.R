### * All functions in this file are exported

### * ggtopo (generic)

#' Plot a topology
#'
#' A quick plot using ggraph
#'
#' @param x A network model or a topology matrix.
#' @param layout Optional, layout to use (e.g. "sugiyama", "kk", "stress")
#' @param edge "fan" (the default) or "line" or "curve".
#' @param ... Passed to the methods.
#'
#' @return A ggplot2 plot.
#' 
#' @examples
#' if (requireNamespace("ggraph")) {
#'   ggtopo(aquarium_mod, edge = "line")
#' }
#'
#' @export

ggtopo <- function(x, layout = "auto", edge = "fan", ...) {
    UseMethod("ggtopo")
}

### * ggtopo.networkModel

#' Plot a network topology
#'
#' A quick plot using ggraph
#'
#' @param x A topology matrix.
#' @param layout Optional, layout to use (e.g. "sugiyama", "kk", "stress")
#' @param edge "curve" (the default) or "line".
#' @param ... Not used for now.
#'
#' @return A ggplot2 plot.
#' 
#' @examples
#' if (requireNamespace("ggraph")) {
#'   ggtopo(aquarium_mod, edge = "line")
#'   ggtopo(trini_mod)
#' }
#'
#' @export

ggtopo.networkModel <- function(x, layout = "auto", edge = "fan", ...) {
    topos <- topo(x, simplify = FALSE)
    if (length(unique(topos)) > 1) {
        message("Plotting the topology of the first row of the networkModel.")
    }
    topo <- topos[[1]]
    ggtopo(topo, layout = layout, edge = edge)
}

### * ggtopo.topology

#' Plot a topology
#'
#' A quick plot using ggraph
#'
#' @param x A topology matrix.
#' @param layout Optional, layout to use (e.g. "sugiyama", "kk", "stress")
#' @param edge "curve" (the default), "line" or "fan".
#' @param ... Not used for now.
#'
#' @return A ggplot2 plot.
#' 
#' @examples
#' if (requireNamespace("ggraph")) {
#'   z <- topo(aquarium_mod)
#'   ggtopo(z)
#'   ggtopo(z, edge = "line")
#' 
#'   z <- topo(trini_mod)
#'   ggtopo(z)
#'
#'   # For finer control, one can build a tbl_graph from the topology and
#'   # use ggraph directly
#'   x <- as_tbl_graph(z)
#'   library(ggraph)
#'   ggraph(x) + geom_edge_link()
#' }
#'
#' @export

ggtopo.topology <- function(x, layout = "auto", edge = "fan", ...) {
    `!!` <- rlang::`!!`
    topo <- x
    if (!edge %in% c("curve", "line", "fan")) {
        stop("edge should be either \"curve\", \"fan\", or \"line\".")
    }
    if (!requireNamespace("ggraph", quietly = TRUE)) {
        stop("Package \"ggraph\" needed for this function to work. Please install it.",
             call. = FALSE)
    }
    x <- as_tbl_graph(topo)
    # https://stackoverflow.com/questions/49989158/how-to-have-a-removed-from-a-ggraph-plot-legend
    GeomLabel <- ggplot2::GeomLabel
    GeomLabel$draw_key <- function(data, params, size) {
        ggplot2::draw_key_rect(data)
    }
    # Check that the random seed  is not reset by graphlayouts functions
    if (exists(".Random.seed", .GlobalEnv)) {
        prev_seed <- .GlobalEnv[[".Random.seed"]]
    } else { prev_seed <- NULL }
    msg_ggraph <- capture_msg(ggraph::ggraph(x, layout = layout))
    if (!is.null(msg_ggraph) && msg_ggraph$message == "Multiple parents. Unfolding graph\n") {
        message("\"stress\" layout cannot handle this topology. ",
                "Using \"kk\" layout instead.")
        layout <- "kk"
    }
    g <- ggraph::ggraph(x, layout = layout)
    if (exists(".Random.seed", .GlobalEnv)) {
        new_seed <- .GlobalEnv[[".Random.seed"]]
    }  else { new_seed <- NULL }
    if (!identical(prev_seed, new_seed)) {
        stop("Random seed was reset when calling ggraph::ggraph() in ggtopo().\n",
             "This is unwanted behaviour, please report it as a bug to the isotracer authors.")
    }
    if (edge == "curve") {
        g <- g +
            ggraph::geom_edge_diagonal(ggplot2::aes(start_cap = ggraph::label_rect(`!!`(rlang::sym("node1.name"))),
                                                    end_cap = ggraph::label_rect(`!!`(rlang::sym("node2.name")))),
                                       arrow = grid::arrow(length = grid::unit(1, "line")))
    } else if (edge == "fan") {
        g <- g +
            ggraph::geom_edge_fan(ggplot2::aes(start_cap = ggraph::label_rect(`!!`(rlang::sym("node1.name"))),
                                                    end_cap = ggraph::label_rect(`!!`(rlang::sym("node2.name")))),
                                       arrow = grid::arrow(length = grid::unit(1, "line")))
    } else {
        g <- g +
            ggraph::geom_edge_link(ggplot2::aes(start_cap = ggraph::label_rect(`!!`(rlang::sym("node1.name"))),
                                                end_cap = ggraph::label_rect(`!!`(rlang::sym("node2.name")))),
                                   arrow = grid::arrow(length = grid::unit(1, "line")))
    }
    g <- g +
        ggraph::geom_node_label(ggplot2::aes(label = `!!`(rlang::sym("name")),
                                    fill = `!!`(rlang::sym("steady_state"))),
                                col = "black",
                                fontface = 1,
                                label.padding = grid::unit(0.5, "line"),
                                label.r = grid::unit(0.5, "line")) +
        ggplot2::coord_cartesian(clip = "off") +
        ggraph::theme_graph(base_family = "sans") # https://github.com/thomasp85/ggraph/issues/67
    g <- g + ggplot2::scale_fill_manual(values = c("FALSE" = "#a6cee3",
                                                   "TRUE" = "#b2df8a"),
                                        labels = c("FALSE" = "No",
                                                   "TRUE" = "Yes")) +
        ggplot2::labs(fill = "Steady state")
    if (length(attr(topo, "steadyState")) == 0) {
        g <- g + ggplot2::theme(legend.position = "none")
    }
    return(g)
}

### * ggflows()

#' A quick-and-dirty way of visualizing relative flows in a network
#'
#' @param x A tibble with the flow estimates, with columns "from", "to", and
#'     "flow".
#' @param layout Optional, layout to use (e.g. "sugiyama", "kk", "stress")
#' @param edge "curve" (the default), "line" or "fan".
#' @param max_width Optional, numeric giving the maximum edge width (minimum
#'     width is always 1).
#' @param legend Boolean, display edge width legend?
#' @param ... Not used.
#'
#' @return A ggplot2 plot.
#' 
#' @examples
#' if (requireNamespace("ggraph")) {
#'   z <- tibble::tribble(
#'                ~from,               ~to,            ~flow,
#'      "leavesAndStem", "rootsAndRhizome", 333.929866077124,
#'         "lowerWater", "rootsAndRhizome", 4425.15780019304,
#'    "rootsAndRhizome",   "leavesAndStem", 525.208837577916,
#'         "upperWater",   "leavesAndStem", 11224.0814971855
#'   )
#'   ggflows(z)
#'   ggflows(z, max_width = 15)
#' }
#'
#' @export
#' 

ggflows <- function(x, layout = "auto", edge = "fan", max_width, legend = TRUE, ...) {
    `!!` <- rlang::`!!`
    if (! all(c("from", "to", "flow") %in% colnames(x))) {
        stop("`x` must have at least columns \"from\", \"to\", and \"flow\".")
    }
    if (!requireNamespace("ggraph", quietly = TRUE)) {
        stop("Package \"ggraph\" needed for this function to work. Please install it.",
             call. = FALSE)
    }
    if (!requireNamespace("tidygraph", quietly = TRUE)) {
        stop("Package \"tidygraph\" needed for this function to work. Please install it.",
             call. = FALSE)
    }
    x <- x[, c("from", "to", "flow")]
    if (!edge %in% c("curve", "line", "fan")) {
        stop("edge should be either \"curve\", \"fan\", or \"line\".")
    }
    # https://stackoverflow.com/questions/49989158/how-to-have-a-removed-from-a-ggraph-plot-legend
    # https://stackoverflow.com/questions/56173310/how-to-adjust-the-width-of-edges-by-weight-in-ggraph-network-in-r
    GeomLabel <- ggplot2::GeomLabel
    GeomLabel$draw_key <- function(data, params, size) {
        ggplot2::draw_key_rect(data)
    }
    g <- ggraph::ggraph(x, layout = layout)
    if (edge == "curve") {
        g <- g +
            ggraph::geom_edge_diagonal(ggplot2::aes(start_cap = ggraph::label_rect(`!!`(rlang::sym("node1.name"))),
                                                    end_cap = ggraph::label_rect(`!!`(rlang::sym("node2.name"))),
                                                    width = `!!`(rlang::sym("flow"))),
                                       arrow = grid::arrow(length = grid::unit(1, "line")))
    } else if (edge == "fan") {
        g <- g +
            ggraph::geom_edge_fan(ggplot2::aes(start_cap = ggraph::label_rect(`!!`(rlang::sym("node1.name"))),
                                               end_cap = ggraph::label_rect(`!!`(rlang::sym("node2.name"))),
                                               width = `!!`(rlang::sym("flow"))),
                                       arrow = grid::arrow(length = grid::unit(1, "line")))
    } else {
        g <- g +
            ggraph::geom_edge_link(ggplot2::aes(start_cap = ggraph::label_rect(`!!`(rlang::sym("node1.name"))),
                                                end_cap = ggraph::label_rect(`!!`(rlang::sym("node2.name"))),
                                                width = `!!`(rlang::sym("flow"))),
                                   arrow = grid::arrow(length = grid::unit(1, "line")))
    }
    g <- g +
        ggraph::geom_node_label(ggplot2::aes(label = `!!`(rlang::sym("name"))),
                                fill = grey(0.9),
                                fontface = 1,
                                label.padding = grid::unit(0.5, "line"),
                                label.r = grid::unit(0.5, "line")) +
        ggplot2::coord_cartesian(clip = "off") +
        ggraph::theme_graph(base_family = "sans") # https://github.com/thomasp85/ggraph/issues/67
    if (!missing(max_width)) {
        if (max_width < 1) {
            stop("`max_width` must be >= 1.")
        }
        g <- g +
            ggraph::scale_edge_width(range = c(1, max_width))
    }
    if (!legend) {
        g <- g + ggplot2::theme(legend.position = "none")
    }
    return(g)
}

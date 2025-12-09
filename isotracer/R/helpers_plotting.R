### * TODO

# Clean-up this file

### * None of the functions in this file is exported

### * plot_nm()

#' Plot a network model object
#'
#' Reminder: A network model object is basically a tibble. Each row is one
#' experimental replicate (also called "group"). The tibble might have the
#' columns "observations", "prediction" and "trajectory". Those columns contain
#' the data that can be plotted.
#'
#' @param x A network model object. Alternatively, it can also be the output
#'     from \code{split_to_unit_plot()} that the user has filtered themselves.
#' @param facet_row,facet_column Optional, either can be "group" or
#'     "compartment". Define how facetting is performed.
#' @param scale Define how y-scaling is performed within each panel (or
#'     cell). Can be one of "auto" (scaling on a per-panel basis), "all"
#'     (scaling shared by all panels) and "row" (scaling per row). Note that
#'     the x-scaling is always done for "all" (i.e. shared across all panels).
#' @param type Either "prop", "size" or "both"
#' @param newpage If FALSE, add the plot to the current page.
#' @param xlab,ylab Labels for x and y axes.
#' @param margins Figure margins.
#' @param grid Boolean, draw a grid?
#' @param ylab.size Y-axis label for size data.
#' @param ylab.prop Y-axis label for proportion data.
#' @param legend Boolean, draw legend?
#' @param log Boolean, apply log transform to y axis?
#' @param .colComps Colors for compartments.
#' @param comps Optional, vector of compartment names giving the order in which
#'     they should be shown in the plot.
#' @param keep Optional, vector of names of compartments to keep.
#' @param drop Optional, vector of names of compartments to drop.
#'
#' @importFrom grDevices dev.hold
#' @importFrom grDevices dev.flush
#' 
#' @return NULL
#'
#' @keywords internal
#' @noRd

plot_nm <- function(x, facet_row = NULL, facet_column = NULL, scale = "auto",
                    type = "both", newpage = TRUE, xlab = NULL, ylab = NULL,
                    margins = c(0.5, 0.5, 1, 1), grid = TRUE,
                    ylab.size = NULL, ylab.prop = NULL, legend = TRUE,
                    log = FALSE, .colComps = NULL, comps, keep, drop) {
    if (!"group" %in% colnames(x)) {
        x[["group"]] <- rep(list(NULL), nrow(x))
    }
    # Default geometries for each type of data to plot
    geometries <- c("observations" = "points",
                    "initial" = "points",
                    "prediction" = "envelope",
                    "trajectory" = "line")
    colors <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", 
                "#E5C494", "#B3B3B3")
    # Colors are taken from RColorBrewer::brewer.pal(8, "Set2")
    facet_variables <- c("group", "compartment", "type")
    nature <- type
    if (nature == "both") nature <- c("size", "prop")
    if (length(facet_row) == 0) facet_row <- NULL
    if (length(facet_column) == 0) facet_column <- NULL
    stopifnot(is.null(facet_row) | all(facet_row %in% facet_variables))
    stopifnot(is.null(facet_column) | all(facet_column %in% facet_variables))
    stopifnot(all(nature %in% c("prop", "size", "both")))
    # Prepare the data
    if (!"ready_for_unit_plot" %in% class(x)) {
        if (log) {
            transform <- log10
        } else {
            transform <- NULL
        }
        z <- split_to_unit_plot(x, transform)
    } else {
        z <- x
    }
    # Determine what to draw
    if (!missing(comps)) {
        z <- z[z[["compartment"]] %in% comps, ]
    }
    if (!missing(keep)) {
        z <- z[z[["compartment"]] %in% keep, ]
    }
    if (!missing(drop)) {
        z <- z[!z[["compartment"]] %in% drop, ]
    }
    if ("observations" %in% colnames(x) && !is.null(x$observations[[1]])) {
        draw_obs <- TRUE
    } else {
        draw_obs <- FALSE
    }
    if ("prediction" %in% colnames(x) && !is.null(x$prediction[[1]])) {
        draw_pred <- TRUE
    } else {
        draw_pred <- FALSE
    }
    # Process the data
    if (!draw_pred) {
        z <- dplyr::filter(z, type != "prediction")
    }
    if (!draw_obs) {
        z <- dplyr::filter(z, type != "observations")
    }
    if (nrow(z) == 0) {
        stop("Nothing to plot")
    }
    z <- z[z$nature %in% nature, ]
    z$geometry <- geometries[z$type]
    z$groupLabel <- purrr::map_chr(z$group, function(x) {
        paste0(x, collapse = ", ")
    })
    groupLabels <- sort(unique(z$groupLabel))
    compLabels <- sort(unique(z$compartment))
    if (!missing(comps)) {
        compLabels <- compLabels[order(match(compLabels, comps))]
    }
    colComps <- rep(colors, length(compLabels))
    colComps <- colComps[1:length(compLabels)]
    colComps <- setNames(colComps, nm = compLabels)
    if (!is.null(.colComps)) {
        .colComps <- sort(.colComps)
        .colComps <- .colComps[names(sort(colComps))]
        stopifnot(all(.colComps == sort(colComps)))
    }
    if (newpage) grid::grid.newpage()
    dev.hold()
    on.exit(dev.flush())
    if (legend) {
        labels <- c("compartment", names(x))
        longest <- labels[which.max(nchar(labels))]
        longest <- paste0(longest, "aaaaaaaa", collapse = " ")
        lo <- grid::grid.layout(ncol = 2,
                                widths = grid::unit(c(1, 1), c("null", "strwidth"), list(NULL, longest)))
        vp_top_with_legend <- grid::viewport(layout = lo)
        grid::pushViewport(vp_top_with_legend)
        vp_legend <- grid::viewport(layout.pos.col = 2)
        grid::pushViewport(vp_legend)
        draw_legend(colComps)
        grid::upViewport(1)
        vp_plot <- grid::viewport(layout.pos.col = 1)
        grid::pushViewport(vp_plot)
    }
    # Plot both size and proportion
    if (type == "both") {
        # Plot size and prop
        # Facetting for type
        facet_type <- "column"
        if ("type" %in% facet_row) {
            facet_row <- facet_row[facet_row != "type"]
            facet_type <- "row"
        }
        if ("type" %in% facet_column) {
            facet_column <- facet_column[facet_column != "type"]
            facet_type <- "column"
        }
        # Top vp
        if (facet_type == "column") {
            top_vp <- grid::viewport(layout = grid::grid.layout(ncol = 2))
        } else {
            top_vp <- grid::viewport(layout = grid::grid.layout(nrow = 2))
        }
        grid::pushViewport(top_vp)
        if (facet_type == "column") {
            size_vp <- grid::viewport(layout.pos.col = 1)
        } else {
            size_vp <- grid::viewport(layout.pos.row = 1)
        }
        grid::pushViewport(size_vp)
        plot_nm(x = x, facet_row = facet_row, facet_column = facet_column,
                scale = scale, type = "size", newpage = FALSE,
                xlab = xlab, ylab = ylab.size, margins = margins, grid = grid,
                .colComps = colComps, legend = FALSE, log = log,
                comps = comps, keep = keep, drop = drop)
        grid::upViewport(1)
        if (facet_type == "column") {
            prop_vp <- grid::viewport(layout.pos.col = 2)
        } else {
            prop_vp <- grid::viewport(layout.pos.row = 2)
        }
        grid::pushViewport(prop_vp)
        plot_nm(x = x, facet_row = facet_row, facet_column = facet_column,
                scale = scale, type = "prop", newpage = FALSE,
                xlab = xlab, ylab = ylab.prop, margins = margins, grid = grid,
                .colComps = colComps, legend = FALSE, log = log,
                comps = comps, keep = keep, drop = drop)
        grid::upViewport(1)
        grid::upViewport(1)
        if (legend) {
            grid::upViewport(2)
        }
        return(invisible(NULL))
    }
    stopifnot(scale %in% c("auto", "all", "row"))
    if (is.null(xlab)) xlab <- "Time"
    if (is.null(ylab)) {
        ylab <- ifelse(nature == "size", "Size", "Proportion")
    }
    # Facetting
    if (is.null(facet_row)) {
        z$row_id <- 1
    } else {
        if (facet_row == "group") { z$row_id <- match(z$groupLabel, groupLabels) }
        if (facet_row == "compartment") { z$row_id <- match(z$compartment, compLabels) }
    }
    if (is.null(facet_column)) {
        z$col_id <- 1
    } else {
        if (facet_column == "group") { z$col_id <- match(z$groupLabel, groupLabels) }
        if (facet_column == "compartment") { z$col_id <- match(z$compartment, compLabels) }
    }
    # Functions to get the scales
    xscaling <- function(z) {
        x <- unlist(lapply(z$data, function(x) x[["time"]]))
        return(extendrange(range(x, na.rm = TRUE)))
    }
    yscaling <- function(z) {
        x <- unlist(lapply(z$data, function(x) {
            d <- colnames(x)[startsWith(colnames(x), "quantity")]
            unlist(x[, d])
        }))
        range_x <- range(x, na.rm = TRUE)
        if (range_x[1] == range_x[2]) {
            delta <- 0.01 * range_x[1]
            range_x <- range_x + c(-delta, delta)
        }
        return(extendrange(range_x))
    }
    xscale <- xscaling(z)
    yscale_all <- yscaling(z)
    # Plot layout
    grid::pushViewport(grid::viewport(x = grid::unit(margins[2], "lines"),
                                      y = grid::unit(margins[1], "lines"),
                                      width = grid::unit(1, "npc") - grid::unit(sum(margins[c(2, 4)]), "lines"),
                                      height = grid::unit(1, "npc") - grid::unit(sum(margins[c(1, 3)]), "lines"),
                                      just =c("left", "bottom")))
    # Top container
    vp_top <- grid::viewport(layout = grid::grid.layout(nrow = 2, ncol = 2,
                                                           width = grid::unit(c(1.3, 1), c("lines", "null")),
                                                           height = grid::unit(c(1, 1.3), c("null", "lines"))))
    grid::pushViewport(vp_top)
    vp_xlab <- grid::viewport(layout.pos.row = 2, layout.pos.col = 2)
    grid::pushViewport(vp_xlab)
    grid::grid.text(xlab, gp = grid::gpar(cex = 1))
    grid::upViewport(1)
    vp_ylab <- grid::viewport(layout.pos.row = 1, layout.pos.col = 1)
    grid::pushViewport(vp_ylab)
    grid::grid.text(ylab, gp = grid::gpar(cex = 1), rot = 90)
    grid::upViewport(1)
    ## Widths
    if (scale %in% c("all", "row")) {
        # One y-axis per row
        widths <- rep((grid::unit(1, "npc") - grid::unit(3, "lines"))*(1/max(z$col_id)), max(z$col_id))
        widths[1] <- widths[1] + grid::unit(3, "lines")
    } else {
        # One y-axis per panel
        widths <- rep(grid::unit(1, "npc")*(1/max(z$col_id)), max(z$col_id))
    }
    ## Heights
    heights <- rep((grid::unit(1, "npc") - grid::unit(3, "lines"))*(1/max(z$row_id)), max(z$row_id))
    heights[length(heights)] <- heights[length(heights)] + grid::unit(3, "lines")
    layout <- grid::grid.layout(nrow = max(z$row_id), ncol = max(z$col_id),
                                width = widths, height = heights)
    vp_master <- grid::viewport(layout.pos.row = 1, layout.pos.col = 2,
                                layout = layout)
    grid::pushViewport(vp_master)
    # Draw each panel
    for (i in 1:max(z$row_id)) {
        z_row <- z[z$row_id == i, ]
        yscale_row <- yscaling(z_row)
        for (j in 1:max(z$col_id)) {
            z_ij <- z[z$row_id == i & z$col_id == j, ]
            margins <- rep(0, 4)
            if (i == max(z$row_id)) margins[1] <- 3
            if (j == 1 | scale == "auto") margins[2] <- 3
            layout <- grid::grid.layout(nrow = 3, ncol = 2,
                                        widths = grid::unit(c(margins[2], 1),
                                                       units = c("lines", "null")),
                                        heights = grid::unit(c(1.5, 1, margins[1]),
                                                       units = c("lines", "null", "lines")))
            vp_panel <- grid::viewport(layout.pos.row = i,
                                       layout.pos.col = j,
                                       layout = layout)
            grid::pushViewport(vp_panel)
            # Title
            vp_title <- grid::viewport(layout.pos.row = 1,
                                       layout.pos.col = 2)
            grid::pushViewport(vp_title)
            grid::grid.rect(gp = grid::gpar(fill = grey(0.95)))
            label <- sapply(seq_len(nrow(z_ij)), function(i) {
                if (z_ij$groupLabel[i] != "") {
                    paste(z_ij$groupLabel[i])
                } else {
                    z_ij$compartment[i]
                }
            })
            label <- paste(unique(label), collapse = " + ")
            grid::grid.text(label)
            grid::upViewport(1)
            # Plot
            vp_plot <- grid::viewport(layout.pos.row = 2,
                                       layout.pos.col = 2)
            grid::pushViewport(vp_plot)
            yscale <- yscaling(z_ij)
            if (scale == "all") yscale <- yscale_all
            if (scale == "row") yscale <- yscale_row
            draw_nm_panel(z_ij, colors = colComps[z_ij$compartment],
                          xscale = xscale, yscale = yscale,
                          x.axis = i == max(z$row_id),
                          y.axis = j == 1 | scale == "auto",
                          grid = grid, log = log)
            grid::grid.rect(gp = grid::gpar(fill = NA))
            grid::upViewport(1)
            # Out vp_panel
            grid::upViewport(1)
        }
    }
    # Up viewports
    grid::upViewport(1)
    grid::upViewport(1)
    grid::upViewport(1)
    if (legend) {
        grid::upViewport(2)
    }
    # Return
    return(invisible(NULL))
}

### * split_to_unit_plot()

#' Separate network model object into plot chunks
#' 
#' Function to split enriched network model object (i.e. network model that can
#' have a trajectory or a prediction columns) into unit plot data.
#'
#' @param x A network model object, with obs/traj/prediction data.
#' @param transform Optional, function to apply to all "quantities" columns
#'     (e.g. log).
#'
#' @return A tibble which can be e.g. filtered and used for plotting.
#'
#' @keywords internal
#' @noRd

split_to_unit_plot <- function(x, transform = NULL) {
    `!!` <- rlang::`!!`
    # Convert trajectory column to a light, tidy version
    if ("trajectory" %in% colnames(x)) {
        x$trajectory <- lapply(x$trajectory, function(z) {
            stopifnot(nrow(z) == 1)
            sizes <- tibble::as_tibble(as.data.frame(z$sizes))
            sizes$time <- z$timepoints[[1]]
            sizes <- tidyr::pivot_longer(sizes, cols = -"time",
                                         names_to = "compartment", values_to = "size")
            props <- tibble::as_tibble(as.data.frame(z$proportions))
            props$time <- z$timepoints[[1]]
            props <- tidyr::pivot_longer(props, cols = -"time",
                                         names_to = "compartment", values_to = "prop")
            out <- dplyr::left_join(sizes, props, by = c("time", "compartment"))
            return(out)
        })
    }
    # For now implemented for a network model with prediction column
    if (!"prediction" %in% colnames(x)) {
        x$prediction <- rep(list(NULL), nrow(x))
    }
    target_cols <- c("observations", "prediction", "trajectory")
    target_cols <- target_cols[target_cols %in% colnames(x)]
    if (length(target_cols) < 1) {
        stop("x should have at least one of \"observations\", \"prediction\", or \"trajectory\" columns.")
    }
    y <- x[, c("initial", "group", target_cols)]
    y <- tidyr::pivot_longer(y, cols = tidyselect::all_of(target_cols),
                             names_to = "type",
                             values_to = "data")
    # Drop rows with NULL data
    y <- y[!sapply(y[["data"]], is.null), ]
    # Split by compartment
    z <- y
    z[["data"]] <- lapply(z[["data"]], function(x) {
        tidyr::nest(dplyr::group_by(x, `!!`(rlang::sym("compartment"))))
    })
    z$row_id <- 1:nrow(z)
    for (i in seq_len(nrow(z))) {
        z$data[[i]]$row_id <- i
        z$data[[i]]$data <- as.list(z$data[[i]]$data)
    }
    y <- dplyr::select(z, - "data")
    # Note: in the call, I add `multiple = "all"` because of a new warning
    # in dplyr 1.1.0 (and because in this case rows in `y` can match multiple
    # rows in `dplyr::bind_rows(z$data)`).
    y <- dplyr::full_join(y, dplyr::bind_rows(z$data), by = "row_id",
                          multiple = "all")
    y <- dplyr::select(y, - "row_id")
    # Separate size and proportion data
    y$size <- as.list(rep(NA, nrow(y)))
    y$prop <- as.list(rep(NA, nrow(y)))
    for (i in seq_len(nrow(y))) {
        d <- y$data[[i]]
        ds <- dplyr::select(d, tidyselect::starts_with("size"), "time")
        n <- names(ds)
        n <- gsub("size", "quantity", n)
        names(ds) <- n
        dp <- dplyr::select(d, tidyselect::starts_with("prop"), "time")
        n <- names(dp)
        n <- gsub("proportion", "quantity", n)
        n <- gsub("prop", "quantity", n)
        names(dp) <- n
        y$size[[i]] <- na.omit(ds)
        y$prop[[i]] <- na.omit(dp)
    }
    y <- dplyr::select(y, - "data")
    z <- tidyr::pivot_longer(y, cols = c("size", "prop"),
                             names_to = "nature",
                             values_to = "data")
    z <- dplyr::select(z, "group", "compartment", "nature", "type", "data", "initial")
    # Separate initial values
    init <- dplyr::select(z, "group", "compartment", "initial")
    init$size <- rep(list(NA), nrow(init))
    init$prop <- rep(list(NA), nrow(init))
    for (i in seq_len(nrow(init))) {
        comp_i <- init$compartment[i]
        init$initial[[i]] <- init$initial[[i]][init$initial[[i]][["compartment"]] == comp_i, ]
        stopifnot(nrow(init$initial[[i]]) == 1)
        init$size[[i]] <- tibble::tibble(quantity = init$initial[[i]]$size[1],
                                         time = 0)
        init$prop[[i]] <- tibble::tibble(quantity = init$initial[[i]]$proportion[1],
                                         time = 0)
    }
    init$initial <- NULL
    init$type <- "initial"
    init <- tidyr::pivot_longer(init, cols = c("size", "prop"),
                             names_to = "nature",
                             values_to = "data")
    init <- unique(init)
    # Merge
    z$initial <- NULL
    z <- dplyr::bind_rows(z, init)
    # Apply transform
    if (!is.null(transform)) {
        for (i in seq_len(nrow(z))) {
            d <- z$data[[i]]
            cols <- colnames(d)[grepl("quantity", colnames(d))]
            for (col in cols) {
                d[[col]] <- transform(d[[col]])
            }
            z$data[[i]] <- d
        }
    }
    # Return
    return(structure(z, class = c("ready_for_unit_plot", class(z))))
}

### * draw_nm_panel()

#' Draw a panel of the data (obs/traj/pred) from a network model
#'
#' @param x Output from \code{split_to_unit_plot}.
#' @param colors Passed to \code{draw_nm_layer}.
#' @param xscale,yscale X and y scales.
#' @param x.axis,y.axis Booleans, draw the axes?
#' @param grid Boolean, draw a grid?
#' @param log Boolean, log-transform the y axis?
#' 
#' @keywords internal
#' @noRd

draw_nm_panel <- function(x, colors = "grey", xscale = NULL, yscale = NULL,
                          x.axis = FALSE, y.axis = FALSE, grid = FALSE,
                          log = FALSE) {
    # Get lims
    x_range <- range(unlist(lapply(x$data, function(i) i[["time"]])),
                     na.rm = TRUE)
    y_range <- range(unlist(lapply(x$data, function(i) {
        unlist(i[, grepl("quantity", colnames(i))])
    })), na.rm = TRUE)
    # Create viewport
    if (is.null(xscale)) xscale <- x_range
    if (is.null(yscale)) yscale <- y_range
    vp <- grid::dataViewport(xscale = xscale,
                             yscale = yscale)
    grid::pushViewport(vp)
    # Draw grid
    xat <- pretty(xscale)
    xat <- xat[xat >= xscale[1] & xat <= xscale[2]]
    yat <- pretty(yscale)
    if (log) {
        yat <- log10(prettyLog(yscale))
    }
    yat <- yat[yat >= yscale[1] & yat <= yscale[2]]
    ylabel <- yat
    if (log) {
        ylabel <- prettyLogLabels(yscale)
        ylabel <- ylabel[log10(prettyLog(yscale)) >= yscale[1] & log10(prettyLog(yscale)) <= yscale[2]]
    }
    if (grid) {
        grid.col <- grey(0.95)
        for (xi in xat) {
            grid::grid.lines(x = grid::unit(c(xi, xi), "native"),
                             y = grid::unit(yscale, "native"),
                             gp =grid::gpar(col = grid.col))
        }
        for (yi in yat) {
            grid::grid.lines(x = grid::unit(xscale, "native"),
                             y = grid::unit(c(yi, yi), "native"),
                             gp =grid::gpar(col = grid.col))
        }
    }
    # Draw layers
    colors <- rep(colors, nrow(x))
    for (i in seq_len(nrow(x))) {
        draw_nm_layer(x[i, ], color = colors[i])
    }
    if (x.axis) grid::grid.xaxis()
    if (y.axis) grid::grid.yaxis(at = yat,
                                 label = ylabel)
    # Up viewports
    grid::upViewport(1)
}

### * draw_nm_layer()

#' @importFrom grDevices adjustcolor
#' 
#' @keywords internal
#' @noRd

draw_nm_layer <- function(x, color = "grey") {
    stopifnot(nrow(x) == 1)
    geometry <- x$geometry[[1]]
    stopifnot(geometry %in% c("points", "envelope", "line"))
    if (geometry == "points") {
        if (nrow(x$data[[1]]) > 0) {
            grid::grid.points(x = x$data[[1]]$time,
                              y = x$data[[1]]$quantity,
                              default.unit = "native",
                              pch = 21,
                              gp = grid::gpar(col = "black",
                                              fill = color))
        }
    }
    if (geometry == "envelope") {
        if (nrow(x$data[[1]]) > 0) {
            grid::grid.polygon(x = c(x$data[[1]]$time, rev(x$data[[1]]$time)),
                               y = c(x$data[[1]]$quantity_low, rev(x$data[[1]]$quantity_high)),
                               default.unit = "native",
                               gp = grid::gpar(col = color,
                                               fill = adjustcolor(color, alpha.f = 0.25)))
        }
    }
    if (geometry == "line") {
        if (nrow(x$data[[1]]) > 0) {
            grid::grid.lines(x = x$data[[1]]$time,
                             y = x$data[[1]]$quantity,
                             default.unit = "native",
                             gp = grid::gpar(col = color))
        }
    }
}

### * draw_legend()

#' Draw a legend for plot_nm()
#'
#' @importFrom grDevices adjustcolor
#'
#' @param x A named vector of colors
#' 
#' @keywords internal
#' @noRd

draw_legend <- function(x) {
    lo <- grid::grid.layout(nrow = 2,
                            heights = grid::unit(c(2, 1.5 * length(x)),
                                                 c("lines", "lines")))
    vp_legend <- grid::viewport(layout = lo)
    grid::pushViewport(vp_legend)
    # Title
    vp_title <- grid::viewport(layout.pos.row = 1)
    grid::pushViewport(vp_title)
    grid::grid.text("Compartments", x = grid::unit(2, "strwidth", list("a")),
                    just = "left",
                    gp = grid::gpar(fontface = 2))
    grid::upViewport(1)
    # Label stack
    lo <- grid::grid.layout(nrow = length(x),
                            heights = grid::unit(rep(1.5, length(x)),
                                                 rep("lines", length(x))))
    vp_label_stack_top <- grid::viewport(layout.pos.row = 2)
    grid::pushViewport(vp_label_stack_top)
    vp_label_stack <- grid::viewport(x = grid::unit(3, "strwidth", list("a")),
                                     just = "left",
                                     layout = lo)
    grid::pushViewport(vp_label_stack)
    # Individual labels
    for (i in seq_along(x)) {
        # Colored square + label
        lo <- grid::grid.layout(ncol = 2,
                                widths = grid::unit(c(0.9, 1), c("lines", "null")))
        vp_compartment <- grid::viewport(layout.pos.row = i,
                                         layout = lo)
        grid::pushViewport(vp_compartment)
        vp_square_top <- grid::viewport(layout.pos.col = 1)
        grid::pushViewport(vp_square_top)
        vp_square <- grid::viewport(width = grid::unit(1.1, "lines"),
                                    height = grid::unit(1.1, "lines"))
        grid::pushViewport(vp_square)
        grid::grid.rect(gp = grid::gpar(col = x[i],
                                        fill = adjustcolor(x[i], alpha.f = 0.5)))
        grid::upViewport(2)
        vp_label <- grid::viewport(layout.pos.col = 2)
        grid::pushViewport(vp_label)
        grid::grid.text(names(x)[i], x = grid::unit(2, "strwidth", list("a")),
                        just = "left")
        grid::upViewport(2)
    }
    grid::upViewport(3)
}

### * prettyLog

#' Find pretty ticks for log scale
#'
#' @param range Range of the axis, in log10-transformed scale
#'
#' @return A vector containing the tick values in natural scale
#' 
#' @keywords internal
#' @noRd

prettyLog <- function(range) {
    x <- range
    min_x <- 10^min(x)
    max_x <- 10^max(x)
    z <- list(c(1), c(1, 5), c(1, 2, 5))
    o <- lapply(seq_along(z), function(j) {
        k <- c()
        for (i in -10:10) {
            k <- c(k, z[[j]] * 10^i)
        }
        k <- k[k >= min_x & k <= max_x]
        return(k)
    })
    if (length(o[[1]]) >= 3) {
        return(o[[1]])
    } else if (length(o[[2]]) >= 3) {
        return(o[[2]])
    } else {
        return(o[[3]])
    }
}

### * prettyLogLabels

#' Same as prettyLog but return expression for labels
#'
#' TODO For now the function just returns its input.
#' 
#' @param range Range of the axis, in log10-transformed scale
#'
#' @return A vector containing the tick values in natural scale
#' 
#' @keywords internal
#' @noRd

prettyLogLabels <- function(range) {
    ticks <- prettyLog(range)
    labels <- ticks
    return(labels)
}

### * plot_traces()

#' Plot nice traces from a mcmc list
#'
#' @param x A \code{coda::mcmc.list} object
#' @param variables Optional, a vector specifying the ordered variables to plot
#' @param regexp If TRUE (the default), strings in \code{variables} are used to
#'     select traces to draw by pattern matching with the variables names.
#' @param loglik Boolean, if \code{TRUE} additionally plot the loglik trace.
#' @param ratio Aspect ratio for the overall plot that the function thrives to
#'     respect (this will have an effect on the balance between numbers of rows
#'     and colums)
#' @param transform Function to use to transform the values before plotting
#'     (e.g. log). The function is applied directly to \code{x}, so it should
#'     be able to handle \code{mcmc.list} object (and return a similar object).
#' @param lty Line type for the density plots
#' @param hist Boolean, if TRUE draw histograms instead of density profiles
#' @param nbins Number of histogram bins, used only if \code{drawHist} is TRUE.
#' @param pdf Filename to save the plot as a pdf. If \code{NULL}, the plot is
#'     displayed.
#' @param png Filename to save the plot as a png. If \code{NULL}, the plot is
#'     displayed.
#' @param width,height Width and height of the pdf file in inches. Also used
#'     for the size of the png file, using a RES value of 150.
#'
#' @importFrom grDevices dev.hold
#' @importFrom grDevices dev.flush
#' @importFrom grDevices dev.off
#'
#' @keywords internal
#' @noRd

plot_traces <- function(x, variables = NULL, regexp = TRUE, loglik = FALSE,
                       ratio = 4/3, transform = NULL, lty = 1,
                       hist = TRUE, nbins = 32,
                       pdf = NULL, png = NULL, width = 12, height = 10) {
    x_original <- x
    if (is.null(transform)) {
        x <- x
    } else {
        x <- transform(x)
    }
    drawHist <- hist
    nBars <- nbins
    # Parse pdf/png arguments
    stopifnot(is.null(pdf) | is.null(png))
    if (!is.null(pdf)) {
        pdf(file = pdf, width = width, height = height)
    }
    if (!is.null(png)) {
        RES = 150
        png(file = png, width = width * RES, height = height * RES, res = RES,
            type = "cairo-png")
    }
    dev.hold()
    on.exit(dev.flush())
    # Prepare data
    if (loglik) {
        prev_varnames <- coda::varnames(x)
        # Add loglik trace to each mcmc object
        mcmcList <- lapply(seq_along(x), function(i) {
            y <- cbind(x[[i]], attr(x, "loglik")[[i]])
            colnames(y) <- c(prev_varnames, "loglik")
            chainI <- coda::mcmc(y)
            return(chainI)
        })
        x <- coda::as.mcmc.list(mcmcList)
        for (i in seq_along(x)) {
            attr(x[[i]], "mcpar") <- coda::mcpar(x_original)
        }
    }
    if (regexp & !is.null(variables)) {
        vars <- coda::varnames(x)
        selectedVars <- vars[unlist(sapply(variables, grep, x = vars))]
        selectedVars <- unique(selectedVars)
        if (length(selectedVars) == 0) {
            stop("No variables found matching: ", variables)
        }
        variables <- selectedVars
    }
    drawTraces(mcmc.list = x, variables = variables,
               ratio = ratio, lty = lty,
               drawHist = drawHist, nBars = nBars)
    if (!is.null(pdf) | !is.null(png)) {
        invisible(dev.off())
    }
    return(invisible(x_original))
}

### * drawTraces

#' Draw the traces for a mcmc.list object
#'
#' @param mcmc.list A coda mcmc.list object
#' @param drawXAxis Boolean
#' @param variables A vector containing the names of the variables to include
#'     in the plot. If NULL, all variables are used.
#' @param ratio Aspect ratio for the overall plot that the function thrives to
#'     respect (this will have an effect on the balance between numbers of rows
#'     and colums)
#' @param drawHist Boolean, if TRUE draw histograms instead of density profiles
#' @param nBars Number of histogram bars, used only if \code{drawHist} is TRUE.
#' @param newpage Boolean, run grid.newpage() before drawing?
#' @param ... Arguments passed to \code{\link{drawTraceOneVar}}
#'
#' @keywords internal
#' @noRd

drawTraces = function(mcmc.list, drawXAxis = FALSE, variables = NULL,
                      ratio = 4/3, drawHist = FALSE, nBars = 64,
                      newpage = TRUE, ...) {
    nchains = coda::nchain(mcmc.list)
    nvars = coda::nvar(mcmc.list)
    varsToPlot = 1:nvars
    if (!is.null(variables)) {
        nvars = length(variables)
        varsToPlot = variables
    }
    colors = c("deeppink", "green3", "darkmagenta", "dodgerblue")
    colors = rep(colors, nchains)
    mfrow = n2mfrowByRatio(nvars, ratio = ratio)
    # New page
    if (newpage) grid::grid.newpage()
    # Viewport for traces
    if (drawXAxis) {
        vpTraces = grid::viewport(layout = grid::grid.layout(nrow = mfrow[1] + 1, ncol = mfrow[2],
                                                             heights = grid::unit(c(rep(1, mfrow[1]), 4), c(rep("null", mfrow[1]), "lines"))),
                                  name = "vpTraces", clip = "on")
    } else {
        vpTraces = grid::viewport(layout = grid::grid.layout(nrow = mfrow[1], ncol = mfrow[2]),
                                  name = "vpTraces", clip = "on")
    }
    grid::pushViewport(vpTraces)
    # Draw panel for each parameter
    lapply(seq_len(nvars), function(v) {
        row = 1 + (v-1) %/% mfrow[2]
        column = 1 + (v-1) %% mfrow[2]
        vpCell = grid::viewport(layout.pos.row = row,
                                layout.pos.col = column,
                                name = paste("vpCell", row, column, sep = "."))
        grid::pushViewport(vpCell)
        grid::grid.rect(gp = grid::gpar(col = grey(0.5)))
        if (drawHist) {
            drawTraceOneVarHist(mcmc.list = mcmc.list, varname = varsToPlot[v], colors = colors,
                            drawXAxis = ifelse(drawXAxis,
                            (row == mfrow[1] | (row == (mfrow[1] - 1) & column == mfrow[2])),
                            FALSE), nBars = nBars, ...)
        } else {
            drawTraceOneVar(mcmc.list = mcmc.list, varname = varsToPlot[v], colors = colors,
                            drawXAxis = ifelse(drawXAxis,
                            (row == mfrow[1] | (row == (mfrow[1] - 1) & column == mfrow[2])),
                            FALSE), ...)
        }
        grid::upViewport(1)
    })
    # Back to parent viewport
    grid::upViewport(1)
}

### * drawTraceOneVar

#' Draw the chain traces for one variable, using a density profile for the density panel
#'
#' This function is mostly useful when it is called by other functions as a
#' buildign block to draw the traces of several variables.
#'
#' @param mcmc.list A coda mcmc.list object
#' @param varname Name of the variable of which the trace is to be drawn
#' @param colors Vector of colors for the chain traces
#' @param drawXAxis Boolean
#' @param lty Line type for the density plot
#'
#' @importFrom stats density
#' @importFrom grDevices grey
#' @importFrom grDevices extendrange
#' @importFrom grDevices adjustcolor
#'
#' @keywords internal
#' @noRd

drawTraceOneVar = function(mcmc.list, varname, colors, drawXAxis = FALSE, lty = 1) {
    EXTENSION = c(0.025, 0.1)
    ALPHA_FILL = 0.2
    ALPHA_TRACES = 0.6
    DENSITY_MAX_RELATIVE_SHIFT = 0 # Can be set to e.g. 0.25 for slight shift in density plots
    MODE_COL = "white"
    CI_LWD = c(0.25, 0.15) # Unit: char
    # Process the data
    varnameOriginal <- varname
    if (is.numeric(varname)) {
        varname = coda::varnames(mcmc.list)[varname]
        if (is.null(varname)) {
            varname <- "unnamed_variable"
        }
    }
    mcpars = coda::mcpar(mcmc.list[[1]])
    nchains = coda::nchain(mcmc.list)
    if (!is.null(dim(mcmc.list[[1]]))) {
        varData = lapply(mcmc.list, function(mcmc) mcmc[, varnameOriginal])
    } else {
        # There is only one variable
        varData = lapply(mcmc.list, function(mcmc) as.vector(mcmc))
    }
    xData = seq(from = mcpars[1], to = mcpars[2], by = mcpars[3])
    yData = unlist(varData)
    densities = lapply(varData, density)
    densitiesData = unlist(lapply(densities, "[[", "y"))
    densitiesRange = range(densitiesData)
    if (nchains > 1) {
        stepDensity = diff(densitiesRange) * DENSITY_MAX_RELATIVE_SHIFT / (nchains - 1)
    } else {
        stepDensity = 0
    }
    densitiesRange[2] = densitiesRange[2] + stepDensity * (nchains - 1) + diff(densitiesRange) * 0.1
    # Viewport for the parameter panel
    vpParamPanel = grid::viewport(layout = grid::grid.layout(nrow = 2, ncol = 1,
                                                             heights = grid::unit(c(1.5, 1), c("lines", "null"))),
                                  name = paste("vpParamPanel", varname, sep = "."))
    grid::pushViewport(vpParamPanel)
    # Viewport for the title
    vpTitle = grid::viewport(name = "vpTitle", layout.pos.row = 1)
    grid::pushViewport(vpTitle)
    grid::grid.rect(gp = grid::gpar(fill = grey(0.95), col = grey(0.5)))
    grid::grid.text(varnameToExp(varname),
                    gp = grid::gpar(col = grey(0.5), cex = 1.2))
    grid::upViewport(1)
    # Viewport for the graph
    vpGraph = grid::viewport(layout = grid::grid.layout(nrow = 1, ncol = 3,
                                                        widths = grid::unit(c(3, 1, 0.25), c("lines", "null", "null"))),
                             name = "vpGraph", layout.pos.row = 2, clip = ifelse(drawXAxis, "off", "on"))
    grid::pushViewport(vpGraph)
    # Viewport for the y axis
    vpYAxis = grid::viewport(name = "vpYAxis", layout.pos.col = 1)
    grid::pushViewport(vpYAxis)
    grid::upViewport(1)
    # Viewport for the trace plot
    vpTracePlot = grid::dataViewport(xData = xData, yData = yData, extension = EXTENSION,
                                     name = "vpTracePlot", layout.pos.col = 2)
    grid::pushViewport(vpTracePlot)
    ## Draw the chains
    lapply(seq_len(nchains), function(i) {
        grid::grid.lines(x = xData, y = varData[[i]], default.units = "native",
                         gp = grid::gpar(col = adjustcolor(colors[i], alpha.f = ALPHA_TRACES),
                                         lwd = 1))
    })
    ## Draw axes
    grid::grid.yaxis(gp = grid::gpar(cex = 0.7))
    if (drawXAxis) {
        # https://stackoverflow.com/questions/8816456/rotate-labels-in-grid-xaxis
        grid::grid.xaxis(edits = grid::gEdit(gPath="labels", rot=90))
    }
    grid::upViewport(1)
    # Viewport for the density plot
    vpDensityPlot = grid::dataViewport(xscale = extendrange(densitiesRange, f = 0.025),
                                       yData = yData, extension = EXTENSION,
                                       name = "vpDensityPlot", layout.pos.col = 3,
                                       clip = "on")
    grid::pushViewport(vpDensityPlot)
    lapply(seq_len(nchains), function(i) {
        grid::grid.polygon(x = c(densities[[i]]$y, densities[[i]]$y[1]) + stepDensity * (i - 1),
                           y = c(densities[[i]]$x, densities[[i]]$x[1]),
                           default.unit = "native",
                           gp = grid::gpar(col = colors[i],
                                           fill = adjustcolor(colors[i], alpha.f = ALPHA_FILL),
                                           lty = lty))
    })
    grid::upViewport(1)
    # Back to vpParamPanel
    grid::upViewport(1)
    # Back to original (parent) viewport
    grid::upViewport(1)
}

### * drawTraceOneVarHist

#' Draw the chain traces for one variable, using a histogram for density panel
#'
#' This function is mostly useful when it is called by other functions as a
#' buildign block to draw the traces of several variables.
#'
#' @param mcmc.list A coda mcmc.list object
#' @param varname Name of the variable of which the trace is to be drawn
#' @param colors Vector of colors for the chain traces
#' @param drawXAxis Boolean
#' @param lty Line type for the density plot
#' @param nBars Number of histogram bars per trace
#'
#' @importFrom stats density
#' @importFrom graphics hist
#' @importFrom grDevices grey
#' @importFrom grDevices extendrange
#' @importFrom grDevices adjustcolor
#'
#' @keywords internal
#' @noRd

drawTraceOneVarHist = function(mcmc.list, varname, colors, drawXAxis = FALSE, lty = 1, nBars = 128) {
    EXTENSION = c(0.025, 0.1)
    ALPHA_FILL = 0.2
    ALPHA_TRACES = 0.6
    DENSITY_MAX_RELATIVE_SHIFT = 0 # Can be set to e.g. 0.25 for slight shift in density plots
    MODE_COL = "white"
    CI_LWD = c(0.25, 0.15) # Unit: char
    # Process the data
    varnameOriginal <- varname
    if (is.numeric(varname)) {
        varname = coda::varnames(mcmc.list)[varname]
        if (is.null(varname)) {
            varname <- "unnamed_variable"
        }
    }
    mcpars = coda::mcpar(mcmc.list[[1]])
    nchains = coda::nchain(mcmc.list)
    if (!is.null(dim(mcmc.list[[1]]))) {
        varData = lapply(mcmc.list, function(mcmc) mcmc[, varnameOriginal])
    } else {
        # There is only one variable
        varData = lapply(mcmc.list, function(mcmc) as.vector(mcmc))
    }
    xData = seq(from = mcpars[1], to = mcpars[2], by = mcpars[3])
    yData = unlist(varData)
    densities = lapply(varData, density)
    densitiesData = unlist(lapply(densities, "[[", "y"))
    densitiesRange = range(densitiesData)
    histRange <- range(unlist(varData))
    histBreaks <- seq(histRange[1], histRange[2], length.out = nBars + 1)
    histograms <- lapply(seq_along(varData), function(i) {
        d <- varData[[i]]
        hist(d, breaks = histBreaks, plot = FALSE)$density
    })
    if (nchains > 1) {
        stepDensity = diff(densitiesRange) * DENSITY_MAX_RELATIVE_SHIFT / (nchains - 1)
    } else {
        stepDensity = 0
    }
    densitiesRange[2] = densitiesRange[2] + stepDensity * (nchains - 1) + diff(densitiesRange) * 0.1
    # Viewport for the parameter panel
    vpParamPanel = grid::viewport(layout = grid::grid.layout(nrow = 2, ncol = 1,
                                                             heights = grid::unit(c(1.5, 1), c("lines", "null"))),
                                  name = paste("vpParamPanel", varname, sep = "."))
    grid::pushViewport(vpParamPanel)
    # Viewport for the title
    vpTitle = grid::viewport(name = "vpTitle", layout.pos.row = 1)
    grid::pushViewport(vpTitle)
    grid::grid.rect(gp = grid::gpar(fill = grey(0.95), col = grey(0.5)))
    grid::grid.text(varnameToExp(varname),
                    gp = grid::gpar(col = grey(0.5), cex = 1.2))
    grid::upViewport(1)
    # Viewport for the graph
    vpGraph = grid::viewport(layout = grid::grid.layout(nrow = 1, ncol = 3,
                                                        widths = grid::unit(c(3, 1, 0.25), c("lines", "null", "null"))),
                             name = "vpGraph", layout.pos.row = 2, clip = ifelse(drawXAxis, "off", "on"))
    grid::pushViewport(vpGraph)
    # Viewport for the y axis
    vpYAxis = grid::viewport(name = "vpYAxis", layout.pos.col = 1)
    grid::pushViewport(vpYAxis)
    grid::upViewport(1)
    # Viewport for the trace plot
    if (length(unique(yData)) == 1) {
        if (yData[1] == 0) {
            adjYdata <- c(-0.1, 0.1)
        } else {
            adjYdata <- c(0.9 * unique(yData), yData, 1.1 * unique(yData))
        }
    } else {
        adjYdata <- yData
    }
    vpTracePlot = grid::dataViewport(xData = xData, yData = adjYdata, extension = EXTENSION,
                                     name = "vpTracePlot", layout.pos.col = 2)
    grid::pushViewport(vpTracePlot)
    ## Draw the chains
    lapply(seq_len(nchains), function(i) {
        grid::grid.lines(x = xData, y = varData[[i]], default.units = "native",
                         gp = grid::gpar(col = adjustcolor(colors[i], alpha.f = ALPHA_TRACES),
                                         lwd = 1))
    })
    ## Draw axes
    grid::grid.yaxis(gp = grid::gpar(cex = 0.7))
    if (drawXAxis) {
        # https://stackoverflow.com/questions/8816456/rotate-labels-in-grid-xaxis
        grid::grid.xaxis(edits = grid::gEdit(gPath="labels", rot=90))
    }
    grid::upViewport(1)
    # Viewport for the density plot
    if (!any(is.na(unlist(histograms)))) {
        if (length(unique(yData)) == 1) {
            adjYdata <- c(0.9 * unique(yData), yData, 1.1 * unique(yData))
        } else {
            adjYdata <- yData
        }
        vpDensityPlot = grid::dataViewport(xscale = extendrange(c(0, max(unlist(histograms))), f = 0.025),
                                           yData = adjYdata, extension = EXTENSION,
                                           name = "vpDensityPlot", layout.pos.col = 3,
                                           clip = "on")
        grid::pushViewport(vpDensityPlot)
        lapply(seq_len(nchains), function(i) {
            grid::grid.polygon(x = c(0, rep(histograms[[i]], each = 2), 0), #c(densities[[i]]$y, densities[[i]]$y[1]) + stepDensity * (i - 1),
                               y = rep(histBreaks, each = 2), #c(densities[[i]]$x, densities[[i]]$x[1]),
                               default.unit = "native",
                               gp = grid::gpar(col = colors[i],
                                               fill = adjustcolor(colors[i], alpha.f = ALPHA_FILL),
                                               lty = lty))
        })
        grid::upViewport(1)
    }
    # Back to vpParamPanel
    grid::upViewport(1)
    # Back to original (parent) viewport
    grid::upViewport(1)
}

### * n2mfrowByRatio

#' Provide a good mfrow argument given a number of cells and a target ratio
#'
#' @param n Number of cells to fit in the plot
#' @param ratio Aspect ratio for the overall plot that the function thrives to
#'     respect (this will have an effect on the balance between numbers of rows
#'     and colums)
#'
#' @importFrom grDevices dev.size
#'
#' @keywords internal
#' @noRd

n2mfrowByRatio = function(n, ratio = 4/3) {
    # Build all the reasonable combinations of nRows x nColumns
    nOne = seq_len(n)
    nTheOther = ceiling(n / nOne)
    nTheOtherKept = unique(nTheOther)
    nOneKept = sapply(nTheOtherKept, function(k) {
        min(nOne[nTheOther == k])
    })
    # Note: one can do nRows = c(nOneKept, nTheOtherKept) and nColumns =
    # c(nTheOtherKept, nOneKept) if it is important to minimize the number of
    # empty cells
    nRows = c(nOne, nTheOther)
    nColumns = c(nTheOther, nOne)
    nRbyC = as.data.frame(unique(cbind(nRows, nColumns)))
    names(nRbyC) = c("nRows", "nColumns")
    # Get device size
    devSize = dev.size()
    # Calculate ratios
    nRbyC$rowHeight = devSize[2] / nRbyC$nRows
    nRbyC$colWidth = devSize[1] / nRbyC$nColumns
    nRbyC$ratio = nRbyC$colWidth / nRbyC$rowHeight
    nRbyC$dist = abs(log(nRbyC$ratio / ratio))
    i = which(nRbyC$dist == min(nRbyC$dist))
    # Return
    return(c(nRbyC$nRows[i], nRbyC$nColumns[i]))
}

### * varnameToExp

#' Convert a variable name to the appropriate mathematical expression
#'
#' @param varname Variable name to be converted
#'
#' @keywords internal
#' @noRd

varnameToExp = function(varname) {
    rm_underscore <- function(x) {
        gsub("_", ".", x)
    }
    # Try to split with "|" to find replication variables
    elements = strsplit(varname, split = "[|]")[[1]]
    varname = elements[1]
    replVar = ""
    if (length(elements) > 1) {
        replVar = paste(elements[2:length(elements)], collapse = ",")
        replVar = paste(" |", rm_underscore(replVar))
    }
    # Uptake rate
    if (grepl("^upsilon_", varname)) {
        tmp = strsplit(varname, "^upsilon_")[[1]][2]
        tmp = strsplit(tmp, "_to_")[[1]]
        from = rm_underscore(tmp[1])
        to = rm_underscore(tmp[2])
        return(latex2exp::TeX(paste0("$\\upsilon_{", from, " \\rightarrow ", to, "}$", replVar)))
    }
    # Loss rate
    if (grepl("^lambda_", varname)) {
        comp = rm_underscore(strsplit(varname, "^lambda_")[[1]][2])
        return(latex2exp::TeX(paste0("$\\lambda_{", comp, "}$", replVar)))
    }
    # Active fraction
    if (grepl("^portion\\.act_", varname)) {
        comp = rm_underscore(strsplit(varname, "^portion\\.act_")[[1]][2])
        if (nchar(comp) > 4) {
            comp = substr(comp, start = 1, stop = nchar(comp) - 4)
        }
        return(latex2exp::TeX(paste0("$\\pi_{", comp, "}$", replVar)))
    }
    # propCv
    if (varname == "eta") {
        if (replVar == "") {
            return(latex2exp::TeX(paste0("$\\eta$")))
        }
        return(latex2exp::TeX(paste0("$\\eta_{", "}$", replVar)))
    }
    # sizeCv
    if (varname == "zeta") {
        if (replVar == "") {
            return(latex2exp::TeX(paste0("$\\zeta$")))
        }
        return(latex2exp::TeX(paste0("$\\zeta_{", "}$", replVar)))
    }
    if (grepl("^zeta_", varname)) {
        comp = rm_underscore(strsplit(varname, "^zeta_")[[1]][2])
        if (replVar == "") {
            return(latex2exp::TeX(paste0("$\\zeta_{", comp, "}$")))
        }
        return(latex2exp::TeX(paste0("$\\zeta_{", comp, "}$", replVar)))
    }
    # Return
    if (replVar == "") {
        return(varname)
    }
    return(paste0(varname, replVar))
}

### * capture_msg()

#' Capture a message generated by running an expression
#'
#' This function is inspired by the code from testthat::capture_message(). Note
#' that an expression such as `m <- new_networkModel()` will not have an effect
#' on the calling environment if a message is caught (i.e. no `m` object is
#' created in the calling environment of `capture_msg()`). In my understanding,
#' this is because the emission of a message interrupts the expression
#' evaluation.
#'
#' @param expr Expression to evaluate.
#'
#' @return NULL if no message was produced, the first message produced otherwise.
#'
#' @examples
#' isotracer:::capture_msg(new_networkModel())
#' 
#' @keywords internal
#' @noRd

capture_msg <- function(expr) {
    captured <- tryCatch({
        expr
        NULL
    }, message = function(msg) {
        return(msg)
    })
    return(captured)
}

### * flows_from_topo()

#' Build a flow tibble from a topology
#'
#' @param x A topology.
#'
#' @return A tibble with columns "from" and "to".
#'
#' @examples
#' flows_from_topo(topo(trini_mod))
#'
#' @keywords internal
#' @noRd

flows_from_topo <- function(x) {
    x <- unclass(x) # Remove the "topo" class to treat it as a matrix
    n_comps <- ncol(x)
    links <- which(x > 0)
    from <- links %/% n_comps + 1
    to <- links %% n_comps
    links <- tibble::tibble(from = from, to = to)
    for (i in seq_len(nrow(links))) {
        if (links$to[i] == 0) {
            links$from[i] <- links$from[i] - 1
            links$to[i] <- n_comps
        }
        stopifnot(x[links$to[i], links$from[i]] > 0)
    }
    flows <- tibble::tibble(from = colnames(x)[links$from],
                            to = rownames(x)[links$to])
    return(flows)
}

### * nodes_from_topo()

#' Build a node tibble from a topology
#'
#' @param x A topology.
#'
#' @return A tibble with columns "comp" and "label".
#'
#' @examples
#' nodes_from_topo(topo(trini_mod))
#'
#' @keywords internal
#' @noRd

nodes_from_topo <- function(x) {
    nodes <- tibble::tibble(comp = colnames(x),
                            label = colnames(x))
    return(nodes)
}

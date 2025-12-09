### * None of the functions in this file is exported

### * bezierCurve()

#' Calculate the coordinates of a Bezier curve from control points
#'
#' @param x,y Coordinates of the control points
#' @param n Integer, number of steps to use (larger is smoother)
#'
#' @return A two-column matrix with the coordinates of the Bezier curve
#'
#' @examples
#' bezierCurve <- isotracer:::bezierCurve
#' set.seed(9)
#' x = runif(6)
#' y = runif(6)
#' plot(x, y, type = "l", col = "grey")
#' lines(bezierCurve(x, y, 2), col = "cornflowerblue", lwd = 1)
#' lines(bezierCurve(x, y, 4), col = "cornflowerblue", lwd = 1)
#' lines(bezierCurve(x, y, 8), col = "purple", lwd = 1.5)
#' lines(bezierCurve(x, y, 64), col = "indianred", lwd = 4)
#'
#' @keywords internal
#' @noRd

bezierCurve = function(x, y, n) {
    stopifnot(length(x) == length(y))
    stopifnot(length(x) > 1)
    n = as.integer(n)
    stopifnot(n > 0)
    xOut = rep(NA, n + 2)
    yOut = rep(NA, n + 2)
    weights = seq(0, 1, length.out = n + 2)
    # Calculate coordinates
    for (i in seq_along(weights)) {
        xControl = x
        yControl = y
        nControl = length(xControl)
        weight = weights[i]
        while(nControl > 1) {
            xControl = (1 - weight) * xControl[1:(nControl - 1)] + weight * xControl[2:nControl]
            yControl = (1 - weight) * yControl[1:(nControl - 1)] + weight * yControl[2:nControl]
            nControl = length(xControl)
        }
        stopifnot(nControl == 1)
        xOut[i] = xControl
        yOut[i] = yControl
    }
    # Return
    return(as.matrix(cbind(xOut, yOut)))
}

### * ribbonFromTrajectory()

#' Calculate a ribbon polygon spanning a trajectory
#'
#' @param x Two-column object giving the coordinates defining the indicating
#'     trajectory.
#' @param width Numeric, width of the returned segment on the x scale.
#' @param constantWidth.y Boolean, if TRUE the ribbon coordinates are corrected
#'     for the aspect ratio of the plot so that the ribbon has a constant y
#'     width.
#'
#' @return A two-columnm matrix containing the coordinates of the ribbon, ready
#'     to be used with \code{\link{polygon}}
#'
#' @examples
#' bezierCurve <- isotracer:::bezierCurve
#' ribbonFromTrajectory <- isotracer:::ribbonFromTrajectory
#' 
#' set.seed(8)
#' x = runif(4)
#' y = runif(4)
#' b = bezierCurve(x, y, 64)
#' 
#' plot(x, y, type = "l", col = "grey", asp = 1, main = "Plot asp = 1")
#' lines(b, col = "indianred", lwd = 2)
#' bvp <- gridBase::baseViewports()
#' do.call(grid::pushViewport, bvp)
#' polygon(ribbonFromTrajectory(b, width = 0.05),
#'         col = adjustcolor("cornflowerblue", alpha.f = 0.5))
#' points(ribbonFromTrajectory(b, width = 0.05), pch = 19, cex = 0.5)
#' points(ribbonFromTrajectory(b, width = 0.10), pch = 19, cex = 0.5, col = "red")
#' 
#' plot(x, y, type = "l", col = "grey", asp = 3, main = "Plot asp = 3, constant y width")
#' lines(b, col = "indianred", lwd = 2)
#' polygon(ribbonFromTrajectory(b, width = 0.05),
#'         col = adjustcolor("cornflowerblue", alpha.f = 0.5))
#' 
#' plot(x, y, type = "l", col = "grey", asp = 3, main = "Plot asp = 3, no constant y width")
#' lines(b, col = "indianred", lwd = 2)
#' polygon(ribbonFromTrajectory(b, width = 0.05, constantWidth.y = FALSE),
#'         col = adjustcolor("cornflowerblue", alpha.f = 0.5))
#'
#' @keywords internal
#' @noRd

ribbonFromTrajectory = function(x, width, constantWidth.y = TRUE) {
    xIn = x
    x = xIn[,1]
    y = xIn[,2]
    n = length(x)
    # Get the plot aspect ratio (this will probably fail if no plot exists yet)
    if (constantWidth.y) {
        ## # https://stat.ethz.ch/pipermail/r-help/2005-October/080598.html
        ## wRes = par("pin")[1] / diff(par("usr")[c(1, 2)])
        ## hRes = par("pin")[2] / diff(par("usr")[c(3, 4)])
        ## aspectRatio = hRes / wRes
        aspectRatio <- grid_get_asp()
    } else {
        # Use default value of one
        aspectRatio = 1
    }
    # Convert x values to asp=1 space
    xCenter = mean(x)
    x = (x - xCenter) / aspectRatio
    # Calculate one way
    x0 = x
    y0 = y
    xNormal = diff(x0)
    yNormal = diff(y0)
    xyNorm = sqrt(xNormal^2 + yNormal^2)
    thetas = acos(xNormal / xyNorm)
    thetas[yNormal < 0] = - thetas[yNormal < 0]
    out = matrix(ncol = 4, nrow = n) # Columns: xleft, yleft, xright, yright
    for (i in 1:(n-1)) {
        initLeft = c(-width/2, 0)
        initRight = c(width/2, 0)
        # Rotation
        myAngle = thetas[i] + pi/2
        rotationMatrix = matrix(c(cos(myAngle), - sin(myAngle),
                                  sin(myAngle), cos(myAngle)),
                                ncol = 2, byrow = T)
        rotLeft = rotationMatrix %*% initLeft
        rotRight = rotationMatrix %*% initRight
        left = rotLeft + c(x0[i], y0[i])
        right = rotRight + c(x0[i], y0[i])
        out[i, ] = c(left, right)
    }
    # Add the last segment
    out[n, ] = c(rotLeft + c(x0[n], y0[n]), rotRight + c(x0[n], y0[n]))
    # Calculate the other way
    x0 = rev(x)
    y0 = rev(y)
    xNormal = diff(x0)
    yNormal = diff(y0)
    xyNorm = sqrt(xNormal^2 + yNormal^2)
    thetas = acos(xNormal / xyNorm)
    thetas[yNormal < 0] = - thetas[yNormal < 0]
    out2 = matrix(ncol = 4, nrow = n) # Columns: xleft, yleft, xright, yright
    for (i in 1:(n-1)) {
        initLeft = c(-width/2, 0)
        initRight = c(width/2, 0)
        # Rotation
        myAngle = thetas[i] + pi/2
        rotationMatrix = matrix(c(cos(myAngle), - sin(myAngle),
                                  sin(myAngle), cos(myAngle)),
                                ncol = 2, byrow = T)
        rotLeft = rotationMatrix %*% initLeft
        rotRight = rotationMatrix %*% initRight
        left = rotLeft + c(x0[i], y0[i])
        right = rotRight + c(x0[i], y0[i])
        out2[i, ] = c(left, right)
    }
    # Add the last segment
    out2[n, ] = c(rotLeft + c(x0[n], y0[n]), rotRight + c(x0[n], y0[n]))
    # Average both out matrices
    out2 = out2[nrow(out2):1, ]
    outTmp = out2
    out2[, 1:2] = out2[, 3:4]
    out2[, 3:4] = outTmp[, 1:2]
    for (i in 1:nrow(out)) {
        for (j in 1:ncol(out)) {
            out2[i, j] = (out[i, j] + out2[i, j]) / 2
        }
    }
    # Arrange format
    out = rbind(out2[, 1:2], out2[nrow(out2):1, 3:4])
    # Back-convert to plot aspect ratio space
    out[,1] = out[,1]*aspectRatio + xCenter
    # Return
    return(out)
}

### * grid_get_asp()

#' Get the aspect ratio of the current grid viewport
#'
#' @examples
#' grid_get_asp <- isotracer:::grid_get_asp
#' 
#' library(grid)
#' grid.newpage()
#' x <- mtcars$wt
#' y <- mtcars$drat
#' vp <- dataViewport(xData = x, yData = y)
#' pushViewport(vp)
#' grid.points(x, y)
#'
#' # Run this snippet several times, and resize the plot window in-between
#' # The newly drawn rectangle should always be exactly square.
#' width <- 1
#' asp <- grid_get_asp()
#' height <- width / asp
#' col <- sample(colors(), 1)
#' grid.rect(width = unit(width, "native"), height = unit(height, "native"))
#' 
#' @keywords internal
#' @noRd

grid_get_asp <- function() {
    # Get x and y ranges
    vp <- grid::current.viewport()
    xrange <- diff(vp$xscale)
    yrange <- diff(vp$yscale)
    span <- min(xrange, yrange) / 2.5
    xmid <- mean(vp$xscale)
    ymid <- mean(vp$yscale)
    loc_mid <- grid::deviceLoc(grid::unit(xmid, "native"),
                               grid::unit(ymid, "native"))
    loc_right <- grid::deviceLoc(grid::unit(xmid + span, "native"),
                               grid::unit(ymid, "native"))
    loc_top <- grid::deviceLoc(grid::unit(xmid, "native"),
                               grid::unit(ymid + span, "native"))
    height <- as.numeric(loc_top$y) - as.numeric(loc_mid$y)
    width <- as.numeric(loc_right$x) - as.numeric(loc_mid$x)
    return(height/width)
}

### * topo_has_loop()

#' Test if a topology has a loop
#'
#' @param x A topology matrix or a network model (the topology of the first row
#'     is used in this case).
#'
#' @return Boolean
#'
#' @examples
#' topo_has_loop <- isotracer:::topo_has_loop
#' 
#' topo_has_loop(aquarium_mod)
#' topo_has_loop(trini_mod)
#'
#' @keywords internal
#' @noRd

topo_has_loop <- function(x) {
    if (is(x, "networkModel")) {
        x <- unique(topo(x, simplify = FALSE))
        stopifnot(length(x) == 1)
        x <- x[[1]]
    }
    x <- unclass(x) # Remove the "topo" class to treat it as a matrix
    n_comps <- ncol(x)
    # Calculate powers of the transition matrix
    iters <- list()
    iters[[1]] <- x
    if (n_comps == 1) {
        stop("Only one compartment present in the network topology.")
    }
    for (i in 2:n_comps) {
        iters[[i]] <- iters[[i-1]] %*% x
    }
    # Look for loops
    loops <- lapply(iters, diag)
    any_loop <- sapply(loops, function(l) any(l!=0))
    # Return
    return(any(any_loop))
}

is_dag <- function(x) {
    !topo_has_loop(x)
}

### * not_implemented_for_topology_with_loop()

#' Check and throw an error if appropriate
#'
#' @param x A topology matrix or a network model (the topology of the first row
#'     is used in this case).
#'
#' @keywords internal
#' @noRd

not_implemented_for_topology_with_loop <- function(x) {
    if (topo_has_loop(x)) {
        stop("Function not implemented for topologies containing loop(s).")
    }
}

### * sankey_get_layout()

#' Process layout option for a given topology 
#' 
#' @param x A topology.
#' @param layout String or NULL, node-placing algorithm to use from the ggraph
#'     package (e.g. "stress" or "sugiyama"). If NULL, a default layout is
#'     returned, which depends on the topology. The ggraph package itself uses
#'     some algoritms from the igraph package. See the Details in the help of
#'     \code{\link[ggraph]{layout_tbl_graph_igraph}} for available
#'     algorithms. The ggraph package must be installed for this argument to be
#'     taken into account.
#'
#' @return A string
#' 
#' @keywords internal
#' @noRd

sankey_get_layout <- function(x, layout) {
    if (is.null(layout)) {
        if (is_dag(x)) {
            layout <- "left2right"
        } else {
            layout <- "stress"
        }
    }
    return(layout)
}


### * sankey_calc_nodes_locations()

#' Calculate nodes locations for a topology
#'
#' @param x A topology.
#' @param layout String, one of "in-house" or the layout options accepted by
#'     \code{\link[ggraph]{create_layout}}.
#' @param minimize_simple (For in-house algorithm.) Boolean, apply simple
#'     energy minimization by adjusting consumers location based on their
#'     sources y location?
#' @param minimize_E Boolean, use \code{\link{sankey_minimize_energy_sim}} to
#'     adjust node locations using an energy minimization algorithm?
#' @param k1 Numeric, stiffness for springs between nodes on the same x
#'     position (used if minimize_E = TRUE).
#' @param k2 Numeric, sitffness for springs between connected nodes (used if
#'     minimize_E = TRUE).
#' @param l1 Numeric, rest length for springs with k1 stiffness (used if
#'     minimize_E = TRUE).
#' @param l2 Numeric, rest length for springs with k2 stiffness (used if
#'     minimize_E = TRUE).
#'
#' @return A tibble with the nodes and ordered x and y values. The columns are
#'     "comp", "x", and "y".
#'
#' @examples
#' sankey_calc_nodes_locations <- isotracer:::sankey_calc_nodes_locations
#' 
#' sankey_calc_nodes_locations(topo(trini_mod), "left2right")
#' sankey_calc_nodes_locations(topo(aquarium_mod), "stress")
#'
#' @keywords internal
#' @noRd

sankey_calc_nodes_locations <- function(x, layout, 
                                        minimize_simple = TRUE,
                                        minimize_E = FALSE, k1 = 1, k2 = 1, l1 = 1, l2 = 1) {
    if (layout %in% c("left2right", "left-to-right")) {
        # In-house algorithm for topologies without loops
        not_implemented_for_topology_with_loop(x)
        nodes <- sankey_calc_nodes_locations_inhouse(x, minimize_simple = minimize_simple)
        if (minimize_E) {
            nodes <- sankey_minimize_energy_sim(x, nodes, k1 = k1, k2 = k2, l1 = l1,
                                                l2 = l2)
        }
    } else {
        if (!requireNamespace("ggraph", quietly = TRUE)) {
            stop("Package \"ggraph\" needed when specifying a layout argument. Please install it.",
                 call. = FALSE)
        }
        # Check that the random seed  is not reset by graphlayouts functions
        if (exists(".Random.seed", .GlobalEnv)) {
            prev_seed <- .GlobalEnv[[".Random.seed"]]
        } else { prev_seed <- NULL }
        x <- as_tbl_graph(x)
        nodes <- ggraph::create_layout(graph = x, layout = layout)
        if (exists(".Random.seed", .GlobalEnv)) {
            new_seed <- .GlobalEnv[[".Random.seed"]]
        }  else { new_seed <- NULL }
        if (!identical(prev_seed, new_seed)) {
            stop("Random seed was reset when calling ggraph::create_layout() in sankey_calc_nodes_locations().\n",
                 "This is unwanted behaviour, please report it as a bug to the isotracer authors.")
        }
        nodes <- tibble::as_tibble(nodes[, c("name", "x", "y")])
        names(nodes) <- c("comp", "x", "y")
    }
    attr(nodes, "layout") <- layout
    return(nodes)
}

### * sankey_calc_nodes_locations_inhouse()

#' Calculate roughly optimized nodes locations for a topology (in-house algorithm)
#'
#' @param x A topology matrix or a network model (the topology of the first row
#'     is used in this case).
#' @param minimize_simple Boolean, apply simple energy minimization by
#'     adjusting consumers location based on their sources y location.
#'
#' @return A tibble with the nodes and ordered x and y values.
#'
#' @keywords internal
#' @noRd

sankey_calc_nodes_locations_inhouse <- function(x, minimize_simple) {
    if (is(x, "networkModel")) {
        x <- unique(topo(x, simplify = FALSE))
        stopifnot(length(x) == 1)
        x <- x[[1]]
    }
    x <- unclass(x) # Remove the "topo" class to treat it as a matrix
    not_implemented_for_topology_with_loop(x)
    z <- topo_x_ordering(x)
    comps <- names(z)
    d <- tibble::tibble(comp = comps, x = z)
    nodes <- topo_y_ordering(x, d)
    # Center y locations
    x_ranks <- unique(nodes$x)
    for (xi in x_ranks) {
        y_shift <- mean(nodes$y[nodes$x == xi])
        nodes$y[nodes$x == xi] <- nodes$y[nodes$x == xi] - y_shift
    }
    # Adjust y locations
    if (minimize_simple) {
        for (xi in x_ranks) {
            comps <- nodes$comp[nodes$x == xi]
            sources <- colnames(x)[apply(x[comps, , drop = FALSE], 2, sum) > 0]
            if (length(sources) > 0) {
                mean_y_sources <- mean(nodes$y[nodes$comp %in% sources])
                nodes$y[nodes$x == xi] <- nodes$y[nodes$x == xi] + mean_y_sources
            }
        }
    }
    return(nodes)
}

### * topo_x_ordering()

#' Order compartments from left to right
#'
#' @param x A topology matrix or a network model (the topology of the first row
#'     is used in this case).
#'
#' @examples
#' topo_x_ordering <- isotracer:::topo_x_ordering
#' 
#' sort(topo_x_ordering(trini_mod))
#' 
#' @keywords internal
#' @noRd

topo_x_ordering <- function(x) {
    if (is(x, "networkModel")) {
        x <- unique(topo(x, simplify = FALSE))
        stopifnot(length(x) == 1)
        x <- x[[1]]
    }
    x <- unclass(x) # Remove the "topo" class to treat it as a matrix
    n_comps <- ncol(x)
    max_iters <- 2 * n_comps
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
    ranks <- rep(0, n_comps)
    stable <- FALSE
    iter <- 0
    while (!stable & iter <= max_iters) {
        prev_ranks <- ranks
        for (i in seq_len(nrow(links))) {
            if (ranks[links$from[i]] >= ranks[links$to[i]]) {
                ranks[links$to[i]] <- ranks[links$to[i]] + 1
            }
        }
        stable <- all(prev_ranks == ranks)
        iter <- iter + 1
    }
    if (iter == (max_iters + 1)) {
        stop("Maximum number of iterations reached.")
    }
    out <- setNames(ranks, colnames(x))
    return(out)
}

### * topo_y_ordering()

#' Order compartment vertically
#'
#' This function tries to minimize the number of edges crossings for graphical
#' display of the topology.
#'
#' This function assumes that the x locations given in \code{nodes} have been
#' established using \code{\link{topo_x_ordering}}.
#' 
#' @param x A topology matrix or a network model (the topology of the first row
#'     is used in this case).
#' @param nodes A tibble giving the x locations for each node. It must contain
#'     columns "comp" and "x".
#' @param n_iters Number of iterations used to try to optimize the compartment
#'     ordering.
#'
#' @return An updated tibble similar to the \code{nodes} argument.
#'
#' @examples
#' topo_x_ordering <- isotracer:::topo_x_ordering
#' topo_y_ordering <- isotracer:::topo_y_ordering
#' 
#' z <- topo(trini_mod)
#' x <- topo_x_ordering(z)
#' comps <- names(x)
#' d <- tibble::tibble(comp = comps, x = x)
#' nodes <- topo_y_ordering(z, d)
#' 
#' @keywords internal
#' @noRd

topo_y_ordering <- function(x, nodes, n_iters = 2) {
    if (is(x, "networkModel")) {
        x <- unique(topo(x, simplify = FALSE))
        stopifnot(length(x) == 1)
        x <- x[[1]]
    }
    x <- unclass(x) # Remove the "topo" class to treat it as a matrix
    nodes$y <- seq_len(nrow(nodes))
    nodes <- nodes[order(nodes$x, nodes$y), ]
    nodes$id <- seq_len(nrow(nodes))
    comp_id <- setNames(nodes$id, nm = nodes$comp)
    counts <- setNames(topo_count_crossings(x, nodes),
                       paste0(nodes$id, collapse = "-"))
    x_ranks <- unique(nodes$x)
    stopifnot(length(x_ranks) > 1)
    attempts <- 0
    for (iter in seq_len(n_iters)) {
        # Forward walk
        for (i in 1:length(x_ranks)) {
            focal_comps <- nodes$comp[nodes$x == x_ranks[i]]
            focal_y <- nodes$y[nodes$x == x_ranks[i]]
            this_rank_counts <- vector()
            for (ci in focal_comps) {
                swaps <- insert_in_between(ci, focal_comps[focal_comps != ci])
                for (k in seq_along(swaps)) {
                    new_focal_comps <- swaps[[k]]
                    nodes$y[match(new_focal_comps, nodes$comp)] <- focal_y
                    count_name <- paste0(nodes$id[order(nodes$x, nodes$y)],
                                         collapse = "-")
                    counts[count_name] <- topo_count_crossings(x, nodes)
                    this_rank_counts[count_name] <- counts[count_name]
                }
            }
            this_rank_best <- names(this_rank_counts)[which.min(this_rank_counts)]
            this_rank_best_order <- as.numeric(strsplit(this_rank_best, "-")[[1]])
            nodes <- nodes[match(this_rank_best_order, nodes$id), ]
            nodes$y <- seq_len(nrow(nodes))
        }
        # Backward walk
        for (i in length(x_ranks):1) {
            focal_comps <- nodes$comp[nodes$x == x_ranks[i]]
            focal_y <- nodes$y[nodes$x == x_ranks[i]]
            this_rank_counts <- vector()
            for (ci in focal_comps) {
                swaps <- insert_in_between(ci, focal_comps[focal_comps != ci])
                for (k in seq_along(swaps)) {
                    new_focal_comps <- swaps[[k]]
                    nodes$y[match(new_focal_comps, nodes$comp)] <- focal_y
                    count_name <- paste0(nodes$id[order(nodes$x, nodes$y)],
                                         collapse = "-")
                    counts[count_name] <- topo_count_crossings(x, nodes)
                    this_rank_counts[count_name] <- counts[count_name]
                }
            }
            this_rank_best <- names(this_rank_counts)[which.min(this_rank_counts)]
            this_rank_best_order <- as.numeric(strsplit(this_rank_best, "-")[[1]])
            nodes <- nodes[match(this_rank_best_order, nodes$id), ]
            nodes$y <- seq_len(nrow(nodes))
        }
    }
    # Return
    return(nodes[, c("comp", "x", "y")])
}

### * insert_in_between()

#' Insert an element in all possible positions of another vector
#'
#' @param x Element to insert
#' @param into Target vector
#'
#' @return A list of vectors with the x element inserted in all posssible
#'     positions of the target.
#'
#' @examples
#' isotracer:::insert_in_between("a", 1:5)
#'
#' @keywords internal
#' @noRd

insert_in_between <- function(x, into) {
    l <- length(into)
    out <- list()
    for (i in 0:l) {
        z <- c(into[0:i], x)
        if (i+1 <= l) {
            z <- c(z, into[(i+1):l])
        }
        out[[i+1]] <- z
    }
    return(out)
}

### * topo_count_crossings()

#' Count edges crossings given a topology and a proposed node arrangement
#'
#' This function assumes that the x locations given in \code{nodes} have been
#' established using \code{\link{topo_x_ordering}}.
#' 
#' @param x A topology matrix or a network model (the topology of the first row
#'     is used in this case).
#' @param nodes A tibble giving the x and y locations for each node. It must
#'     contain columns "comp", "x" and "y".
#'
#' @return Integer, the number of crossing edges when drawing the nodes
#'     and the connecting edges.
#'
#' @examples
#' topo_x_ordering <- isotracer:::topo_x_ordering
#' topo_count_crossings <- isotracer:::topo_count_crossings
#' 
#' z <- topo(trini_mod)
#' x <- topo_x_ordering(z)
#' comps <- names(x)
#' y <- 1:length(comps)
#' d <- tibble::tibble(comp = comps, x = x, y = y)
#' topo_count_crossings(z, d)
#' 
#' @keywords internal
#' @noRd

topo_count_crossings <- function(x, nodes) {
    if (is(x, "networkModel")) {
        x <- unique(topo(x, simplify = FALSE))
        stopifnot(length(x) == 1)
        x <- x[[1]]
    }
    x <- unclass(x) # Remove the "topo" class to treat it as a matrix
    crossings <- 0
    nodes <- nodes[order(nodes$x, nodes$y), ]
    x_ranks <- unique(nodes$x)
    stopifnot(length(x_ranks) > 1)
    for (i in 2:length(x_ranks)) {
        # Get the edges for gap i
        n <- nodes[nodes$x %in% x_ranks[c(i-1, i)], ]
        e <- x[n$comp[n$x == x_ranks[i]], n$comp[n$x == x_ranks[i-1]], drop = FALSE]
        if (ncol(e)>1) {
            for (j in 2:ncol(e)) {
                k <- which(e[, j] > 0)
                for (w in 1:(j-1)) {
                    for (ki in k) {
                        if (ki < nrow(e)) {
                            crossings <- crossings + sum(e[, w][(ki+1):nrow(e)])
                        }
                    }
                }
            }
        }
    }
    return(crossings)
}

### * sankey_minimize_energy()

#' Adjust nodes y positions using a spring/energy minimization model
#'
#' WIP: This function does not seem to working properly at the moment.
#'
#' @param x A topology.
#' @param nodes The output of \code{\link{sankey_calc_nodes_locations}} run on
#'     \code{x}.
#' @param k1 Numeric, stiffness for springs between nodes on the same x position.
#' @param k2 Numeric, sitffness for springs between connected nodes.
#' @param l1 Numeric, rest length for springs with k1 stiffness.
#' @param l2 Numeric, rest length for springs with k2 stiffness.
#'
#' @return An updated \code{nodes} tibble.
#'
#' @importFrom stats optim
#' 
#' @examples
#' sankey_calc_nodes_locations <- isotracer:::sankey_calc_nodes_locations
#' sankey_minimize_energy <- isotracer:::sankey_minimize_energy
#' 
#' nodes <- sankey_calc_nodes_locations(trini_mod, layout = "left2right")
#' sankey_minimize_energy(topo(trini_mod), nodes)
#' 
#' @keywords internal
#' @noRd

sankey_minimize_energy <- function(x, nodes, k1 = 100, k2 = 10, l1 = 1, l2 = 1) {
    n_comps <- ncol(x)
    links <- which(unclass(x) > 0)
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
    links$from <- colnames(x)[links$from]
    links$to <- colnames(x)[links$to]
    # The y location of the first node is fixed.
    stopifnot(nrow(nodes) > 1)
    nodes <- nodes[order(nodes$x, nodes$y), ]
    links$from <- match(links$from, nodes$comp)
    links$to <- match(links$to, nodes$comp)
    energy <- function(...) {
        nodes$y <- c(nodes$y[1], unlist(list(...)))
        energy <- 0
        # Energy due to springs between nodes on same x_rank
        x_ranks <- unique(nodes$x)
        for (xi in x_ranks) {
            ni <- nodes[nodes$x == xi, ]
            if (nrow(ni) > 1) {
                for (i in 2:nrow(ni)) {
                    delta_l <- ni$y[i] - ni$y[i-1] - l1
                    energy <- energy + 1/2 * k1 * delta_l^2
                }
            }
        }
        # Energy due to springs along connections between nodes of different x_rank
        for (i in 1:nrow(links)) {
            delta_l <- abs(nodes$y[links$to[i]] - nodes$y[links$from[i]]) - l2
            energy <- energy + 1/2 * k2 * delta_l^2
        }
        return(energy)
    }
    start_values <- nodes$y[2:nrow(nodes)]
    y_locs <- optim(start_values, energy, method = "BFGS")$par
    nodes$y <- c(nodes$y[1], y_locs)
    return(nodes)
}

### * sankey_minimize_energy_sim()

#' Adjust nodes y positions using a spring/energy minimization model
#'
#' @param x A topology.
#' @param nodes The output of \code{\link{sankey_calc_nodes_locations}} run on
#'     \code{x}.
#' @param k1 Numeric, stiffness for springs between nodes on the same x
#'     position.
#' @param k2 Numeric, sitffness for springs between connected nodes.
#' @param l1 Numeric, rest length for springs with k1 stiffness.
#' @param l2 Numeric, rest length for springs with k2 stiffness.
#' @param cd Numeric, dampening constant.
#' @param debug Boolean, if TRUE return trajectories instead of the node
#'     tibble.
#' @param return_acc Boolean, if TRUE also return velocity and acceleration
#'     values in the node tibble (useful for debugging).
#' @param n_iters Number of iterations in the simulation loop.
#' @param dt Time step used in the simulation loop.
#'
#' @return An updated \code{nodes} tibble.
#'
#' @examples
#' sankey_calc_nodes_locations <- isotracer:::sankey_calc_nodes_locations
#' sankey_minimize_energy_sim <- isotracer:::sankey_minimize_energy_sim
#' 
#' nodes <- sankey_calc_nodes_locations(trini_mod, layout = "left2right")
#' nodes <- nodes[nodes$x < 2, ]
#' z <- sankey_minimize_energy_sim(topo(trini_mod)[nodes$comp, nodes$comp], nodes,
#'        n_iters = 200, cd = 0.8, dt = 0.1, debug = TRUE)
#' z <- do.call(rbind, z)
#' lattice::xyplot(ts(z))
#'
#' (z <- sankey_minimize_energy_sim(topo(trini_mod)[nodes$comp, nodes$comp], nodes,
#'         n_iters = 200, cd = 0.8, dt = 0.1, return_acc = TRUE))
#' plot(z$x, z$y)
#' text(z$x, z$y, z$comp)
#' 
#' @keywords internal
#' @noRd

sankey_minimize_energy_sim <- function(x, nodes, k1 = 1, k2 = 1, l1 = 1, l2 = 1,
                                       cd = 0.8, n_iters = 25, dt = 0.5, debug = FALSE,
                                       return_acc = FALSE) {
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
    links$from <- colnames(x)[links$from]
    links$to <- colnames(x)[links$to]
    stopifnot(nrow(nodes) > 1)
    nodes <- nodes[order(nodes$x, nodes$y), ]
    nodes$v <- 0
    links$from <- match(links$from, nodes$comp)
    links$to <- match(links$to, nodes$comp)
    traj <- list()
    x_ranks <- unique(nodes$x)
    # Dynamic simulation
    for (i in seq_len(n_iters)) {
        nodes$a <- 0
        traj[[i]] <- nodes$y
        # Add forces due to springs between nodes on same x_rank
        for (xi in x_ranks) {
            ni <- nodes[nodes$x == xi, ]
            if (nrow(ni) > 1) {
                for (i in 2:nrow(ni)) {
                    delta_l <- ni$y[i] - ni$y[i-1] - l1
                    force <- k1 * delta_l
                    ni$a[i] <- ni$a[i] - force
                    ni$a[i-1] <- ni$a[i-1] + force
                }
            }
            nodes[nodes$x == xi, ] <- ni
        }
        # Add forces due to springs along connections between nodes of different x_rank
        for (i in 1:nrow(links)) {
            delta_l <- abs(nodes$y[links$to[i]] - nodes$y[links$from[i]]) - l2
            force <- k2 * delta_l
            if (nodes$y[links$to[i]] > nodes$y[links$from[i]]) {
                nodes$a[links$to[i]] <- nodes$a[links$to[i]] - force
                nodes$a[links$from[i]] <- nodes$a[links$from[i]] + force
            } else {
                nodes$a[links$to[i]] <- nodes$a[links$to[i]] + force
                nodes$a[links$from[i]] <- nodes$a[links$from[i]] - force
            }
        }
        # Dampening
        for (i in seq_len(nrow(nodes))) {
            nodes$a[i] <- nodes$a[i] - cd * nodes$v[i]
        }
        # Integration
        for (i in seq_len(nrow(nodes))) {
            nodes$y[i] <- nodes$y[i] + dt * nodes$v[i]
            nodes$v[i] <- nodes$v[i] + dt * nodes$a[i]
        }
    }
    if (debug) {
        return(traj)
    }
    if (return_acc) {
        return(nodes)
    }
    return(nodes[, c("comp", "x", "y")])
}

### * sankey_draw_node()

#' Draw a rounded rectangle with a label
#'
#' @param x,y Locations of the center of the rectangle.
#' @param label String, label added inside the rectangle.
#' @param fill Fill color.
#' @param col Border color.
#' @param text_col Text color.
#' @param style One of "roundrect", "square".
#' @param padding Padding value.
#' @param padding_factor Adjustment factor for padding.
#' @param r_factor Adjustment factor for rounded corner.
#' @param side Vector of length 1 or 2 giving node width and height values
#'     (used for style = "square").
#' @param factor Adjustment factor for node dimensions (used for style =
#'     "square").
#'
#' @keywords internal
#' @noRd

sankey_draw_node <- function(x, y, label,
                             fill = NULL, col = "black", text_col = NULL,
                             style = "roundrect",
                             padding = NULL, padding_factor = 1, r_factor = 1, # For roundrect
                             side = NULL, factor = 1) {
    # Convert x and y coordinates
    x <- grid::unit(x, "native")
    y <- grid::unit(y, "native")
    # Draw box
    if (is.null(fill)) {
        gp <- grid::gpar(col = col)
    } else {
        gp <- grid::gpar(col = col, fill = fill)
    }
    if (style == "roundrect") {
        r <- grid::unit(0.1, "snpc") * r_factor
        if (is.null(padding)) {
            padding <- grid::unit(1, "strwidth", "o")
        }
        width <- grid::unit(1, "strwidth", label) + 2 * padding * padding_factor
        height <- grid::unit(1, "strheight", label) + 2 * padding * padding_factor
        grid::grid.roundrect(x = x, y = y, width = width, height = height,
                             gp = gp, r = r)
    } else if (style == "square") {
        if (is.null(side)) {
            side <- grid::unit(1, "strwidth", "toto")
        }
        if (length(side) == 1) {
            side <- grid::unit.c(side, side)
        }
        grid::grid.rect(x = x, y = y, width = side[1] * factor,
                        height = side[2] * factor, gp = gp)
    }
    # Draw label
    if (is.null(text_col)) {
        text_col <- col
    }
    gp <- grid::gpar(col = text_col)
    grid::grid.text(label = label, x = x, y = y, gp = gp)
}

### * sankey_draw_edge()

#' Draw a constant-width ribbon between nodes
#'
#' @param backbone Two-column table containing the x and y coordinates of the
#'     points making the mid-line of the edge ribbon.
#' @param width Width of the edge ribbon.
#' @param fill Color for ribbon fill.
#' @param col Color for ribbon border.
#' @param factor Adjustment factor for ribbon width.
#' 
#' @keywords internal
#' @noRd

sankey_draw_edge <- function(backbone, width = 0.1, fill = NULL, col = "black",
                             factor = 1) {
    rb <- ribbonFromTrajectory(backbone, width * factor)
    if (is.null(fill)) {
        gp <- grid::gpar(col = col)
    } else {
        gp <- grid::gpar(col = col, fill = fill)
    }
    grid::grid.polygon(x = rb[, 1], y = rb[, 2], default.units = "native", gp = gp)
}

### * make_and_push_ortho_vp()

#' Make an orthonormal data viewport
#'
#' This function must be run when a parent viewport already exists. It adds a
#' container viewport, centered, with specified width and height, and fills
#' this container viewport with an orthonormal data viewport (based on the
#' physical dimensions of the container viewport).
#'
#' Note that this function creates and push two viewports on the stack (a
#' container filling the parent viewport completely and a data viewport filling
#' the container, with an orthonormal cartesian coordinate system), so that two
#' viewports must be popped in order to come back to the parent viewport in use
#' before the function call.
#'
#' @param width,height Numeric between 0 and 1, fraction of the parent viewport
#'     width and height filled by the new viewport.
#' @param debug Boolean.
#' 
#' @return An orthonormal data viewport which bounds (0, 1) on one axis.
#'
#' @examples
#' library(grid)
#' grid.newpage()
#' pushViewport(viewport())
#' ortho_vp <- isotracer:::make_and_push_ortho_vp(width = 0.9, height = 0.9,
#'               debug = TRUE)
#'
#' @keywords internal
#' @noRd

make_and_push_ortho_vp <- function(width = 1, height = 1, debug = FALSE) {
    debug_bg_col <- "#095ba7"
    debug_line_col <- "#9fccfa" # "#baecfa"
    parent_vp <- grid::current.viewport()
    if (debug) {
        grid::grid.rect(gp = grid::gpar(col = debug_line_col,
                                        fill = debug_bg_col))
    }
    container_vp <- grid::viewport(width = width, height = height,
                                   name = "ortho_container")
    grid::pushViewport(container_vp)
    if (debug) {
        grid::grid.rect(gp = grid::gpar(col = debug_line_col,
                                        fill = debug_bg_col))
    }
    container_dims <- grid::deviceDim(container_vp$width,
                                      container_vp$height,
                                      valueOnly = TRUE)
    canvas_dims <- container_dims
    if (canvas_dims$w >= canvas_dims$h) {
        y_scale <- c(0, 1)
        x_scale <- y_scale * canvas_dims$w / canvas_dims$h
        x_scale <- x_scale - (x_scale[2] - 1) / 2
    } else {
        x_scale <- c(0, 1)
        y_scale <- x_scale * canvas_dims$h / canvas_dims$w
        y_scale <- y_scale - (y_scale[2] - 1) / 2
    }
    canvas_vp <- grid::dataViewport(xscale = x_scale, yscale = y_scale,
                                    name = "ortho_canvas")
    grid::pushViewport(canvas_vp)
    if (debug) {
        shift <- 0.05
        arrow_style <- grid::arrow(length = grid::unit(0.01, "native"))
        grid::grid.rect(width = 1, height = 1,
                        default.units = "native",
                        gp = grid::gpar(col = debug_line_col, lty = 2))
        grid::grid.lines(x = c(0, 0.1) + shift, y = shift, default.units = "native",
                         gp = grid::gpar(col = debug_line_col),
                         arrow = arrow_style)
        grid::grid.lines(x = shift, y = c(0, 0.1) + shift, default.units = "native",
                         gp = grid::gpar(col = debug_line_col),
                         arrow = arrow_style)
        grid::grid.points(x = 0.5, y = 0.5, default.units = "native",
                          pch = 3, gp = grid::gpar(col = debug_line_col))
        grid::grid.text("0.1 x", x = grid::unit(0.05 + shift, "native"),
                        y = grid::unit(shift, "native") + grid::unit(0.7, "strheight", "x"),
                        gp = grid::gpar(cex = 1, col = debug_line_col))
        grid::grid.text("0.1 y", x = grid::unit(shift, "native") + grid::unit(0.7, "strheight", "x"),
                        y = grid::unit(0.05 + shift, "native"),
                        gp = grid::gpar(cex = 1, col = debug_line_col),
                        rot = -90)
        grid::grid.text("(0, 0)",
                        x = grid::unit(0, "native") + grid::unit(0.1, "strwidth", "x"),
                        y = grid::unit(0, "native") + grid::unit(0.4, "strheight", "x"),
                        hjust = 0, vjust = 0,
                        gp = grid::gpar(cex = 1, col = debug_line_col))
        grid::grid.text("(1, 1)",
                        x = grid::unit(1, "native") - grid::unit(1, "strwidth", "(1, 1)"),
                        y = grid::unit(1, "native") - grid::unit(0.4, "strheight", "x"),
                        hjust = 0, vjust = 1,
                        gp = grid::gpar(cex = 1, col = debug_line_col))
        topleft <- signif(c(canvas_vp$xscale[1], canvas_vp$yscale[2]), 3)
        topleft_char <- paste0("(", topleft[1], ", ", topleft[2], ")")
        grid::grid.text(topleft_char,
                        x = grid::unit(topleft[1], "native") + grid::unit(0.1, "strwidth", "x"),
                        y = grid::unit(topleft[2], "native") - grid::unit(0.4, "strheight", "x"),
                        hjust = 0, vjust = 1,
                        gp = grid::gpar(cex = 1, col = debug_line_col))
        bottomright <- signif(c(canvas_vp$xscale[2], canvas_vp$yscale[1]), 3)
        bottomright_char <- paste0("(", bottomright[1], ", ", bottomright[2], ")")
        grid::grid.text(bottomright_char,
                        x = grid::unit(bottomright[1], "native") - grid::unit(1, "strwidth", bottomright_char),
                        y = grid::unit(bottomright[2], "native") + grid::unit(0.4, "strheight", "x"),
                        hjust = 0, vjust = 0,
                        gp = grid::gpar(cex = 1, col = debug_line_col))
        grid::grid.text("(0.5, 0.5)",
                        x = grid::unit(0.5, "native") + grid::unit(0.1, "strwidth", "x"),
                        y = grid::unit(0.5, "native") + grid::unit(0.4, "strheight", "x"),
                        hjust = 0, vjust = 0,
                        gp = grid::gpar(cex = 1, col = debug_line_col))
    }
    return(canvas_vp)
}

### * sankey_draw_edges()

#' @keywords internal
#' @noRd

sankey_draw_edges <- function(edges, defaults = list(col = adjustcolor("black", alpha.f = 0.5),
                                                     fill = adjustcolor("grey", alpha.f = 0.5),
                                                     lwd = 1, lty = 1),
                              debug = FALSE) {
    debug_bg_col <- "#095ba7"
    debug_line_col <- "#9fccfa" # "#baecfa"
    if (is.null(edges) || nrow(edges) == 0) {
        return(NULL)
    }
    for (property in names(defaults)) {
        if (! property %in% colnames(edges)) {
            edges[[property]] <- defaults[[property]]
        }
    }
    for (i in seq_len(nrow(edges))) {
        rb <- ribbonFromTrajectory(edges[["backbone"]][[i]],
                                   width = edges[["width"]][i])
        gp <- grid::gpar(col = edges[["col"]][i],
                         fill = edges[["fill"]][i],
                         lwd = edges[["lwd"]][i],
                         lty = edges[["lty"]][i])
        grid::grid.polygon(x = rb[, 1], y = rb[, 2], default.units = "native",
                           gp = gp)
        if (debug) {
            grid::grid.lines(x = edges[["control_points"]][[i]][, 1],
                             y = edges[["control_points"]][[i]][, 2],
                             default.units = "native",
                             gp = grid::gpar(col = debug_line_col,
                                             lty = 2))
            grid::grid.lines(x = edges[["backbone"]][[i]][, 1],
                             y = edges[["backbone"]][[i]][, 2],
                             default.units = "native",
                             gp = grid::gpar(col = debug_line_col))
            grid::grid.points(x = edges[["control_points"]][[i]][, 1],
                              y = edges[["control_points"]][[i]][, 2],
                              default.units = "native",
                              pch = 1,
                              gp = grid::gpar(col = debug_line_col,
                                              cex = 0.75))
            grid::grid.points(x = edges[["backbone"]][[i]][, 1],
                              y = edges[["backbone"]][[i]][, 2],
                              default.units = "native",
                              pch = 21,
                              gp = grid::gpar(col = debug_line_col,
                                              fill = debug_line_col,
                                              cex = 0.5))
        }
    }
    return(NULL)
}

### * sankey_draw_nodes()

#' @keywords internal
#' @noRd

sankey_draw_nodes <- function(nodes, defaults = list(label = "",
                                                     col = adjustcolor("black", alpha.f = 0.5),
                                                     fill = adjustcolor("grey", alpha.f = 0.5),
                                                     lwd = 1, lty = 1),
                              debug = FALSE, node_s = "default") {
    debug_bg_col <- "#095ba7"
    debug_line_col <- "#9fccfa" # "#baecfa"
    if (is.null(nodes) || nrow(nodes) == 0) {
        return(NULL)
    }
    for (property in names(defaults)) {
        if (! property %in% colnames(nodes)) {
            nodes[[property]] <- defaults[[property]]
        }
    }
    for (i in seq_len(nrow(nodes))) {
        gp <- grid::gpar(col = nodes$col[i],
                         fill = nodes$fill[i],
                         lwd = nodes$lwd[i],
                         lty = nodes$lty[i])
        if (node_s == "roundsquare") {
            grid::grid.roundrect(x = nodes$x[i], y = nodes$y[i],
                            width = nodes$width[[i]],
                            height = nodes$height[[i]],
                            default.units = "native",
                            gp = gp)
        } else {
            grid::grid.rect(x = nodes$x[i], y = nodes$y[i],
                            width = nodes$width[[i]],
                            height = nodes$height[[i]],
                            default.units = "native",
                            gp = gp)
        }
        if (debug) {
            gp <- grid::gpar(col = debug_line_col,
                             lty = 5)
            if (node_s == "roundrect") {
                grid::grid.roundrect(x = nodes$x[i], y = nodes$y[i],
                                width = nodes$width[[i]],
                                height = nodes$height[[i]],
                                default.units = "native",
                                gp = gp)
            } else {
                grid::grid.rect(x = nodes$x[i], y = nodes$y[i],
                                width = nodes$width[[i]],
                                height = nodes$height[[i]],
                                default.units = "native",
                                gp = gp)
            }
            left <- nodes$x[i] - grid::convertWidth(nodes$width[[i]], unitTo = "native",
                                                    valueOnly = TRUE) / 2
            right <- nodes$x[i] + grid::convertWidth(nodes$width[[i]], unitTo = "native",
                                                     valueOnly = TRUE) / 2
            bottom <- nodes$y[i] - grid::convertHeight(nodes$height[[i]], unitTo = "native",
                                                       valueOnly = TRUE) / 2
            top <- nodes$y[i] + grid::convertHeight(nodes$height[[i]], unitTo = "native",
                                                    valueOnly = TRUE) / 2
            gp <- grid::gpar(col = debug_line_col,
                             lty = "26")
            grid::grid.lines(x = c(left, right), y = c(top, bottom),
                             default.units = "native", gp = gp)
            grid::grid.lines(x = c(left, right), y = c(bottom, top),
                             default.units = "native", gp = gp)
            gp <- grid::gpar(col = debug_line_col,
                             fill = debug_bg_col)
            grid::grid.points(x = nodes[["x"]][i], y = nodes[["y"]][i],
                              pch = 5, size = grid::unit(0.8, "char"),
                              default.units = "native", gp = gp)
            grid::grid.text(x = nodes[["x"]][i],
                            y = grid::unit(nodes[["y"]][i], "native") +
                                grid::unit(1.4, "strheight", "x"),
                            default.units = "native", gp = gp,
                            label = nodes[["comp"]][i])
        }
    }
    return(NULL)
}

### * sankey_draw_labels()

#' @keywords internal
#' @noRd

sankey_draw_labels <- function(labels, defaults = list(label = "",
                                                       cex = 1,
                                                       col = adjustcolor("black", alpha.f = 0.5),
                                                       fill = adjustcolor("grey", alpha.f = 0.5),
                                                       lwd = 1, lty = 1),
                               debug = FALSE) {
    debug_bg_col <- "#095ba7"
    debug_line_col <- "#9fccfa" # "#baecfa"
    if (is.null(labels) || nrow(labels) == 0) {
        return(NULL)
    }
    for (property in names(defaults)) {
        if (! property %in% colnames(labels)) {
            labels[[property]] <- defaults[[property]]
        }
    }
    for (i in seq_len(nrow(labels))) {
        if (debug) {
            gp <- grid::gpar(col = debug_line_col,
                             lwd = 0.5)
            grid::grid.points(x = labels$label_x[i],
                              y = labels$label_y[[i]],
                              default.units = "native",
                              pch = 3, gp = gp)
            half_height <- grid::convertHeight(grid::unit(1/2 * labels$cex[i], "strheight", labels$label[[i]]),
                                               unitTo = "native", valueOnly = TRUE)
            top <- labels$label_y[i] + half_height
            bottom <- labels$label_y[i] - half_height
            left <- grid::convertWidth(grid::unit(labels$label_x[i], "native") -
                                        grid::unit(0.525 * labels$cex[i], "strwidth", labels$label[[i]]),
                                        unitTo = "native", valueOnly = TRUE)
            right <- grid::convertWidth(grid::unit(labels$label_x[i], "native") +
                                         grid::unit(0.525 * labels$cex[i], "strwidth", labels$label[[i]]),
                                         unitTo = "native", valueOnly = TRUE)
            grid::grid.lines(c(left, right), c(top, top),
                             default.units = "native", gp = gp)
            grid::grid.lines(c(left, right), c(bottom, bottom),
                             default.units = "native", gp = gp)
        }
        gp <- grid::gpar(col = labels$col[i],
                         cex = labels$cex[i])
        grid::grid.text(label = labels$label[[i]],
                        x = labels$label_x[i],
                        y = labels$label_y[[i]],
                        default.units = "native", gp = gp)
    }
    return(NULL)
}

### * (1) sankey_place_nodes()

#' Perform initial placement of the nodes
#'
#' @param topo Topology.
#' @param nodes Node tibble.
#' @param flows Flows tibble.
#' @param layout Layout string.
#' @param xlim,ylim Limits of the x and y scales.
#' 
#' @examples
#' sankey_place_nodes <- isotracer:::sankey_place_nodes
#' 
#' topo <- topo(trini_mod)
#' nodes <- tibble::tibble(comp = colnames(topo), size = 1, col = "red")
#' nodes$label <- letters[1:nrow(nodes)]
#' sankey_place_nodes(topo, nodes, layout = "left2right")
#' sankey_place_nodes(topo, nodes, layout = "stress")
#' 
#' @keywords internal
#' @noRd

sankey_place_nodes <- function(topo, nodes = NULL, flows, layout, xlim = c(0, 1),
                               ylim = c(0, 1)) {
    nodes_arg <- nodes
    # Calculate node locations
    nodes <- sankey_calc_nodes_locations(topo, layout)
    # Adjust node locations to fill the canvas
    nodes$x <- (nodes$x - min(nodes$x)) / (max(nodes$x) - min(nodes$x)) * diff(xlim) + xlim[1]
    nodes$y <- (nodes$y - min(nodes$y)) / (max(nodes$y) - min(nodes$y)) * diff(ylim) + ylim[1]
    nodes$label <- nodes$comp
    # Return
    if (!is.null(nodes_arg)) {
        if (any(colnames(nodes_arg) %in% c("x", "y"))) {
            stop("Provided `nodes` tibble cannot have `x` or `y` column.")
        }
        if (! setequal(nodes$comp, nodes_arg$comp)) {
            stop("Provided `nodes` tibble must have a `comp` column with exactly the same entries as the topology compartments.")
        }
        if ("label" %in% colnames(nodes_arg)) {
            nodes$label <- NULL
        }
        nodes <- dplyr::left_join(nodes, nodes_arg, by = "comp")
    }
    out <- list(nodes = nodes, edges = NULL)
    attr(out, "layout") <- layout
    return(out)
}

### * (2) sankey_place_edge_sockets_on_nodes()

#' Determine relative location of edge sockets on nodes
#'
#' @param scene A list with a "nodes" tibble.
#' @param topo A topology.
#' @param nodes A node tibble.
#' @param flows A tibble giving the flow rates between connected nodes.
#' @param layout String specifying the plot layout.
#'
#' @examples
#' sankey_place_nodes <- isotracer:::sankey_place_nodes
#' sankey_place_edge_sockets_on_nodes <- isotracer:::sankey_place_edge_sockets_on_nodes
#' 
#' topo <- topo(trini_mod)
#' flows <- flows_from_topo(topo)
#' flows$width <- 1
#' layout <- "stress"
#' scene <- sankey_place_nodes(topo, flows = flows, layout = layout)
#' z <- sankey_place_edge_sockets_on_nodes(scene, topo, flows = flows, layout = layout)
#' 
#' @keywords internal
#' @noRd

sankey_place_edge_sockets_on_nodes <- function(scene, topo, nodes = NULL, flows, layout) {
    nodes <- scene$nodes
    edges <- flows
    stopifnot(is.null(scene[["edges"]]))
    edges$from_x <- nodes$x[match(edges$from, nodes$comp)]
    edges$to_x <- nodes$x[match(edges$to, nodes$comp)]
    edges$from_y <- nodes$y[match(edges$from, nodes$comp)]
    edges$to_y <- nodes$y[match(edges$to, nodes$comp)]
    sockets <- list()
    # Layout left2right
    if (layout == "left2right") {
        edges <- edges[order(edges$from_x, edges$from_y), ]
        nodes$left_accumulator <- 0
        nodes$right_accumulator <- 0
        for (i in seq_len(nrow(edges))) {
            from <- edges$from[i]
            to <- edges$to[i]
            edge_id <- c(from = from, to = to)
            # Socket from
            socket_from <- list()
            socket_from[["node_id"]] <- from
            socket_from[["node_side"]] <- "right"
            socket_from[["edge_id"]] <- list(edge_id)
            socket_from[["edge_end"]] <- "from"
            socket_from[["rel_loc"]] <- (nodes$right_accumulator[nodes$comp == from] +
                                         edges$width[i]/2)
            nodes$right_accumulator[nodes$comp == from] <- (nodes$right_accumulator[nodes$comp == from] +
                                                            edges$width[i])
            socket_from[["width"]] <- edges$width[i]
            # Socket to
            socket_to <- list()
            socket_to[["node_id"]] <- to
            socket_to[["node_side"]] <- "left"
            socket_to[["edge_id"]] <- list(edge_id)
            socket_to[["edge_end"]] <- "to"
            socket_to[["rel_loc"]] <- (nodes$left_accumulator[nodes$comp == to] +
                                       edges$width[i]/2)
            nodes$left_accumulator[nodes$comp == to] <- (nodes$left_accumulator[nodes$comp == to] +
                                                         edges$width[i])
            socket_to[["width"]] <- edges$width[i]
            # Store sockets
            sockets[[2*i-1]] <- socket_from
            sockets[[2*i]] <- socket_to
        }
    } else if (layout %in% c("stress", "kk", "lgl", "fr", "dh", "mds")) {
        # Layout stress or kk or ...
        edges <- edges[order(edges$from_x, edges$from_y), ]
        sides <- c("right", "top", "left", "bottom")
        # Place sockets (node side and angular location)
        for (i in seq_len(nrow(edges))) {
            from <- edges$from[i]
            to <- edges$to[i]
            edge_id <- c(from = from, to = to)
            edge_id_char <- paste(from, to, sep = "->")
            recip_edge_id <- paste(to, from, sep = "->")
            # Socket from
            quadrant <- sankey_stress_quadrant(y = edges$to_y[i] - edges$from_y[i],
                                               x = edges$to_x[i] - edges$from_x[i])
            socket_from <- list()
            socket_from[["node_id"]] <- from
            socket_from[["node_side"]] <- sides[quadrant["q"]]
            socket_from[["edge_id"]] <- list(edge_id)
            socket_from[["edge_id_char"]] <- edge_id_char
            socket_from[["recip_edge_id"]] <- recip_edge_id
            socket_from[["edge_end"]] <- "from"
            socket_from[["rel_quadrant_angle"]] <- quadrant["a"]
            socket_from[["width"]] <- edges$width[i]
            # Socket to
            quadrant <- sankey_stress_quadrant(y = -(edges$to_y[i] - edges$from_y[i]),
                                               x = -(edges$to_x[i] - edges$from_x[i]))
            socket_to <- list()
            socket_to[["node_id"]] <- to
            socket_to[["node_side"]] <- sides[quadrant["q"]]
            socket_to[["edge_id"]] <- list(edge_id)
            socket_to[["edge_id_char"]] <- edge_id_char
            socket_to[["recip_edge_id"]] <- recip_edge_id
            socket_to[["edge_end"]] <- "to"
            socket_to[["rel_quadrant_angle"]] <- quadrant["a"]
            socket_to[["width"]] <- edges$width[i]
            # Give a little nudge if a reciprocal edge exists
            if (recip_edge_id %in% sapply(sockets, "[[", "edge_id_char")) {
                socket_to[["rel_quadrant_angle"]] <- socket_to[["rel_quadrant_angle"]] - 1e-8
            }
            # Store sockets
            sockets[[2*i-1]] <- socket_from
            sockets[[2*i]] <- socket_to
        }
        sockets <- dplyr::bind_rows(sockets)
        sockets <- sockets[order(sockets$node_id, sockets$node_side,
                                 sockets$rel_quadrant_angle), ]

        # Update the node side accumulators and sockets relative locations
        nodes$left_accumulator <- 0
        nodes$right_accumulator <- 0
        nodes$top_accumulator <- 0
        nodes$bottom_accumulator <- 0
        sockets[["rel_loc"]] <- 0
        for (i in seq_len(nrow(sockets))) {
            ni <- which(nodes$comp == sockets$node_id[i])
            side <- sockets$node_side[i]
            if (side == "left") {
                sockets$rel_loc[i] <- nodes$left_accumulator[ni] - sockets$width[i]/2
                nodes$left_accumulator[ni] <- nodes$left_accumulator[ni] - sockets$width[i]
            }
            if (side == "bottom") {
                sockets$rel_loc[i] <- nodes$bottom_accumulator[ni] + sockets$width[i]/2
                nodes$bottom_accumulator[ni] <- nodes$bottom_accumulator[ni] + sockets$width[i]
            }
            if (side == "right") {
                sockets$rel_loc[i] <- nodes$right_accumulator[ni] + sockets$width[i]/2
                nodes$right_accumulator[ni] <- nodes$right_accumulator[ni] + sockets$width[i]
            }
            if (side == "top") {
                sockets$rel_loc[i] <- nodes$top_accumulator[ni] - sockets$width[i]/2
                nodes$top_accumulator[ni] <- nodes$top_accumulator[ni] - sockets$width[i]
            }
        }

        # Optimize sockets distribution around a given node
        for (n in nodes$comp) {
            node_sockets <- sockets[sockets$node_id == n, ]
            current_imbalance <- sankey_stress_socket_imbalance(node_sockets)
            possible_moves <- sankey_stress_socket_moves(node_sockets)
            if (length(possible_moves) > 0) {
                possible_imbalances <- sapply(possible_moves,
                                              sankey_stress_socket_imbalance)
                possible_moves <- possible_moves[order(possible_imbalances)]
                possible_imbalances <- sort(possible_imbalances)
                if (possible_imbalances[1] < current_imbalance) {
                    # Do the move
                    sockets[sockets$node_id == n, ] <- possible_moves[[1]]
                }
            }
        }

        # Update nodes accumulators and sockets rel_loc
        sockets <- sockets[order(sockets$node_id, sockets$node_side,
                                 sockets$rel_quadrant_angle), ]
        nodes$left_accumulator <- 0
        nodes$right_accumulator <- 0
        nodes$top_accumulator <- 0
        nodes$bottom_accumulator <- 0
        sockets[["rel_loc"]] <- 0
        for (i in seq_len(nrow(sockets))) {
            ni <- which(nodes$comp == sockets$node_id[i])
            side <- sockets$node_side[i]
            if (side == "left") {
                sockets$rel_loc[i] <- nodes$left_accumulator[ni] - sockets$width[i]/2
                nodes$left_accumulator[ni] <- nodes$left_accumulator[ni] - sockets$width[i]
            }
            if (side == "bottom") {
                sockets$rel_loc[i] <- nodes$bottom_accumulator[ni] + sockets$width[i]/2
                nodes$bottom_accumulator[ni] <- nodes$bottom_accumulator[ni] + sockets$width[i]
            }
            if (side == "right") {
                sockets$rel_loc[i] <- nodes$right_accumulator[ni] + sockets$width[i]/2
                nodes$right_accumulator[ni] <- nodes$right_accumulator[ni] + sockets$width[i]
            }
            if (side == "top") {
                sockets$rel_loc[i] <- nodes$top_accumulator[ni] - sockets$width[i]/2
                nodes$top_accumulator[ni] <- nodes$top_accumulator[ni] - sockets$width[i]
            }
        }
    } else {
        # Default sockets
        for (i in seq_len(nrow(edges))) {
            from <- edges$from[i]
            to <- edges$to[i]
            edge_id <- c(from = from, to = to)
            # Socket from
            socket_from <- list()
            socket_from[["node_id"]] <- from
            socket_from[["node_side"]] <- "center"
            socket_from[["edge_id"]] <- list(edge_id)
            socket_from[["edge_end"]] <- "from"
            socket_from[["rel_loc"]] <- 0
            socket_from[["width"]] <- edges$width[i]
            # Socket to
            socket_to <- list()
            socket_to[["node_id"]] <- to
            socket_to[["node_side"]] <- "center"
            socket_to[["edge_id"]] <- list(edge_id)
            socket_to[["edge_end"]] <- "to"
            socket_to[["rel_loc"]] <- 0
            socket_to[["width"]] <- edges$width[i]
            # Store sockets
            sockets[[2*i-1]] <- socket_from
            sockets[[2*i]] <- socket_to
        }
    }
    # Return
    sockets <- dplyr::bind_rows(sockets)
    out <- scene
    out[["nodes"]] <- nodes
    out[["edges"]] <- edges
    out[["edge_sockets"]] <- sockets
    return(out)
}

### * (3) sankey_calc_node_shape()

#' Determine node shape
#'
#' @importFrom stats aggregate
#' 
#' @param scene Scene list.
#' @param topo Topology.
#' @param nodes Node tibble.
#' @param flows Flows tibble.
#' @param layout Layout string.
#' @param node_f Multiplicative factor used to adjust node size.
#' @param xlim Limits of the x scale.
#' @param node_s String defining how node size is calculated. The effect of the
#'     string also depends on the chosen layout.
#' 
#' @examples
#' library(magrittr)
#' 
#' sankey_place_nodes <- isotracer:::sankey_place_nodes
#' sankey_place_edge_sockets_on_nodes <- isotracer:::sankey_place_edge_sockets_on_nodes
#' sankey_calc_node_shape <- isotracer:::sankey_calc_node_shape
#' 
#' topo <- topo(trini_mod)
#' flows <- flows_from_topo(topo)
#' flows$width <- 1
#' layout <- "left2right"
#' scene <- sankey_place_nodes(topo, flows = flows, layout = layout)
#' scene <- sankey_place_edge_sockets_on_nodes(scene, topo, flows = flows, layout = layout)
#' z <- sankey_calc_node_shape(scene, topo, flows = flows, layout = layout)
#'
#' topo <- topo(trini_mod)
#' flows <- flows_from_topo(topo)
#' flows$width <- 1
#' layout <- "stress"
#' scene <- sankey_place_nodes(topo, flows = flows, layout = layout)
#' scene <- sankey_place_edge_sockets_on_nodes(scene, topo, flows = flows, layout = layout)
#' z <- sankey_calc_node_shape(scene, topo, flows = flows, layout = layout)
#'
#' y <- new_networkModel() %>%
#'          set_topo(c("subs -> NH3 -> subs",
#'                     "NH3 -> Q, E", "E -> Q -> E",
#'                     "E -> D, M")) %>%
#'          set_steady("subs") %>%
#'              set_prop_family("normal_sd")
#' y <- topo(y)
#' nodes <- nodes_from_topo(y)
#' nodes$size <- runif(nrow(nodes), 1, 2)
#' flows <- flows_from_topo(y)
#' flows$width <- runif(nrow(flows), 0.2, 5)
#' layout <- "stress"
#' scene <- sankey_place_nodes(y, nodes = nodes, flows = flows, layout = layout)
#' scene <- sankey_place_edge_sockets_on_nodes(scene, y, flows = flows, layout = layout)
#' z <- sankey_calc_node_shape(scene, y, flows = flows, layout = layout)
#'  
#' @keywords internal
#' @noRd

sankey_calc_node_shape <- function(scene, topo, nodes = NULL, flows, layout,
                                   node_f = 1, xlim = c(0, 1), node_s = "auto") {
    nodes <- scene$nodes
    sockets <- scene$edge_sockets
    nodes$shape <- NA
    nodes$width <- NA
    nodes$height <- NA
    if (!"size" %in% colnames(nodes)) {
        nodes[["size"]] <- 1
    }
    # Layout left2right
    if (layout == "left2right") {
        for (i in seq_len(nrow(nodes))) {
            nodes$shape[i] <- "rect"
            nodes$height[i] <- list(grid::unit(max(nodes$left_accumulator[i],
                                              nodes$right_accumulator[i]),
                                              "native"))
            nodes$width[i] <- nodes$size[i] / as.numeric(nodes$height[[i]])
        }
        # Adjust nodes width
        max_width_per_x <- aggregate(width ~ x, data = nodes, FUN = max)
        total_width <- sum(max_width_per_x$width)
        max_allowed_width <- diff(xlim) * 0.5
        adj_factor <- max_allowed_width / total_width
        nodes$width <- lapply(nodes$width * adj_factor * node_f,
                              function(x) grid::unit(x, "native") )
    } else if (layout %in% c("stress", "kk", "lgl", "fr", "dh", "mds")) {
        # Layout stress or kk or ...
        min_width <- grid::convertWidth(grid::unit(1, "strwidth", "x"), unitTo = "native",
                                        valueOnly = TRUE)
        min_height <- abs(grid::convertHeight(grid::unit(1, "strheight", "x"), unitTo = "native",
                                              valueOnly = TRUE))
        for (i in seq_len(nrow(nodes))) {
            node_sockets <- sockets[sockets$node_id == nodes$comp[i], ]
            side_sockets <- lapply(c("top", "left", "right", "bottom"), function(s) {
                z <- node_sockets[node_sockets$node_side == s, ]
                if (nrow(z) == 0) return(0)
                return(sum(z$width))
            })
            names(side_sockets) <- c("top", "left", "right", "bottom")
            nodes$shape[i] <- "rect"
            nodes$width[i] <- list(grid::unit(max(min_width, side_sockets[["top"]],
                                                  side_sockets[["bottom"]]),
                                              units = "native"))
            nodes$height[i] <- list(grid::unit(max(min_height, side_sockets[["left"]],
                                                   side_sockets[["right"]]),
                                               units = "native"))
        }
        if (node_s == "constant") {
            max_width <- do.call(max, nodes$width)
            max_height <- do.call(max, nodes$height)
            nodes$width <- rep(list(max_width), nrow(nodes))
            nodes$height <- rep(list(max_height), nrow(nodes))
        }
        if (node_s == "square" | node_s == "roundsquare") {
            ext_factor <- ifelse(node_s == "roundsquare", 1.10, 1.05)
            max_width <- do.call(max, nodes$width)
            max_height <- do.call(max, nodes$height)
            side_dim <- max(max_width, max_height) * ext_factor
            nodes$width <- rep(list(side_dim), nrow(nodes))
            nodes$height <- rep(list(side_dim), nrow(nodes))
        }
        if (node_s == "prop") {
            nodes$min_area <- as.numeric(nodes$width) * as.numeric(nodes$height)
            nodes$dim_adj <- nodes$size / nodes$min_area
            nodes$dim_adj <- nodes$dim_adj / min(nodes$dim_adj)
            for (i in seq_len(nrow(nodes))) {
                a <- max(as.numeric(nodes$width[[i]]), as.numeric(nodes$height[[i]]))
                b <- min(as.numeric(nodes$width[[i]]), as.numeric(nodes$height[[i]]))
                adj <- nodes$dim_adj[i]
                if (adj <= a/b) {
                    beta <- adj
                    alpha <- 1
                } else {
                    beta <- sqrt(adj * a/b)
                    alpha <- sqrt(adj * b/a)
                }
                if (as.numeric(nodes$width[[i]]) >= as.numeric(nodes$height[[i]])) {
                    nodes$width[[i]] <- grid::unit(alpha * as.numeric(nodes$width[[i]]), "native")
                    nodes$height[[i]] <- grid::unit(beta * as.numeric(nodes$height[[i]]), "native")
                } else {
                    nodes$width[[i]] <- grid::unit(beta * as.numeric(nodes$width[[i]]), "native")
                    nodes$height[[i]] <- grid::unit(alpha * as.numeric(nodes$height[[i]]), "native")
                }
            }
        }
    } else {
        # Default node shape
        width <- list(grid::unit(1, "strwidth", "toto"))
        height <- list(grid::unit(1, "strheight", "toto"))
        for (i in seq_len(nrow(nodes))) {
            nodes$shape[i] <- "rect"
            nodes$width[i] <- width
            nodes$height[i] <- height
        }
    }
    # Return
    out <- scene
    out$nodes <- nodes
    return(out)
}

### * (4) sankey_adjust_node_locations()

#' Adjust node location
#' 
#' @param scene Scene list.
#' @param topo Topology.
#' @param nodes Node tibble.
#' @param flows Flows tibble.
#' @param layout Layout string.
#' 
#' @examples
#' sankey_place_nodes <- isotracer:::sankey_place_nodes
#' sankey_place_edge_sockets_on_nodes <- isotracer:::sankey_place_edge_sockets_on_nodes
#' sankey_calc_node_shape <- isotracer:::sankey_calc_node_shape
#' sankey_adjust_node_locations <- isotracer:::sankey_adjust_node_locations
#' 
#' topo <- topo(trini_mod)
#' flows <- flows_from_topo(topo)
#' flows$width <- 1
#' layout <- "left2right"
#' scene <- sankey_place_nodes(topo, flows = flows, layout = layout)
#' scene <- sankey_place_edge_sockets_on_nodes(scene, topo, flows = flows, layout = layout)
#' scene <- sankey_calc_node_shape(scene, topo, flows = flows, layout = layout)
#' z <- sankey_adjust_node_locations(scene, topo, flows = flows, layout = layout)
#' 
#' @keywords internal
#' @noRd

sankey_adjust_node_locations <- function(scene, topo, nodes = NULL, flows, layout) {
    # Layout left2right
    if (layout == "left2right") {
        nodes <- scene$nodes
        nodes <- nodes[order(nodes$x, nodes$y), ]
        ylim <- range(nodes$y)
        x_locs <- sort(unique(nodes$x))
        # Fix collision
        for (xi in x_locs) {
            ni <- nodes[nodes$x == xi, ]
            previous_mean_y <- mean(ni$y)
            total_height <- sum(as.numeric(unlist(ni$height)))
            available_height <- diff(ylim) - total_height
            min_spacer <- min(0.08, 0.1 * total_height, available_height / (nrow(ni) + 1))
            if (nrow(ni) > 1) {
                for (i in 2:nrow(ni)) {
                    top_a <- ni$y[i-1] + as.numeric(ni$height[i-1]) / 2
                    bottom_b <- ni$y[i] - as.numeric(ni$height[i]) / 2
                    if (bottom_b - top_a < min_spacer) {
                        # Collision or too close, shift all remaining nodes upwards
                        shift <- top_a - bottom_b + min_spacer
                        ni$y[i] <- ni$y[i] + shift
                    }
                }
            }
            new_mean_y <- mean(nodes$y)
            ni$y <- ni$y - new_mean_y + previous_mean_y
            nodes[nodes$x == xi, ] <- ni
        }
        # Quick minimization
        for (xi in x_locs) {
            comps <- nodes$comp[nodes$x == xi]
            sources <- colnames(topo)[apply(topo[comps,, drop = FALSE], 2, sum) > 0]
            if (length(sources) > 0) {
                mean_y_self <- mean(nodes$y[nodes$x == xi])
                mean_y_sources <- mean(nodes$y[nodes$comp %in% sources])
                nodes$y[nodes$x == xi] <- nodes$y[nodes$x == xi] - mean_y_self + mean_y_sources
            }
        }
        # Adjust node location so that none is outside the canvas
        for (xi in x_locs) {
            ni <- nodes[nodes$x == xi, ]
            if (nrow(ni) > 1) {
                previous_mean_y <- mean(ni$y)
                total_height <- sum(as.numeric(unlist(ni$height)))
                available_height <- diff(ylim) - total_height
                min_spacer <- min(0.08, 0.1 * total_height, available_height / (nrow(ni) + 1))
                lowest <- ni$y[1] - as.numeric(ni$height[1]) / 2
                if (lowest < ylim[1]) {
                    extra <- ylim[1] - lowest
                    total_spacing <- (ni$y[nrow(ni)] - as.numeric(ni$height[nrow(ni)]) / 2) - lowest
                    if (total_spacing > extra) {
                        # The extra height can be accommodated in the spacing
                        spacing_adj_factor <- 1 - extra / total_spacing
                        # Apply this factor on the nodes from top to bottom
                        for (i in (nrow(ni)-1):1) {
                            current_spacing <- (ni$y[i+1] - as.numeric(ni$height[i+1]) / 2 -
                                                ni$y[i] + as.numeric(ni$height[i]) / 2)
                            shift <- current_spacing * (1 - spacing_adj_factor)
                            for (j in i:1) {
                                ni$y[j] <- ni$y[j] + shift
                            }
                        }
                    }
                }
                highest <- ni$y[nrow(ni)] + as.numeric(ni$height[nrow(ni)]) / 2
                if (highest > ylim[2]) {
                    extra <- highest - ylim[2]
                    total_spacing <- highest - ni$y[1] - as.numeric(ni$height[1]) / 2
                    if (total_spacing > extra) {
                        # The extra height can be accommodated in the spacing
                        spacing_adj_factor <- 1 - extra / total_spacing
                        # Apply this factor on the nodes from bottom to top
                        for (i in 2:nrow(ni)) {
                            current_spacing <- (ni$y[i] - as.numeric(ni$height[i]) / 2 -
                                                ni$y[i-1] + as.numeric(ni$height[i-1]) / 2)
                            shift <- current_spacing * (1 - spacing_adj_factor)
                            for (j in i:nrow(ni)) {
                                ni$y[j] <- ni$y[j] - shift
                            }
                        }
                    }
                }
            }
            nodes[nodes$x == xi, ] <- ni
        }
        out <- scene
        out$nodes <- nodes
    } else {
        # Default is to do nothing
        out <- scene
    }
    return(out)
}

### * (5) sankey_adjust_edge_sockets()

#' Adjust edge sockets relative location on nodes
#'
#' @param scene Scene list.
#' @param topo Topology.
#' @param nodes Node tibble.
#' @param flows Flows tibble.
#' @param layout Layout string.
#' 
#' @keywords internal
#' @noRd

sankey_adjust_edge_sockets <- function(scene, topo, nodes = NULL, flows, layout) {
    nodes <- scene$nodes
    edges <- scene$edges
    edge_sockets <- scene$edge_sockets
    # Layout left2right
    if (layout == "left2right") {
        for (i in seq_len(nrow(edge_sockets))) {
            # Slide sockets so that each node face has its group of sockets centered
            node_i <- which(nodes$comp == edge_sockets$node_id[i])
            if (edge_sockets$edge_end[i] == "from") {
                shift <- nodes$right_accumulator[node_i] / 2
            } else {
                shift <- nodes$left_accumulator[node_i] / 2
            }
            edge_sockets$rel_loc[i] <- edge_sockets$rel_loc[i] - shift
        }
        out <- scene
        out[["edge_sockets"]] <- edge_sockets
    } else if (layout %in% c("stress", "kk", "lgl", "fr", "dh", "mds")) {
        for (i in seq_len(nrow(edge_sockets))) {
            # Slide sockets so that each node face has its group of sockets centered
            node_i <- which(nodes$comp == edge_sockets$node_id[i])
            side <- edge_sockets$node_side[i]
            if (side == "left") {
                shift <- nodes$left_accumulator[node_i] / 2
            } else if (side == "top") {
                shift <- nodes$top_accumulator[node_i] / 2
            } else if (side == "right") {
                shift <- nodes$right_accumulator[node_i] / 2
            } else {
                shift <- nodes$bottom_accumulator[node_i] / 2
            }
            edge_sockets$rel_loc[i] <- edge_sockets$rel_loc[i] - shift
        }
        out <- scene
        out[["edge_sockets"]] <- edge_sockets
    } else {
        # Default is to do nothing
        out <- scene
    }
    # Return
    return(out)
}

### * (6) sankey_calc_edge_socket_coordinates()

#' Calculate absolute coordinates of edge sockets
#'
#' This function calculates the absolute (x, y) coordinates of the center of
#' each edge socket, as well as the normal vector of each socket (which is
#' actually the normal vector of the receiving node face).
#' 
#' @param scene Scene list.
#' @param topo Topology.
#' @param nodes Node tibble.
#' @param flows Flows tibble.
#' @param layout Layout string.
#' 
#' @examples
#' sankey_place_nodes <- isotracer:::sankey_place_nodes
#' sankey_place_edge_sockets_on_nodes <- isotracer:::sankey_place_edge_sockets_on_nodes
#' sankey_calc_node_shape <- isotracer:::sankey_calc_node_shape
#' sankey_adjust_edge_sockets <- isotracer:::sankey_adjust_edge_sockets
#' sankey_calc_edge_socket_coordinates <- isotracer:::sankey_calc_edge_socket_coordinates
#'
#' topo <- topo(trini_mod)
#' nodes <- nodes_from_topo(topo)
#' nodes$size <- 1
#' flows <- flows_from_topo(topo)
#' flows$width <- 1
#' layout <- "left2right"
#' scene <- sankey_place_nodes(topo, nodes = nodes, flows = flows, layout = layout)
#' scene <- sankey_place_edge_sockets_on_nodes(scene, topo, flows = flows, layout = layout)
#' scene <- sankey_calc_node_shape(scene, topo, flows = flows, layout = layout)
#' scene <- sankey_adjust_edge_sockets(scene, topo, flows = flows, layout = layout)
#' z <- sankey_calc_edge_socket_coordinates(scene, topo, flows = flows, layout = layout)
#'
#' @keywords internal
#' @noRd

sankey_calc_edge_socket_coordinates <- function(scene, topo, nodes = NULL, flows, layout) {
    nodes <- scene$nodes
    sockets <- scene$edge_sockets
    sockets$center_x <- NA
    sockets$center_y <- NA
    sockets$normal <- rep(list(NA), nrow(sockets))
    # Process the sockets
    for (i in seq_len(nrow(sockets))) {
        side <- sockets$node_side[i]
        ni <- which(nodes$comp == sockets$node_id[i])
        nw <- as.numeric(grid::convertWidth(nodes$width[[ni]], unitTo = "native"))
        nh <- as.numeric(grid::convertHeight(nodes$height[[ni]], unitTo = "native"))
        stopifnot(side %in% c("center", "left", "right", "top", "bottom"))
        if (side == "center") {
            sockets$center_x[i] <- nodes$x[ni]
            sockets$center_y[i] <- nodes$y[ni]
            sockets$normal[[i]] <- c(0, 0)
        } else if (side %in% c("left", "right")) {
            sockets$center_x[i] <- nodes$x[ni]
            sockets$center_y[i] <- (nodes$y[ni] +
                                    sockets$rel_loc[i])
            if (side == "left") {
                sockets$normal[[i]] <- c(-1, 0)
                sockets$center_x[i] <- sockets$center_x[i] - nw / 2
            } else {
                sockets$normal[[i]] <- c(1, 0)
                sockets$center_x[i] <- sockets$center_x[i] + nw / 2
            }
        } else if (side %in% c("top", "bottom")) {
            sockets$center_x[i] <- (nodes$x[ni] +
                                    sockets$rel_loc[i])
            sockets$center_y[i] <- nodes$y[ni]
            if (side == "top") {
                sockets$normal[[i]] <- c(0, 1)
                sockets$center_y[i] <- sockets$center_y[i] + nh / 2
            } else {
                sockets$normal[[i]] <- c(0, -1)
                sockets$center_y[i] <- sockets$center_y[i] - nh / 2
            }
        }
    }
    # Return
    out <- scene
    out[["edge_sockets"]] <- sockets
    return(out)
}

### * (7) sankey_place_edge_backbones()

#' Calculate absolute coordinates of edge backbones
#' 
#' @param scene Scene list.
#' @param topo Topology.
#' @param nodes Node tibble.
#' @param flows Flows tibble.
#' @param layout Layout string.
#' @param n Integer, number of steps used for Bézier curve interpolation.
#' 
#' @examples
#' sankey_place_nodes <- isotracer:::sankey_place_nodes
#' sankey_place_edge_sockets_on_nodes <- isotracer:::sankey_place_edge_sockets_on_nodes
#' sankey_calc_node_shape <- isotracer:::sankey_calc_node_shape
#' sankey_calc_edge_socket_coordinates <- isotracer:::sankey_calc_edge_socket_coordinates
#' sankey_place_edge_backbones <- isotracer:::sankey_place_edge_backbones
#'
#' t <- topo(trini_mod)
#' nodes <- tibble::tibble(comp = colnames(t), size = 1, col = "red")
#' flows <- flows_from_topo(t)
#' flows$width <- 1
#' layout <- "left2right"
#' scene <- sankey_place_nodes(t, nodes = nodes, flows = flows, layout = layout)
#' scene <- sankey_place_edge_sockets_on_nodes(scene, t, flows = flows, layout = layout)
#' scene <- sankey_calc_node_shape(scene, t, flows = flows, layout = layout)
#' scene <- sankey_calc_edge_socket_coordinates(scene, t, flows = flows, layout = layout)
#' z <- sankey_place_edge_backbones(scene, t, flows = flows, layout = layout)
#' 
#' @keywords internal
#' @noRd

sankey_place_edge_backbones <- function(scene, topo, nodes = NULL, flows, layout,
                                        n = 32) {
    nodes <- scene$nodes
    edges <- scene$edges
    edges$backbone <- rep(list(NA), nrow(edges))
    edges$control_points <- rep(list(NA), nrow(edges))
    edge_sockets <- scene$edge_sockets
    # Default
    for (i in seq_len(nrow(edges))) {
        edge_id <- c(from = edges$from[i], to = edges$to[i])
        sockets <- edge_sockets[sapply(edge_sockets$edge_id, function(x) identical(edge_id, x)), ]
        s_from <- as.list(sockets[sockets$edge_end == "from", ])
        s_to <- as.list(sockets[sockets$edge_end == "to", ])
        xy_start <- c(s_from$center_x, s_from$center_y)
        xy_end <- c(s_to$center_x, s_to$center_y)
        if (identical(s_from$normal[[1]], c(0, 0))) {
            v <- xy_end - xy_start
            v <- v / sqrt(sum(v^2))
            s_from$normal[[1]] <- v
        }
        if (identical(s_to$normal[[1]], c(0, 0))) {
            v <- xy_start - xy_end
            v <- v / sqrt(sum(v^2))
            s_to$normal[[1]] <- v
        }
        if (layout %in% c("left2right")) {
            len_straight <- sqrt(sum((xy_end[1] - xy_start[1])^2))
            xy_start2 <- xy_start + c(s_from$normal[[1]][1], 0) * len_straight * 0.15
            xy_end2 <- xy_end + c(s_to$normal[[1]][1], 0) * len_straight * 0.15
            xy_start3 <- xy_start + c(s_from$normal[[1]][1], 0) * len_straight * 0.3
            xy_end3 <- xy_end + c(s_to$normal[[1]][1], 0) * len_straight * 0.3
            bb <- cbind(xy_start, xy_start2, xy_start3,
                        xy_end3, xy_end2, xy_end)
        } else if (layout %in% c("stress", "kk", "lgl", "fr", "dh", "mds")) {
            len_straight <- sqrt(sum((xy_end - xy_start)^2))
            xy_start2 <- xy_start + s_from$normal[[1]] * len_straight * 0.15
            xy_end2 <- xy_end + s_to$normal[[1]] * len_straight * 0.15
            xy_start3 <- xy_start + s_from$normal[[1]] * len_straight * 0.3
            xy_end3 <- xy_end + s_to$normal[[1]] * len_straight * 0.3
            bb <- cbind(xy_start, xy_start2, xy_start3,
                        xy_end3, xy_end2, xy_end)
        } else {
            len_straight <- sqrt(sum((xy_end - xy_start)^2))
            xy_start2 <- xy_start + s_from$normal[[1]] * len_straight * 0.2
            xy_end2 <- xy_end + s_to$normal[[1]] * len_straight * 0.2
            bb <- cbind(xy_start, xy_start2, xy_end2, xy_end)
        }
        edges$control_points[[i]] <- t(bb)
        edges$backbone[[i]] <- bezierCurve(x = bb[1, ], y = bb[2, ], n = n)
    }
    # Return
    out <- scene
    out[["edges"]] <- edges
    return(out)
}

### * (8) sankey_place_labels()

#' Place node labels
#'
#' @param scene Scene list.
#' @param topo Topology.
#' @param nodes Node tibble.
#' @param flows Flows tibble.
#' @param layout Layout string.
#' @param cex_lab Expansion factor for label size.
#' 
#' @examples
#' sankey_place_nodes <- isotracer:::sankey_place_nodes
#' sankey_place_edge_sockets_on_nodes <- isotracer:::sankey_place_edge_sockets_on_nodes
#' sankey_calc_node_shape <- isotracer:::sankey_calc_node_shape
#' sankey_calc_edge_socket_coordinates <- isotracer:::sankey_calc_edge_socket_coordinates
#' sankey_place_edge_backbones <- isotracer:::sankey_place_edge_backbones
#' sankey_place_labels <- isotracer:::sankey_place_labels
#'
#' t <- topo(trini_mod)
#' nodes <- tibble::tibble(comp = colnames(t), size = 1, col = "red")
#' flows <- flows_from_topo(t)
#' flows$width <- 1
#' layout <- "left2right"
#' scene <- sankey_place_nodes(t, nodes = nodes, flows = flows, layout = layout)
#' scene <- sankey_place_edge_sockets_on_nodes(scene, t, flows = flows, layout = layout)
#' scene <- sankey_calc_node_shape(scene, t, flows = flows, layout = layout)
#' scene <- sankey_calc_edge_socket_coordinates(scene, t, flows = flows, layout = layout)
#' scene <- sankey_place_edge_backbones(scene, t, flows = flows, layout = layout)
#' z <- sankey_place_labels(scene, t, flows = flows, layout = layout)
#' 
#' @keywords internal
#' @noRd

sankey_place_labels <- function(scene, topo, nodes, flows, layout, cex_lab = 1) {
    nodes <- scene$nodes
    labels <- nodes[, c("comp", "label", "x", "y", "width", "height")]
    names(labels) <- c("comp", "label", "node_x", "node_y", "node_width", "node_height")
    # Convert node dimensions to native units
    labels$node_width <- sapply(labels$node_width, function(x) {
        grid::convertWidth(x, unitTo = "native", valueOnly = TRUE)
    })
    labels$node_height <- sapply(labels$node_height, function(x) {
        grid::convertHeight(x, unitTo = "native", valueOnly = TRUE)
    })
    # Calculate label coordinates
    labels <- tibble::add_column(labels, cex = cex_lab, label_x = NA, label_y = NA)
    for (i in seq_len(nrow(labels))) {
        node_bottom <- labels$node_y[i] - labels$node_height[i]/2
        labels$label_x[i] <- labels$node_x[i]
        labels$label_y[i] <- (node_bottom -
                              grid::convertHeight(grid::unit(0.5, "strheight", "X"),
                                                  unitTo = "native", valueOnly = TRUE) -
                              grid::convertHeight(grid::unit(cex_lab, "strheight", "X"),
                                                  unitTo = "native", valueOnly = TRUE) / 2)
    }
    # Return
    out <- scene
    out[["labels"]] <- labels
    return(out)
}

### * sankey_stress_quadrant()

#' From an absolute angle, calculate the quadrant and relative angle in this quadrant
#'
#' In the "stress" layout, each node has four quadrants defined from its (x, y)
#' centre: 1 = east (-pi/4, pi/4), 2 = north (pi/4, 3pi/4), 3 = west (3pi/4,
#' 5pi/4), and 4 = south (-3pi/4, -pi/4).
#'
#' This function takes (y, x) in the same way as \code{\link{atan2}}.
#'
#' @param y,x Same as for \code{\link{atan2}}. From atan2 help: "The arc-tangent
#'     of two arguments atan2(y, x) returns the angle between the x-axis and
#'     the vector from the origin to (x, y), i.e., for positive arguments
#'     atan2(y, x) == atan(y/x)."
#'
#' @return A vector with two elements: the quadrant and the relative
#'     anti-clockwise angle from this quadrant origin.
#'
#' @examples
#' sankey_stress_quadrant <- isotracer:::sankey_stress_quadrant
#' 
#' theta <- seq(- pi, pi, length.out = 128)
#' x <- cos(theta)
#' y <- sin(theta)
#' q <- sapply(seq_along(x), function(i) sankey_stress_quadrant(y[i], x[i]))
#' plot(theta, q[2,], col = c("blue", "green", "purple", "red")[q[1,]],
#'      pch = 19)
#' 
#' @keywords internal
#' @noRd

sankey_stress_quadrant <- function(y, x) {
    angle <- as.vector(atan2(y, x))
    stopifnot(angle <= pi & angle >= -pi)
    if (angle >= -pi/4 & angle <= pi/4) {
        # east
        q <- 1
        a <- angle + pi/4
    } else if (angle >= pi/4 & angle <= 3*pi/4) {
        # north
        q <- 2
        a <- angle - pi/4
    } else if (angle >= 3*pi/4 | angle <= -3*pi/4 ) {
        # west
        q <- 3
        if (angle >= 3*pi/4) {
            a <- angle - 3*pi/4
        } else {
            angle <- angle + 2 * pi
            a <- angle - 3*pi/4
        }
    } else {
        # south
        q <- 4
        a <- angle + 3*pi/4
    }
    return(c(q = q, a = a))
}

### * sankey_stress_socket_imbalance()

#' Calculate an imbalance score for socket distribution around a node
#'
#' @param node_sockets A tibble containing the (at most 4) rows describing the
#'     sockets related to one given node.
#'
#' @return The variance of the total socket width per node side (or NA if the
#'     input has zero row). This can be used as an imbalance score to minimize
#'     when optimizing the distribution of sockets around a node.
#'
#' @importFrom stats aggregate
#' @importFrom stats var
#' 
#' @examples
#' sankey_stress_socket_imbalance <- isotracer:::sankey_stress_socket_imbalance
#' 
#' z <- structure(list(node_id = c("arg", "arg"), node_side = c("bottom", "top"),
#'        edge_id = list(c(from = "petro", to = "arg"), c(from = "tricor", to = "arg")),
#'        edge_id_char = c("petro->arg", "tricor->arg"),
#'        recip_edge_id = c("arg->petro", "arg->tricor"), edge_end = c("to", "to"),
#'        rel_quadrant_angle = c(0.00537724841543286, 1.26282578153344), width = c(1, 1),
#'        rel_loc = c(0.5, -0.5)), row.names = c(NA, -2L),
#'        class = c("tbl_df", "tbl", "data.frame"))
#' sankey_stress_socket_imbalance(z)
#' 
#' @keywords internal
#' @noRd

sankey_stress_socket_imbalance <- function(node_sockets) {
    if (nrow(node_sockets) > 0) {
        width_per_side <- tibble::deframe(aggregate(width ~ node_side,
                                                    data = node_sockets,
                                                    FUN = sum))
        width_per_side <- c(width_per_side, rep(0, 4 - length(width_per_side)))
        imbalance <- var(width_per_side)
    } else {
        imbalance <- NA
    }
    return(imbalance)
}

### * sankey_stress_socket_moves()

#' Determine all the possible socket arrangements one move away from input
#'
#' @param node_sockets A tibble containing the (at most 4) rows describing the
#'     sockets related to one given node.
#'
#' @keywords internal
#' @noRd

sankey_stress_socket_moves <- function(node_sockets) {
    if (nrow(node_sockets) == 1) {
        return(list())
    }
    moves <- list()
    moves_i <- 1
    ref <- node_sockets
    cclock_move <- c("top" = "left", "left" = "bottom", "bottom" = "right",
                     "right" = "top")
    clock_move <- c("bottom" = "left", "left" = "top", "top" = "right",
                    "right" = "bottom")
    for (side in unique(ref$node_side)) {
        side_sockets <- ref[ref$node_side == side, ]
        side_sockets <- side_sockets[order(side_sockets$rel_quadrant_angle), ]
        if (nrow(side_sockets) > 1) {
            if (side_sockets$rel_quadrant_angle[1] <= pi/4) {
                # Clockwise move
                new_side <- clock_move[side]
                if (!any(side_sockets$node_side == new_side)) {
                    new_angle <- pi/2 - 0.01
                } else {
                    new_angle <- pi/2 - (pi/2 - max(side_sockets$rel_quadrant_angle[side_sockets$node_side == new_side])) / 2
                }
                # Update
                side_sockets$node_side[1] <- new_side
                side_sockets$rel_quadrant_angle[1] <- new_angle
                z <- ref
                z[z$node_side == side, ] <- side_sockets
                moves[[moves_i]] <- z
                moves_i <- moves_i + 1
            }
            side_sockets <- ref[ref$node_side == side, ]
            side_sockets <- side_sockets[order(side_sockets$rel_quadrant_angle), ]
            if (side_sockets$rel_quadrant_angle[nrow(side_sockets)] > pi/4) {
                # Counter-clockwise move
                new_side <- cclock_move[side]
                if (!any(side_sockets$node_side == new_side)) {
                    new_angle <- 0.01
                } else {
                    new_angle <- min(side_sockets$rel_quadrant_angle[side_sockets$node_side == new_side]) / 2
                }
                # Update
                side_sockets$node_side[nrow(side_sockets)] <- new_side
                side_sockets$rel_quadrant_angle[nrow(side_sockets)] <- new_angle
                z <- ref
                z[z$node_side == side, ] <- side_sockets
                moves[[moves_i]] <- z
                moves_i <- moves_i + 1
            }
        }
    }
    return(moves)
}

### * sankey_get_elements_lims()

#' Return the xlim and ylim to contain all the graphical elements
#'
#' @param scene Scene list.
#'
#' @return A list with "xlim" and "ylim" elements.
#'
#' @keywords internal
#' @noRd

sankey_get_elements_lims <- function(scene) {
    nodes <- scene[["nodes"]]
    nodes_min_x <- min(nodes$x - as.numeric(nodes$width)/2)
    nodes_max_x <- max(nodes$x + as.numeric(nodes$width)/2)
    nodes_min_y <- min(nodes$y - as.numeric(nodes$height)/2)
    nodes_max_y <- max(nodes$y + as.numeric(nodes$height)/2)
    edges <- scene[["edges"]]
    edges$ribbon <- lapply(seq_len(nrow(edges)), function(i) {
        ribbonFromTrajectory(edges[["backbone"]][[i]],
                             width = edges[["width"]][i])
    })
    ribbons <- do.call(rbind, edges$ribbon)
    edges_min_x <- min(ribbons[, 1])
    edges_max_x <- max(ribbons[, 1])
    edges_min_y <- min(ribbons[, 2])
    edges_max_y <- max(ribbons[, 2])
    xlim <- c(min(nodes_min_x, edges_min_x),
              max(nodes_max_x, edges_max_x))
    ylim <- c(min(nodes_min_y, edges_min_y),
              max(nodes_max_y, edges_max_y))
    return(list(xlim = xlim, ylim = ylim))
}

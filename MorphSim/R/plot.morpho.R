#' Plot full evolutionary history
#'
#' @description
#' This function creates a plot showing continuous evolution of discrete traits.
#'
#' @param x A morpho object
#' @param timetree TRUE or FALSE. Indicate whether you want to plot a time
#'   tree or not. Default = FALSE (uses distance tree if FALSE).
#' @param trait The trait number to plot.
#' @param show.fossil Plot the fossil along the tree. Default = FALSE.
#' @param root.edge If TRUE plot the root edge. Default = FALSE.
#' @param reconstructed Plot the reconstructed tree. Default = FALSE.
#' @param show.convergent When convergent = TRUE, transition boxes are highlighted
#' for any state that arose independently more than once during the simulation.
#' This includes transitions that were subsequently reversed on descendant
#' branches
#' @param edge.width Width of the branches.
#' @param label.offset Distance of tip label to tree tips.
#' @param f.cex Size of fossils.
#' @param e.cex Size of extant taxa.
#' @param box.cex Size of traits on plot
#' @param col A vector of colors that should be the same length or longer than
#'   the number of different character states (k). If not specified, the traits
#'   from 0 to 6 can be differentiated.
#' @param col.timescale A single color for the timescale. Default = "darkgrey".
#' @param ... Other arguments to be passed to methods, such as graphical
#'   parameters.
#'
#' @import stats
#' @import graphics
#' @importFrom ape node.depth.edgelength Ntip
#'
#' @return
#' No return value, called for its side effect of producing a plot.
#'
#' @export
#'
#' @examples
#' # simulate a phylogenetic tree
#' data(morpho_data)
#' plot(morpho_data, trait = 4, timetree = FALSE, show.fossil = FALSE,
#'      root.edge = FALSE, reconstructed = FALSE)
#'
plot.morpho <- function(x = NULL,
                        trait = 1,
                        timetree = TRUE,
                        show.fossil = TRUE,
                        reconstructed = TRUE,
                        show.convergent = TRUE,
                        root.edge = FALSE,
                        edge.width = 1,
                        label.offset = 0.01,
                        e.cex = 0.5,
                        f.cex = 1,
                        box.cex = 4,
                        col = c("#fdfdfd", "lightgray", "lightblue", "pink",
                                "yellow", "green", "orange"),
                        col.timescale = "darkgrey",
                        ...) {



  if (is.null(x)) {
    stop("Error: 'x' cannot be NULL. Please provide a morpho object.")
  }
  if (!is.morpho(x) || is.null(x$trees)) {
    stop("Error: 'x' must be a morpho object containing a 'trees' element.")
  }

  # Fall back to simpler defaults if data not available
  if (timetree && is.null(x$trees$TimeTree)) {
    message("Note: no time tree available, using distance tree")
    timetree <- FALSE
  }
  if (show.fossil && is.null(x$fossil)) {
    message("Note: no fossil data available, fossils will not be shown")
    show.fossil <- FALSE
  }
  if (reconstructed && is.null(x$fossil)) {
    message("Note: no fossil data available, reconstructed tree will not be shown")
    reconstructed <- FALSE
  }

  if (!is.null(trait) && (!is.numeric(trait) || length(trait) != 1)) {
    stop("Error: 'trait' must be a single numeric value or NULL.")
  }
  if (!is.null(trait) && trait > length(x$transition_history)) {
    stop(paste0("Error: 'trait' index (", trait, ") is out of range. The object has only ",
                length(x$transition_history), " traits."))
  }

  if (!is.logical(timetree) || length(timetree) != 1) {
    stop("Error: 'timetree' must be TRUE or FALSE.")
  }
  if (!is.logical(show.fossil) || length(show.fossil) != 1) {
    stop("Error: 'fossil' must be TRUE or FALSE.")
  }
  if (!is.logical(root.edge) || length(root.edge) != 1) {
    stop("Error: 'root.edge' must be TRUE or FALSE.")
  }
  if (!is.logical(reconstructed) || length(reconstructed) != 1) {
    stop("Error: 'reconstructed' must be TRUE or FALSE.")
  }

  if (show.fossil && is.null(x$fossil)) {
    stop("Error: No fossil data found in 'x', but 'fossil = TRUE' was requested.")
  }

  data <- x
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mar = c(5, 3, 2.5, 2))

  # choose which tree to plot

  tree_to_plot <- if (timetree) data$trees$TimeTree else data$trees$EvolTree

  label.offset = max(tree_to_plot$edge.length) * 0.02

  # Get branch colours (reconstructed vs. default)

  if (reconstructed) {
    b.cols <- reconstruct.tree(data)
    edge_col <- b.cols[[1]]
  } else {
    edge_col <- "black"
  }

  # plot

  plot(
    tree_to_plot,
    edge.width = edge.width,
    label.offset = label.offset,
    root.edge = root.edge,
    edge.color = edge_col
  )

  # get the plot information

  tree_plot_info <- get("last_plot.phylo", envir = ape::.PlotPhyloEnv)

  edge_start_x <- tree_plot_info$xx[data$trees$EvolTree$edge[, 1]]
  edge_end_x <- tree_plot_info$xx[data$trees$EvolTree$edge[, 2]]
  yy <- tree_plot_info$yy

  edge <- data$trees$EvolTree$edge
  parent <- as.integer(edge[, 1])
  child <- as.integer(edge[, 2])

  # Identify the root

  root <- as.integer(parent[!match(parent, child, 0)][1])

  if (!is.null(trait)) {
    df <- data$transition_history[trait][[1]]

    # identify convergent transitions
    conv_states <- c()
    conv_root <- FALSE
    if (show.convergent) {
      # count how many times each state arose from the transition history
      state_origins <- table(df$state)
      # root is also an origin of its state
      root_st <- as.character(data$root.states[trait])
      if (root_st %in% names(state_origins)) {
        state_origins[root_st] <- state_origins[root_st] + 1
      } else {
        state_origins[root_st] <- 1
      }
      conv_states <- names(state_origins[state_origins > 1])
      conv_root <- root_st %in% conv_states
    }

    root_border <- if (show.convergent && conv_root) "#4292C6" else "black"


    if (root.edge) {
      points(
        data$trees$TimeTree$root.edge, yy[root],
        pch = 22, col = root_border,
        bg = col[as.numeric(data$root.states[trait]) + 1],
        cex = 4
      )
      text(
        data$trees$TimeTree$root.edge, yy[root],
        label = as.numeric(data$root.states[trait])
      )
    } else {
      points(
        0, yy[root], pch = 22, col = root_border,
        bg = col[as.numeric(data$root.states[trait]) + 1],
        cex = box.cex
      )
      text(0, yy[root], label = as.numeric(data$root.states[trait]), cex = box.cex / 4)
    }

    if (nrow(df) > 0) {
      for (i in 1:nrow(df)) {
        branch <- as.numeric(df$edge[i])
        if (timetree) {
          position <- as.numeric(df$hmin[i]) / data$trees$BrRates[branch]
        } else {
          position <- as.numeric(df$hmin[i])
        }
        point_x <- edge_start_x[branch] + position
        if (timetree) {
          point_y <- yy[data$trees$TimeTree[["edge"]][branch, 2]]
        } else {
          point_y <- yy[data$trees$EvolTree[["edge"]][branch, 2]]
        }
        paint <- as.numeric(df$state[i]) + 1

        border_col <- if (show.convergent && as.character(df$state[i]) %in% conv_states) "#4292C6" else "black"

        points(point_x, point_y, pch = 22, col = border_col, bg = col[paint], cex = box.cex)
        text(point_x, point_y, labels = as.numeric(df$state[i]), cex = box.cex / 4)
      }
    }
  }

  if (timetree) {
    tree.age <- max(ape::node.depth.edgelength(data$trees$TimeTree))
  }

  # colour branches (if reconstructed)

  if (reconstructed) {
    if (length(b.cols) == 2) {
      for (p in 1:length(b.cols[[2]])) {
        q <- which(data$fossil$ape.branch == b.cols[[2]][p])
        if (length(q) > 1) {
          fossil_pos <- data$fossil$hmin[which(data$fossil$hmin == min(data$fossil$hmin[q]))]
        } else {
          fossil_pos <- data$fossil$hmin[q]
        }

        tree.age <- max(ape::node.depth.edgelength(data$trees$TimeTree))
        branch <- data$fossil$ape.branch[q]

        time <- fossil_pos
        if (root.edge) {
          actual_position <- tree.age - time + data$trees$TimeTree$root.edge
        } else {
          actual_position <- tree.age - time
        }
        position <- actual_position / data$tree$TimeTree$edge.length[branch]
        point_x <- position * (edge_end_x[branch] - edge_start_x[branch])
        y_vals <- yy[data$trees$TimeTree[["edge"]][b.cols[[2]][p], 2]]
        segments(point_x, y_vals, edge_end_x[b.cols[[2]][p]], y_vals, col = "white")
        segments(point_x, y_vals, edge_end_x[b.cols[[2]][p]], y_vals, col = "grey")
      }
    }
  }

  # add fossils

  if (show.fossil) {
    tree.age <- max(ape::node.depth.edgelength(data$trees$TimeTree))
    for (fsl in 1:length(data$fossil$sp)) {
      branch <- data$fossil$ape.branch[fsl]
      time <- as.numeric(data$fossil$hmax[fsl])
      if (root.edge) {
        actual_position <- tree.age - time + data$trees$TimeTree$root.edge
      } else {
        actual_position <- tree.age - time
      }
      position <- actual_position / data$trees$TimeTree$edge.length[branch]
      point_x <- position * (edge_end_x[branch] - edge_start_x[branch])
      point_y <- yy[data$trees$TimeTree[["edge"]][branch, 2]]

      if (data$fossil$hmax[fsl] == 0) {
        points(point_x, point_y, pch = 16, col = "forestgreen", cex = e.cex)
      } else {
        points(point_x, point_y, pch = 18, col = "black", cex = f.cex)
      }
    }
  }

  # Add a timescale below the plot

  if (timetree) {
    if (root.edge) {
      axis_labels <- c((tree.age + data$trees$TimeTree$root.edge), 0)
      axis(
        1, at = c(0, (tree.age + data$trees$TimeTree$root.edge)),
        labels = round(axis_labels, 2),
        line = 1, col = col.timescale, lwd = 3,
        cex.axis = 1.3, col.axis = col.timescale
      )
    } else {
      axis_labels <- c(tree.age, 0)
      axis(
        1, at = c(0, tree.age),
        labels = round(axis_labels, 2),
        line = 1, col = col.timescale, lwd = 3,
        cex.axis = 1.3, col.axis = col.timescale
      )
    }
    mtext("Time before present", side = 1, line = 2.5, cex = 1.3, col = col.timescale)
  }
}

#' Draws a 2D Adaptable Radial Axes (ARA) plot for standardized data
#'
#' @description
#' Creates a plot associated with an Adaptable Radial Axes (ARA) mapping
#'
#' @details
#' The function \code{draw_ara_plot_2d_standardized()} generates a basic
#' two-dimensional plot related to an "Adaptable Radial Axes" (ARA) mapping
#' (M. Rubio-Sánchez, A. Sanchez, and D. J. Lehmann (2017), doi:
#' 10.1111/cgf.13196) for high-dimensional numerical data (\code{X}) that has
#' been previously standardized (\code{Z}). The plot displays a set of 2D points
#' (\code{P}), each representing an observation from the high-dimensional
#' dataset. It also includes a collection of axis vectors (\code{V}), each
#' corresponding to a specific data variable. If the ARA mapping incorporates
#' weights (\code{weights}), these axis vectors are colored accordingly to
#' reflect the weighting. For a user-specified subset of variables
#' (\code{axis_lines}), the function additionally draws axis lines with tick
#' marks that represent values of the selected variables. Users can estimate the
#' values of the high-dimensional data by visually projecting the plotted points
#' orthogonally onto these axes. The plotted points can also be colored
#' according to the values of the variable \code{color_variable}.
#'
#' @param Z
#' Standardized numeric data matrix of dimensions N x n, where N is the number
#' of observations, and n is the number of variables.
#' @param X
#' Original numeric data matrix (before standardizing) of dimensions N x n
#' @param V
#' Numeric matrix of "axis vectors" of dimensions n x 2, where each row of
#' \code{V} defines an axis vector.
#' @param P
#' Numeric data matrix of dimensions N x 2 containing the N 2-dimensional
#' representations of the data observations
#' (i.e., the embedded points).
#' @param weights
#' Numeric array specifying non-negative weights associated with each variable.
#' Can also be a 1D matrix. Default: array of n ones.
#' @param axis_lines
#' Array of integer variable indices (in \[1,n\]) indicating which calibrated
#' axis lines are to be displayed. Default: NULL.
#' @param color_variable
#' Integer (in \[1,n\]) that indicates the variable used to color the embedded
#' points. Default: NULL.
#'
#' @references
#' M. Rubio-Sánchez, A. Sanchez, D. J. Lehmann: Adaptable radial axes plots for
#' improved multivariate data visualization. Computer Graphics Forum 36, 3
#' (2017), 389–399. [doi:10.1111/cgf.13196](https://onlinelibrary.wiley.com/doi/10.1111/cgf.13196)
#'
#' @returns
#' Returns 0 if the function terminates without errors
#'
#' @export
#'
#' @examples
#' # Define subset of (numerical) variables
#' # 1:"mpg", 4:"horsepower", 5:"weight", 6:"acceleration"
#' selected_variables <- c(1, 4, 5, 6)
#' n <- length(selected_variables)
#'
#' # Retain only selected variables and rename dataset as X
#' X <- auto_mpg[, selected_variables] # Select a subset of variables
#'
#' # Remove rows with missing values from X
#' N <- nrow(X)
#' rows_to_delete <- NULL
#' for (i in 1:N) {
#'   if (sum(is.na(X[i, ])) > 0) {
#'     rows_to_delete <- c(rows_to_delete, -i)
#'   }
#' }
#' X <- X[rows_to_delete, ]
#'
#' # Convert X to matrix
#' X <- apply(as.matrix.noquote(X), 2, as.numeric)
#'
#' # Standardize data
#' Z <- scale(X)
#'
#' # Define axis vectors (2-dimensional in this example)
#' r <- c(0.8, 1, 1.2, 1)
#' theta <- c(225, 100, 315, 80) * 2 * pi / 360
#' V <- pracma::zeros(n, 2)
#' for (i in 1:n) {
#'   V[i,1] <- r[i] * cos(theta[i])
#'   V[i,2] <- r[i] * sin(theta[i])
#' }
#'
#' # Define weights
#' weights <- c(1, 0.75, 0.75, 1)
#'
#' # Compute the mapping
#' mapping <- ara_unconstrained_l2(Z, V, weights = weights, solver = "formula")
#'
#' # Select variables with labeled axis lines on ARA plot
#' axis_lines <- c(1, 4) # 1:"mpg", 4:"acceleration")
#'
#' # Select variable used for coloring embedded points
#' color_variable <- 1 # "mpg"
#'
#' # Draw the ARA plot
#' draw_ara_plot_2d_standardized(
#'   Z,
#'   X,
#'   V,
#'   mapping$P,
#'   weights = weights,
#'   axis_lines = axis_lines,
#'   color_variable = color_variable
#' )
#'
#'
draw_ara_plot_2d_standardized <- function(
    Z,
    X,
    V,
    P,
    weights = rep(1, ncol(Z)),
    axis_lines = NULL,
    color_variable = NULL) {
  ###################   Check validity of input parameters   ###################

  # Check data types

  if ((!inherits(Z, "matrix") && !inherits(Z, "array")) || !is.double(Z)) {
    stop("Input error: Z must be a numeric matrix")
  }

  if ((!inherits(X, "matrix") && !inherits(X, "array")) || !is.double(X)) {
    stop("Input error: X must be a numeric matrix")
  }

  if ((!inherits(V, "matrix") && !inherits(V, "array")) || !is.double(V)) {
    stop("Input error: V must be a numeric matrix")
  }

  if ((!inherits(P, "matrix") && !inherits(P, "array")) || !is.double(P)) {
    stop("Input error: P must be a numeric matrix")
  }

  if (!is.double(weights) && !is.integer(weights)) {
    stop("Input error: weights must be a numeric vector")
  }

  if (!is.null(axis_lines) &&
    !is.double(axis_lines) &&
    !is.integer(axis_lines)) {
    stop("Input error: axis_lines must be a numeric value or vector")
  }

  if (!is.null(color_variable) &&
    !is.double(color_variable) &&
    !is.integer(color_variable)) {
    stop("Input error: color_variable must be an integer")
  }

  # Check dimensions of matrices -----------------------------------------------

  NZ <- nrow(Z)
  NX <- nrow(X)
  NP <- nrow(P)

  nZ <- ncol(Z)
  nX <- ncol(X)
  nV <- nrow(V)

  mV <- ncol(V)
  mP <- ncol(P)

  if (NZ != NP) {
    stop("Input error: The number of observations (rows) of Z must match the
         number of embedded points (rows) of P")
  }

  if (NX != NP) {
    stop("Input error: The number of observations (rows) of X must match the
         number of embedded points (rows) of P")
  }

  if (nZ != nV) {
    stop("Input error: The number of variables of Z (columns) must match the
         number of variables of V (rows)")
  }

  if (nX != nV) {
    stop("Input error: The number of variables of X (columns) must match the
         number of variables of V (rows)")
  }
  n <- nV


  if (mV != 2) {
    stop("Input error: The number of columns of V must be 2 (the dimensionality
         of the visualization space)")
  }

  if (mP != 2) {
    stop("Input error: The number of columns of P must be 2 (the dimensionality
         of the visualization space)")
  }


  # Check that inputs have no missing values -----------------------------------

  if (any(is.na(Z))) {
    stop("Input error: Z cannot contain missing values)")
  }

  if (any(is.na(X))) {
    stop("Input error: X cannot contain missing values)")
  }

  if (any(is.na(V))) {
    stop("Input error: V cannot contain missing values)")
  }

  if (any(is.na(P))) {
    stop("Input error: P cannot contain missing values)")
  }

  if (any(is.na(weights))) {
    stop("Input error: weights cannot contain missing values)")
  }


  # Check additional preconditions on input parameters -------------------------

  if ((length(weights) != n) || (min(weights) < 0)) {
    stop("Input error: weights must be vector of length n with non-negative
         values")
  }

  count <- 0 # Count number of integers in [1,n] contained in axis_lines
  while ((count < length(axis_lines)) &&
    ((axis_lines[count + 1] >= 1) &&
      (axis_lines[count + 1] <= n) &&
      (axis_lines[count + 1] %% 1 == 0))) {
    count <- count + 1
  }
  if ((count > n) ||
    (count != length(axis_lines)) ||
    (any(duplicated(axis_lines)))) {
    stop("Input error: axis_lines must contain at most n distinct variable
         (integer) indices")
  }

  if (!is.null(color_variable) &&
    ((color_variable < 1) ||
      (color_variable > n) ||
      (color_variable %% 1 != 0))) {
    stop("Input error: color_variable must be an integer in [1,n]")
  }


  ###########################   Generate ARA plot   ############################

  # Declare local variables
  x <- y <- P1 <- P2 <- V1 <- V2 <- V3 <- V4 <- NULL


  colnames(P) <- c("P1", "P2")
  variable_names <- colnames(X)
  if (!is.null(color_variable)) {
    color_variable_name <- variable_names[color_variable]
  } else {
    color_variable_name <- NULL
  }

  # Original data means and standard deviations
  meanX <- colMeans(X)
  stdX <- apply(X, 2, stats::sd)

  # Initialize plot
  my_plot <- ggplot2::ggplot()

  # Draw optional reference horizontal & vertical lines
  my_plot <- my_plot +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "gray80",
      linewidth = 0.5
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "gray80",
      linewidth = 0.5
    )


  # Draw optional reference circle
  center <- c(0, 0)
  npoints <- 100
  tt <- seq(0, 2 * pi, length.out = npoints)
  xx <- center[1] + cos(tt)
  yy <- center[2] + sin(tt)
  dfC <- data.frame(x = xx, y = yy)
  my_plot <- my_plot +
    ggplot2::geom_path(
      data = dfC,
      ggplot2::aes(x, y),
      linetype = "dotted",
      color = "gray80",
      linewidth = 0.5
    )

  # Set equal axes and empty background
  my_plot <- my_plot + ggplot2::coord_equal() + ggplot2::theme_void()


  # Draw mapped points on a Cartesian plane
  dfZ <- as.data.frame(Z)
  dfX <- as.data.frame(X)
  dfP <- as.data.frame(P)

  if (is.null(color_variable_name)) {
    dfPZ <- cbind(dfP, dfZ)

    my_plot <- my_plot +
      ggplot2::geom_point(
        data = dfPZ,
        mapping = ggplot2::aes(x = P1, y = P2),
        color = "#4488FF",
        size = 2,
        alpha = 1,
        show.legend = FALSE
      )
  } else {
    colorVar <- dfX[, match(color_variable_name, variable_names)]
    dfPZ <- cbind(dfP, dfZ, colorVar)

    my_plot <- my_plot +
      ggplot2::geom_point(
        data = dfPZ,
        mapping = ggplot2::aes(
          x = P1,
          y = P2,
          color = colorVar
        ),
        size = 2,
        alpha = 1,
        show.legend = TRUE
      )

    # Color points using a viridis palette
    my_plot <- my_plot + ggplot2::scale_colour_viridis_c()

    # Add colorbar
    my_plot <- my_plot +
      ggplot2::theme(legend.key.height = grid::unit(2, "cm")) +
      ggplot2::theme(
        legend.title = ggplot2::element_text(size = 16),
        legend.text = ggplot2::element_text(size = 16)
      )

    my_plot <- my_plot + ggplot2::labs(colour = color_variable_name)
  }


  # Add a rectangular frame
  my_plot <- my_plot +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(
        fill = "transparent",
        color = "gray70",
        linewidth = 2
      ),
      legend.position = "right"
    )

  # Set margins
  my_plot <- my_plot +
    ggplot2::theme(plot.margin = ggplot2::margin(0.5, 0.5, 0.5, 0.5, "cm"))



  #################### Add calibrated axes ####################

  if (!is.null(axis_lines)) {
    for (sel_var in axis_lines) {
      # Compute data estimates axis of selected variable (sel_var)
      estimates <- P %*% V[sel_var, ]

      # Draw axis line
      lo <- floor(min(estimates))
      up <- pracma::ceil(max(estimates))

      v <- matrix(unlist(V[sel_var, ]), nrow = 1)
      vs <- v / norm(v, type = "2")^2 # scaled axis vector
      E <- cbind(lo * vs, up * vs) # endpoints
      dfE <- as.data.frame(E)
      my_plot <- my_plot + ggplot2::geom_segment(
        data = dfE,
        mapping = ggplot2::aes(x = V1, y = V2, xend = V3, yend = V4),
        alpha = 0.7,
        color = "gray30",
        linewidth = 1
      )


      # Draw tick marks
      size_tick_mark <- 0.1
      v_perp <- v
      v_perp[1, 1] <- -v[1, 2]
      v_perp[1, 2] <- v[1, 1]
      v_perp <- v_perp / norm(v_perp, type = "2") * size_tick_mark / 2

      Ticks <- matrix(0, up - lo + 1, 6)
      i <- 1
      for (k in lo:up) {
        Ticks[i, ] <- cbind(k * vs + v_perp, k * vs - v_perp, k * vs)
        i <- i + 1
      }
      dfT <- as.data.frame(Ticks)
      my_plot <- my_plot + ggplot2::geom_segment(
        data = dfT,
        mapping = ggplot2::aes(x = V1, y = V2, xend = V3, yend = V4),
        alpha = 0.7,
        color = "gray30",
        linewidth = 1
      )


      # Draw numeric values at tick marks
      TL <- cbind(rep(0, up - lo + 1, 1), Ticks[, 5:6])
      colnames(TL) <- c("ticklabel", "x", "y")
      dfTL <- as.data.frame(TL)

      i <- 1
      for (k in lo:up) {
        dfTL[i, 1] <- sprintf("%.3f", stdX[sel_var] * k + meanX[sel_var])
        i <- i + 1
      }

      if (v_perp[1] < 0) {
        v_perp <- -v_perp
      }
      factor <- 3
      x_offset <- v_perp[1] * factor + 0.25
      y_offset <- v_perp[2] * factor
      my_plot <- my_plot + ggplot2::geom_text(
        data = dfTL,
        mapping = ggplot2::aes(x = x, y = y),
        label = dfTL$ticklabel,
        color = "gray30",
        fontface = "bold",
        nudge_x = x_offset,
        nudge_y = y_offset,
        check_overlap = TRUE
      )
    }
  }


  # Draw axis vectors
  colnames(V) <- c("V1", "V2")
  dfV <- as.data.frame(V)

  max_weight <- max(weights)
  vector_colors <- rep(1, n)
  for (i in 1:n) {
    gray_value <- 1 - weights[i] / max_weight
    vector_colors[i] <- grDevices::rgb(gray_value, gray_value, gray_value)
  }

  dfV <- cbind(dfV, vector_colors)

  my_plot <- my_plot +
    ggplot2::geom_segment(
      data = dfV,
      mapping = ggplot2::aes(
        x = 0,
        y = 0,
        xend = V1,
        yend = V2
      ),
      arrow = ggplot2::arrow(
        angle = 30,
        length = grid::unit(7, "pt"),
        type = "closed",
        ends = "last"
      ),
      lineend = "round",
      linejoin = "mitre",
      alpha = 1,
      color = vector_colors,
      linewidth = 1
    )


  # Draw variable names
  vertical_offset <- 0.2
  horizontal_factor <- 0.1
  V_names_pos <- V
  for (i in 1:n) {
    varname_length <- nchar(variable_names[i])

    if (V[i, 2] <= 0) {
      if (V[i, 1] <= 0) {
        V_names_pos[i, ] <- V_names_pos[i, ] +
          c(-varname_length * horizontal_factor, -vertical_offset)
      } else {
        V_names_pos[i, ] <- V_names_pos[i, ] +
          c(varname_length * horizontal_factor, -vertical_offset)
      }
    } else {
      if (V[i, 1] <= 0) {
        V_names_pos[i, ] <- V_names_pos[i, ] +
          c(-varname_length * horizontal_factor, vertical_offset + 0.1)
      } else {
        V_names_pos[i, ] <- V_names_pos[i, ] +
          c(varname_length * horizontal_factor, vertical_offset + 0.1)
      }
    }
  }

  dfVN <- data.frame(
    varnames = variable_names,
    x = V_names_pos[, 1],
    y = V_names_pos[, 2]
  )
  my_plot <- my_plot +
    ggplot2::geom_text(
      data = dfVN,
      mapping = ggplot2::aes(x = x, y = y),
      label = dfVN$varnames,
      family = "sans",
      fontface = "bold",
      color = "black",
      check_overlap = FALSE,
      size = 6
    )


  # Draw plot
  my_plot

  return(my_plot)
}

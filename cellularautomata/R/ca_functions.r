
#' Create the rule for a specific Wolfram number
#'
#' @param rule the Wolfram rule
#'
#' @return a vector with 8 elements defining the responses to:
#'         (111), (110), (101), (100), (011), (010), (001), (000)
#'         on the previous row
#' @export
#'
#' @examples
#' # get the definition of rule 30
#' wolfram_rule(30)
#'
#' @importFrom utils tail
wolfram_rule <- function(rule) {
  rule |> intToBits() |> as.numeric() |> rev() |> tail(8)
}


#' Plot the definition of a Wolfram rule
#'
#' @param rule integer, the Wolfram rule
#'
#' @return a ggplot object defining the rule
#' @export
#'
#' @examples
#' wolfram_rule_def(30)
#'
#' @importFrom purrr map
#' @importFrom patchwork wrap_plots
wolfram_rule_def <- function(rule) {

  list(
    c(1,1,1),
    c(1,1,0),
    c(1,0,1),
    c(1,0,0),
    c(0,1,1),
    c(0,1,0),
    c(0,0,1),
    c(0,0,0)
  ) |>
  purrr::map(\(r){
    ca(rule, initialstate = r, steps = 2, wrap = FALSE) |>
      plot(title = NULL) +
      theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
  }) |>
  patchwork::wrap_plots(nrow = 1, heights = 1)

}

#' Create Cellular Automaton
#'
#' @param wolframrule integer identifying the algorithm according to Wolfram numbering
#' @param initialstate a vector setting up the initial state
#' @param steps integer spacifying for how long to run the algorithm
#' @param ncols how many columns to have. If `initialstate` is specified, `ncols` is
#'        calculated as `length(initialstate)`. If `initialstate` is not specified,
#'        it is defined as a 1 in the middle of zeros. For instance, with the
#'        default `ncols = 11`, the `initialstate` is a vector of 5 zeros, 1,
#'        and another 5 zeros.
#' @param wrap boolean, default is TRUE. Whether it uses a circular wrap at the
#'        end and beginning of lines. If FALSE it puts empty slots on the first
#'        and last columns.
#'
#' @return an object of class `c("cellular_automaton", "matrix")`
#'
#' @author Adapted from code by Nicola Procopio
#'
#' @references <https://en.wikipedia.org/wiki/Cellular_automaton>
#'
#' @export
#'
#' @examples
#' # Wolfram's rule 30
#' ca(30)
#'
#' # Wolfram's rule 126 with a random initial state
#' ca(126,
#'    initialstate = sample(c(0, 1), size = 100, replace = TRUE),
#'    steps = 100)
#'
ca <- function(
    wolframrule,
    initialstate,
    steps = 100,
    ncols = 101,
    wrap = TRUE) {

  if (missing(initialstate)) {
    left  <- floor(ncols/2)
    right <- ncols - 1 - left
    initialstate <- c(rep(0, left), 1, rep(0, right))
  }
  nrows <- steps
  ncols <- length(initialstate)

  A      <- matrix(0, nrows, ncols)
  A[1, ] <- initialstate

  ru <- wolfram_rule(wolframrule)

  # run the algorithm
  for (i in 2:nrows) {
    for (j in 1:ncols) {

      # find what's above in the middle
      m <- A[i - 1, j]

      # find what's above on the right, and wrap line to the beginning
      if (j == ncols) { r <- A[i - 1, 1] } else { r <- A[i - 1, j + 1] }

      # find what's above on the left, and wrap line to the end
      if (j == 1) { l <- A[i - 1, ncols] } else { l <- A[i - 1, j - 1] }

      # use the wolfram rule
      if (isTRUE(
        ( l &  m &  r & ru[1]) |
        ( l &  m & !r & ru[2]) |
        ( l & !m &  r & ru[3]) |
        ( l & !m & !r & ru[4]) |
        (!l &  m &  r & ru[5]) |
        (!l &  m & !r & ru[6]) |
        (!l & !m &  r & ru[7]) |
        (!l & !m & !r & ru[8]))) {
        A[i, j] = 1
      }

      if (!wrap) {
        A[i,     1] <- 0
        A[i, ncols] <- 0
      }

    }
  }

  class(A) <- c("cellular_automata", "matrix")
  attr(A, "wolfram_rule") <- wolframrule
  return(A)
}

#' Plot a cellular automaton
#'
#' @param x A cellular automaton, usually previously defined by `ca()`.
#' @param time_flow String: "down" (default) or "up". Whether time flow is
#'        represented as going from top-to-bottom or bottom-to-top.
#' @param circle Whether to make the plot circular. Default is FALSE.
#' @param animate Whether to return a gganimate object instead of a static
#'        ggplot. Default FALSE.
#' @param title Title of the plot. Use `NULL` to remove.
#' @param ... Not used (included for consistency with the `plot` generic).
#'
#' @return A ggplot of the visual representation of the cellular automaton,
#'        or a gganimate object.
#' @export
#'
#' @examples
#' ca(30) |> plot()
#' ca(30, ncols = 100, steps = 100) |> plot()
#' ca(45, ncols = 100, steps = 100) |> plot()
#' ca(86, ncols = 100, steps = 100) |> plot()
#'
#' # use a random initial state
#' ca(126,
#'    initialstate = sample(c(0, 1), size = 100, replace = TRUE),
#'    steps = 100) |>
#'  plot()
#'
#' @import ggplot2
#' @import gganimate
#' @importFrom rlang .data
#'
plot.cellular_automata <- function(
    x,
    time_flow = "down",
    circle = FALSE,
    title = paste("Rule: ", attr(x, "wolfram_rule")),
    animate = FALSE,
    ...) {

  # pivot the matrix into long data.frame
  df <- cbind(expand.grid(1:nrow(x), 1:ncol(x)), c(x))
  colnames(df) <- c("row", "col", "value")

  # main plot
  p <- df |>
    ggplot2::ggplot() +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_manual(values = c("0" = "white", "1" = "black")) +
    ggplot2::ggtitle(title) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      panel.border = ggplot2::element_rect(
        color = "black", fill = NA, size = 1)
    )

  # vertical orientation
  if (time_flow == "down") p <- p + ggplot2::scale_y_reverse()

  # circular plot
  if (circle) {
    p <- p + ggplot2::coord_polar()
  } else {
    p <- p + ggplot2::coord_fixed()
  }

  # finish the plot or animation
  if (!animate) {

    p <- p + ggplot2::aes(x = .data$col,
                          y = .data$row,
                          fill = factor(.data$value))

  } else {

    p <- p +
      ggplot2::aes(x = .data$col,
                   y = 1,
                   fill = factor(.data$value)) +
      gganimate::transition_states(
        states = row,
        transition_length = 0,
        state_length = 1)

    gganimate::animate(p)

  }

  return(p)

}

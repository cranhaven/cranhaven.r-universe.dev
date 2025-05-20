#' Add bands of transparent colors to denote generations on plots of ancestor-match matrices
#'
#' Pass it the original ggplot, and this will return it, with the bands added
#' @param g the original ggplot
#' @param L the number of rows (or columns) in the ancestor-match matrices
#' @param alpha the transparency to use for these color-bands
#' @param colors the colors in order of self, parent, grandparent, etc.  By default it
#' is just rainbow order starting from red.
#' @param add_impossibles pass TRUE if you want to blot out the cells that are not possible
#' because they conflict with the sex of the individuals.  You set the fill of the
#' impossibles with a `scale_fill_manual()` in the main ggplot call.  i.e.,
#' ```
#' scale_fill_manual(values = c(`FALSE` = NA, Impossible = "white", `TRUE` = "black"))
#' ```
#' @keywords internal
gg_add_generation_bands <- function(
  g,
  L,
  alpha = 0.2,
  colors = c("red", "orange", "yellow", "green", "blue"),
  add_impossibles = FALSE
) {
  # determine number of generations
  Gen <- ceiling(log(L + 1, base = 2))

  # lay down a series of rectangles for each generation level
  for(i in 1:Gen) {
    g <- g +
      annotate(
        "rect",
        xmin = 2^(i-1) - 0.5,
        xmax = 2^i - 1 + 0.5,
        #ymin = 2^(i-1) - 0.5,
        #ymax = L + 0.5,
        ymin = 0.5,
        ymax = 2^Gen - 1 + 0.5,
        fill = colors[i],
        colour = NA,
        alpha = alpha
      ) +
      annotate(
        "rect",
        #xmin = 2^(i-1) - 0.5,
        #xmax = L + 0.5,
        xmin = 0.5,
        xmax = 2^Gen - 1 + 0.5,
        ymin = 2^(i-1) - 0.5,
        ymax = 2^i - 1 + 0.5,
        fill = colors[i],
        colour = NA,
        alpha = alpha
      )
  }

  if(add_impossibles == TRUE) {
    #  make the tibble for it
    imp_tib <- tibble(
      x = rep(1:L, L),
      y = rep(1:L, each = L),
      val = NA_character_
    ) %>%
      mutate(
        val = case_when(
          x == 1 | y == 1 ~ "FALSE",
          (x %% 2) == (y %% 2) ~ "Impossible",
          TRUE ~ "FALSE"
        )
      )

    g <- g +
      geom_tile(
        data = imp_tib,
        mapping = aes(x = x, y = y, fill = val),
        colour = "black"
      )
  }


  g
}


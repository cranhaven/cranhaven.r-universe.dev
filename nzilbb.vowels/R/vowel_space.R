#' Plot vowel space for speaker or speakers.
#'
#' Given vowel data with the first column identifying speakers, the second
#' identifying vowels, the third containing F1 and the fourth containing F2
#' values, plot a vowel space using the speaker's mean values for each vowel.
#' Typically it is best to produce a plot from scratch. The primary purpose of
#' this function is to generate quick plots for interactive use, rather than to
#' produce plots for publication.
#'
#' @param vowel_data data frame of vowel tokens as described above.
#' @param speakers list of speaker identifiers for speaker whose vowel space
#' is to be plotted.
#' @param vowel_colours a named list of vowel = colour entries to indicate
#' which colour to plot each vowel.
#' @param label_size It is often convenient to adjust the size of the labels (in
#'   pts). Default is 4.
#' @param means_only whether to plot means only or all data points. Default:
#'   TRUE.
#' @param ellipses whether to 95% confidence ellipses. Only works if means_only
#'   is FALSE. Default is FALSE.
#' @param point_alpha alpha value for data points if means_only is FALSE.
#' @param facet whether to plot distinct speakers in distinct facets. Default is
#'   TRUE.
#' @return `ggplot` object.
#' @importFrom dplyr mutate filter summarise group_by vars
#' @importFrom ggplot2 ggplot geom_label facet_wrap scale_colour_manual aes labs
#'   geom_point scale_x_reverse scale_y_reverse expansion geom_point
#'   stat_ellipse
#' @importFrom ggrepel geom_label_repel
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @examples
#' # Plot mean vowel space across
#' plot_vowel_space(
#'   onze_vowels,
#'   speakers = NULL,
#'   vowel_colours = NULL,
#'   label_size = 4,
#'   means_only = TRUE,
#'   ellipses = FALSE,
#'   point_alpha = 0.1,
#'   facet = FALSE
#'  )
#' @export
plot_vowel_space <- function(
    vowel_data,
    speakers = NULL,
    vowel_colours = NULL,
    label_size = 4,
    means_only = TRUE,
    ellipses = FALSE,
    point_alpha = 0.1,
    facet = TRUE
  ) {

  base::stopifnot(
    "Column one must be a factor or character vector of speaker ids." =
      base::is.character(vowel_data[[1]]) | base::is.factor(vowel_data[[1]]),
    "Column two must be a factor or character vector of vowel ids." =
      base::is.character(vowel_data[[2]]) | base::is.factor(vowel_data[[2]]),
    "Column three must be a numeric vector of F1 values." =
      base::is.numeric(vowel_data[[3]]),
    "Column four must be a numeric vector of F2 values." =
      base::is.numeric(vowel_data[[4]])
  )

  # Assume speaker is first column, vowel is second, F1 is third, and F2 is
  # fourth.
  speaker_col_name <- base::names(vowel_data)[[1]]
  vowel_col_name <- base::names(vowel_data)[[2]]
  F1_col_name <- base::names(vowel_data)[[3]]
  F2_col_name <- base::names(vowel_data)[[4]]

  # If no speaker list provided, speakers are all speakers in the data.
  if (base::is.null(speakers)) {
    speakers <- vowel_data[[speaker_col_name]] %>% base::unique()
  }

  if (facet == TRUE) {

    # Calculate mean F1 and F2 values for each speaker.
    means <- vowel_data %>%
      filter(
        .data[[speaker_col_name]] %in% speakers
      ) %>%
      group_by(
        .data[[speaker_col_name]],
        .data[[vowel_col_name]]
      ) %>%
      summarise(
        F1 = base::mean(.data[[F1_col_name]]),
        F2 = base::mean(.data[[F2_col_name]])
      )

    # Determine if more than one speaker is being plotted. If so, include facets.
    if (length(speakers) > 1) {
      facet_element <- facet_wrap(vars(.data[[speaker_col_name]]))

    } else {
      facet_element <- NULL
    }

  } else {
    facet_element <- NULL

    # Calculate mean F1 and F2 values for all data.
    means <- vowel_data %>%
      filter(
        .data[[speaker_col_name]] %in% speakers
      ) %>%
      group_by(
        .data[[vowel_col_name]]
      ) %>%
      summarise(
        F1 = base::mean(.data[[F1_col_name]]),
        F2 = base::mean(.data[[F2_col_name]])
      )
  }

  # Add colours if provided.
  if (base::is.null(vowel_colours)) {
    colour_element <- NULL
  } else {
    colour_element <- scale_colour_manual(values = vowel_colours)
  }

  ellipse_element <- NULL

  if (means_only == FALSE) {
    if (ellipses == TRUE) {
      point_element <- geom_point(
          mapping = aes(
            x = .data[[F2_col_name]],
            y = .data[[F1_col_name]]
          ),
          data = vowel_data,
          alpha = point_alpha,
          show.legend = FALSE
        )

        ellipse_element <- stat_ellipse(
          mapping = aes(
            x = .data[[F2_col_name]],
            y = .data[[F1_col_name]]
          ),
          data = vowel_data,
          show.legend = FALSE
        )
    } else {
      point_element <- geom_point(
        mapping = aes(
          x = .data[[F2_col_name]],
          y = .data[[F1_col_name]]
        ),
        size = 0.5,
        data = vowel_data,
        alpha = point_alpha
      )
    }
  } else {
    point_element <- NULL
  }

  means %>%
    ggplot(
      aes(
        x = .data$F2,
        y = .data$F1,
        colour = .data[[vowel_col_name]],
        label = .data[[vowel_col_name]]
      )
    ) +
    point_element +
    ellipse_element +
    geom_label_repel(
      show.legend = FALSE,
      alpha = 0.8,
      min.segment.length = 0,
      size = label_size
    ) +
    geom_point(show.legend = FALSE, size=2) +
    scale_x_reverse(expand = expansion(mult = 0.1)) +
    scale_y_reverse(expand = expansion(mult = 0.1)) +
    colour_element +
    labs(
      x = "F2",
      y = "F1"
    ) +
    facet_element

}

# ADD PLOT VOWEL CHANGE HERE
